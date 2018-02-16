{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Run
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Run (
  -- * Running JSM
#ifndef ghcjs_HOST_OS
  -- * Functions used to implement JSaddle using JSON messaging
    runJS
  , newJson
  , eval
  , lazyValResult
  , freeSyncCallback
  , callbackToSyncFunction
  , callbackToAsyncFunction
  , syncPoint
#endif
) where

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types (JSM, syncPoint, syncAfter)
import qualified JavaScript.Web.AnimationFrame as GHCJS
       (waitForAnimationFrame)
#else
import Control.Exception (throwIO, evaluate, bracket, assert)
import Control.Monad (void, when, zipWithM_, join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask, runReaderT)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM.TChan
       (tryReadTChan, TChan, readTChan, writeTChan, newTChanIO)
import Control.Concurrent.STM.TVar
       (TVar, writeTVar, readTVar, readTVarIO, modifyTVar', newTVarIO)
import Control.Concurrent.MVar
       (tryTakeMVar, MVar, putMVar, takeMVar, newMVar, newEmptyMVar, readMVar, modifyMVar, modifyMVar_, withMVar, swapMVar)
import Control.Monad.Primitive

import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak (addFinalizer)
import System.Random

import GHC.Base (IO(..), mkWeak#)
import GHC.Conc (ThreadId(..))
import qualified Data.Aeson as A
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S (empty, member, insert, delete)
import Data.Time.Clock (getCurrentTime,diffUTCTime)
import Data.Int
import Data.IORef
       (IORef, mkWeakIORef, newIORef, atomicWriteIORef, readIORef, writeIORef)
import Control.Monad.Trans.Reader

import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Exception (JSException(..))
import Control.DeepSeq (force, deepseq)
import GHC.Stats (getGCStatsEnabled, getGCStats, GCStats(..))
import Data.Foldable (forM_)
import GHCJS.Prim.Internal (JSValueRef)
import System.IO.Unsafe
import Language.Javascript.JSaddle.Monad (syncPoint)
#endif

newId :: Enum a => (JSContextRef -> TVar a) -> JSM a
newId f = do
  v <- JSM $ asks f
  liftIO $ getNextTVar v

getNextTVar :: Enum a => TVar a -> IO a
getNextTVar v = atomically $ do
  a <- readTVar v
  -- Evaluate this strictly so that thunks cannot build up
  writeTVar v $! succ a
  return a

sendReq :: Req -> JSM ()
sendReq r = do
  s <- JSM $ asks _jsContextRef_sendReq
  liftIO $ s r

eval :: Text -> JSM JSVal
eval script = do
  val <- newRef
  withRefId val $ \valId -> do
    sendReq $ Req_Eval script valId
  JSVal <$> lazyValResult val

callbackToSyncFunction :: CallbackId -> JSM JSVal
callbackToSyncFunction callbackId = do
  val <- newRef
  withRefId val $ \valId -> do
    sendReq $ Req_NewSyncCallback callbackId valId
  JSVal <$> lazyValResult val

callbackToAsyncFunction :: CallbackId -> JSM JSVal
callbackToAsyncFunction callbackId = do
  val <- newRef
  withRefId val $ \valId -> do
    sendReq $ Req_NewAsyncCallback callbackId valId
  JSVal <$> lazyValResult val

--TODO: This *MUST* be run before sendReq; we should change the type to enforce this
lazyValResult :: Ref -> JSM LazyVal
lazyValResult ref = do
  pendingResults <- JSM $ asks _jsContextRef_pendingResults
  liftIO $ do
    refId <- readIORef $ unRef ref
    resultVar <- newEmptyMVar
    atomically $ modifyTVar' pendingResults $ M.insertWith (error "getLazyVal: already waiting for this ref") refId resultVar
    refRef <- newIORef $ Just ref
    resultVal <- unsafeInterleaveIO $ do
      result <- takeMVar resultVar
      writeIORef refRef Nothing
      return result
    let !val = ref <$ resultVal
    return $ LazyVal
      { _lazyVal_ref = refRef
      , _lazyVal_val = val
      }

newRef :: JSM Ref
newRef = do
  -- Bind this strictly in case it would retain anything else in the finalizer
  !valId <- newId _jsContextRef_nextRefId
  wrapRef valId

wrapRef :: RefId -> JSM Ref
wrapRef valId = do
  valRef <- liftIO $ newIORef valId
  -- Bind this strictly to avoid retaining the whole JSContextRef in the finalizer
  !sendReq' <- JSM $ asks _jsContextRef_sendReq
  void $ liftIO $ mkWeakIORef valRef $ do
    sendReq' $ Req_FreeRef valId
  return $ Ref valRef

freeSyncCallback :: SyncCallbackId -> JSM ()
freeSyncCallback cbid = do
  syncCallbacks <- JSM $ asks _jsContextRef_syncCallbacks
  liftIO $ atomically $ modifyTVar' syncCallbacks $ M.delete cbid

-- | Run a computation with the given RefId available; the value will not be freed during this computation
--
-- WARNING: Do not allow the RefId to escape the scope, or it may be freed while a reference still exists
withRefId :: MonadIO m => Ref -> (RefId -> m a) -> m a
withRefId val f = do
  valId <- liftIO $ readIORef $ unRef val
  result <- f valId
  liftIO $ touch val -- Ensure that the value is not freed before the end of the action
  return result

wrapVal :: ValId -> JSM Val
wrapVal = traverse wrapRef

withValId :: MonadIO m => Val -> (ValId -> m a) -> m a
withValId val f = case val of
  PrimVal_Undefined -> f PrimVal_Undefined
  PrimVal_Null -> f PrimVal_Null
  PrimVal_Bool b -> f $ PrimVal_Bool b
  PrimVal_Number n -> f $ PrimVal_Number n
  PrimVal_String s -> f $ PrimVal_String s
  PrimVal_Ref r -> withRefId r $ f . PrimVal_Ref

getJson :: Ref -> JSM A.Value
getJson val = do
  resultVar <- withRefId val $ \valId -> do
    getJsonReqId <- newId _jsContextRef_nextGetJsonReqId
    getJsonReqs <- JSM $ asks _jsContextRef_getJsonReqs
    resultVar <- liftIO $ newEmptyMVar
    liftIO $ atomically $ modifyTVar' getJsonReqs $ M.insert getJsonReqId resultVar
    sendReq $ Req_GetJson valId getJsonReqId
    return resultVar
  liftIO $ takeMVar resultVar

newJson :: A.Value -> JSM Ref
newJson v = do
  val <- newRef
  withRefId val $ \valId -> do
    sendReq $ Req_NewJson v valId
  return val

-- | Run the given action synchronously in the JS engine, without yielding control
sync_ :: JSM () -> JSM ()
sync_ syncBlock = do
  syncBlockId <- newId _jsContextRef_nextSyncCallbackId
  syncCallbacks <- JSM $ asks _jsContextRef_syncCallbacks
  env <- JSM ask
  liftIO $ atomically $ modifyTVar' syncCallbacks $ M.insert syncBlockId $ \_ _ -> do
    syncBlock
    return PrimVal_Undefined

sync :: JSM a -> JSM a
sync syncBlock = do
  resultVar <- liftIO newEmptyMVar
  sync_ $ liftIO . putMVar resultVar =<< syncBlock
  liftIO $ takeMVar resultVar

runJS
  :: (Req -> IO ()) -- ^ Send a request to the JS engine; we assume that requests are performed in the order they are sent; requests received while in a synchronous block must not be processed until the synchronous block ends (i.e. until the JS side receives the final value yielded back from the synchronous block)
  -> IO ( Rsp -> IO () -- Responses must be able to continue coming in as a sync block runs, or else the caller must be careful to ensure that sync blocks are only run after all outstanding responses have been processed
        , SyncCallbackId -> ValId -> [ValId] -> IO (Either ValId Req) -- The input valIds here must always be allocated on the JS side
        , IO (Either ValId Req)
        , JSContextRef
        )
runJS sendReqAsync = do
  nextRefId <- newTVarIO $ RefId 1
  nextGetJsonReqId <- newTVarIO $ GetJsonReqId 1
  getJsonReqs <- newTVarIO M.empty
  nextSyncCallbackId <- newTVarIO $ SyncCallbackId 1
  syncCallbacks <- newTVarIO M.empty
  pendingResults <- newTVarIO M.empty
  yieldVar <- newEmptyMVar
  -- Each value in the map corresponds to a value ready to be returned from the sync frame corresponding to its key
  -- INVARIANT: \(depth, readyFrames) -> all (< depth) $ M.keys readyFrames
  syncState <- newMVar (0, M.empty)
  let enterSyncFrame = modifyMVar syncState $ \(oldDepth, readyFrames) -> do
        let !newDepth = succ oldDepth
        return ((newDepth, readyFrames), newDepth)
      exitSyncFrame myDepth myRetVal = modifyMVar_ syncState $ \(oldDepth, readyFrames) -> case oldDepth `compare` myDepth of
        LT -> error "should be impossible: trying to return from deeper sync frame than the current depth"
        -- We're the top frame, so yield our value to the caller
        EQ -> do
          let yieldAllReady :: Int -> Map Int Val -> Val -> IO (Int, Map Int Val)
              yieldAllReady depth readyFrames retVal = do
                -- Even though the valId is escaping, this is safe because we know that our yielded value will go out before any potential FreeVal request could go out
                withValId retVal $ \retValId -> do
                  putMVar yieldVar $ Left retValId

                let !nextDepth = pred depth
                case M.maxViewWithKey readyFrames of
                  -- The parent frame is also ready to yield
                  Just ((k, nextRetVal), nextReadyFrames)
                    | k == nextDepth
                      -> yieldAllReady nextDepth nextReadyFrames nextRetVal
                  _ -> return (nextDepth, readyFrames)
          yieldAllReady oldDepth readyFrames myRetVal
        -- We're not the top frame, so just store our value so it can be yielded later
        GT -> do
          let !newReadyFrames = M.insertWith (error "should be impossible: trying to return from a sync frame that has already returned") myDepth myRetVal readyFrames
          return (oldDepth, newReadyFrames)
      yield = takeMVar yieldVar
      processRsp = \case
        Rsp_GetJson getJsonReqId val -> atomically $ do
          reqs <- readTVar getJsonReqs
          writeTVar getJsonReqs $! M.delete getJsonReqId reqs
        Rsp_Result refId primVal -> do
          mResultVar <- atomically $ do
            resultVars <- readTVar pendingResults
            let mResultVar = M.lookup refId resultVars
            when (isJust mResultVar) $ do
              writeTVar pendingResults $! M.delete refId resultVars
            return mResultVar
          forM_ mResultVar $ \resultVar -> do
            putMVar resultVar primVal
      --TODO: Possibly return a batch of requests rather than just one
      runSyncCallback syncCallbackId this args = do
        mSyncCallback <- fmap (M.lookup syncCallbackId) $ atomically $ readTVar syncCallbacks
        case mSyncCallback of
          Just (syncCallback :: Val -> [Val] -> JSM Val) -> do
            --TODO: Only use use the yield var for requests that someone might block on; e.g., don't do it for FreeVal; however, FreeVal must still wait until the synchronous block has finished, because otherwise it might free the return value of the synchronous block; however, we also don't want to prevent all cleanup in the event of a long sync block
            myDepth <- enterSyncFrame
            _ <- forkIO $ do
              result <- flip runJSM env $ join $ syncCallback
                <$> wrapVal this
                <*> traverse wrapVal args --TODO: Handle exceptions that occur within the syncCallback
              exitSyncFrame myDepth result
            yield
          Nothing -> error $ "sync callback " <> show syncCallbackId <> " called, but does not exist"
      continueSyncCallback = yield
      env = JSContextRef
        { _jsContextRef_sendReq = \req -> withMVar syncState $ \case
            -- When no synchronous operation is in progress, send our request asynchronously
            (0, _) -> sendReqAsync req
            -- When a synchronous operation is in progress, make it yield our request
            _ -> putMVar yieldVar $ Right req
        , _jsContextRef_nextRefId = nextRefId
        , _jsContextRef_nextGetJsonReqId = nextGetJsonReqId
        , _jsContextRef_getJsonReqs = getJsonReqs
        , _jsContextRef_nextSyncCallbackId = nextSyncCallbackId
        , _jsContextRef_syncCallbacks = syncCallbacks
        , _jsContextRef_pendingResults = pendingResults
        }
  return (processRsp, runSyncCallback, continueSyncCallback, env)
