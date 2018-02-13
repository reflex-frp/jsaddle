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
    syncPoint
  , syncAfter
  , waitForAnimationFrame
  , nextAnimationFrame
  , enableLogging
#ifndef ghcjs_HOST_OS
  -- * Functions used to implement JSaddle using JSON messaging
  , runJavaScript
  , AsyncCommand(..)
  , Command(..)
  , Result(..)
  , sendCommand
  , sendLazyCommand
  , sendAsyncCommand
  , wrapJSVal
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
       (IORef, mkWeakIORef, newIORef, atomicWriteIORef, readIORef)
import Control.Monad.Trans.Reader

import Language.Javascript.JSaddle.Types
       (Command(..), AsyncCommand(..), Result(..), BatchResults(..), Results(..), JSContextRef(..), JSVal(..),
        Object(..), JSValueReceived(..), JSM(..), Batch(..), JSValueForSend(..), syncPoint, syncAfter, sendCommand)
import Language.Javascript.JSaddle.Exception (JSException(..))
import Control.DeepSeq (force, deepseq)
import GHC.Stats (getGCStatsEnabled, getGCStats, GCStats(..))
import Data.Foldable (forM_)
import GHCJS.Prim.Internal (JSValueRef)
import System.IO.Unsafe
#endif

-- | Enable (or disable) JSaddle logging
enableLogging :: Bool -> JSM ()
#ifdef ghcjs_HOST_OS
enableLogging _ = return ()
#else
enableLogging v = do
    f <- doEnableLogging <$> JSM ask
    liftIO $ f v
#endif

-- | On GHCJS this is 'JavaScript.Web.AnimationFrame.waitForAnimationFrame'.
--   On GHC it will delay the execution of the current batch of asynchronous
--   command when they are sent to JavaScript.  It will not delay the Haskell
--   code execution.  The time returned will be based on the Haskell clock
--   (not the JavaScript clock).
waitForAnimationFrame :: JSM Double
#ifdef ghcjs_HOST_OS
waitForAnimationFrame = GHCJS.waitForAnimationFrame
#else
waitForAnimationFrame = do
    -- We can't get the timestamp from requestAnimationFrame so this will have to do
    start <- startTime <$> JSM ask
    now <- liftIO getCurrentTime
    void $ sendLazyCommand SyncWithAnimationFrame
    return $ realToFrac (diffUTCTime now start)
#endif

-- | Tries to executes the given code in the next animation frame callback.
--   Avoid synchronous opperations where possible.
nextAnimationFrame :: (Double -> JSM a) -> JSM a
nextAnimationFrame f = do
    t <- waitForAnimationFrame
    syncAfter (f t)

#ifndef ghcjs_HOST_OS
sendLazyCommand :: (JSValueForSend -> AsyncCommand) -> JSM JSVal
sendLazyCommand cmd = do
    nextRefTVar <- nextRef <$> JSM ask
    n <- liftIO . atomically $ do
        n <- subtract 1 <$> readTVar nextRefTVar
        writeTVar nextRefTVar $! n
        return n
    s <- doSendAsyncCommand <$> JSM ask
    liftIO $ s (cmd $ JSValueForSend n)
    wrapJSVal (JSValueReceived n)

sendAsyncCommand :: AsyncCommand -> JSM ()
sendAsyncCommand cmd = do
    s <- doSendAsyncCommand <$> JSM ask
    liftIO $ s cmd

-- | A reference to a value that exists in the javascript heap.  If positive, allocated by the Haskell side; if negative, allocated by the javascript side; if zero, always refers to 'undefined'.  In either case, must be freed by the Haskell side using a finalizer.
newtype ValId = ValId { unValId :: Int32 } deriving (Show, Read, Eq, Ord, Enum)

isJsAllocatedValId :: ValId -> Bool
isJsAllocatedValId = (< 0) . unValId

--TODO: We know what order we issued SyncBlock, GetJson, etc. requests in, so we can probably match them up without explicit IDs

newtype GetJsonReqId = GetJsonReqId { unGetJsonReqId :: Int32 } deriving (Show, Read, Eq, Ord, Enum)

newtype SyncCallbackId = SyncCallbackId { unSyncCallbackId :: Int32 } deriving (Show, Read, Eq, Ord, Enum)

data Req
   = Req_Eval Text ValId -- ^ Evaluate the given JavaScript code and save the result as the given ValId
   | Req_FreeVal ValId
   | Req_GetJson ValId GetJsonReqId
   | Req_SyncBlock SyncCallbackId -- ^ Ask JS to begin a synchronous block
   | Req_NewSyncCallback SyncCallbackId ValId

data Rsp
   = Rsp_GetJson GetJsonReqId A.Value

data Env = Env
  { _env_sendReq :: !(Req -> IO ())
  , _env_nextValId :: !(TVar ValId)
  , _env_nextGetJsonReqId :: !(TVar GetJsonReqId)
  , _env_getJsonReqs :: !(TVar (Map GetJsonReqId (MVar A.Value))) -- ^ The GetJson requests that are currently in-flight
  , _env_nextSyncCallbackId :: !(TVar SyncCallbackId)
  , _env_syncCallbacks :: !(TVar (Map SyncCallbackId (Val -> Val -> [Val] -> JSM' Val)))
  }

newtype Val = Val { unVal :: IORef ValId }

type JSM' = ReaderT Env IO

newId :: Enum a => (Env -> TVar a) -> JSM' a
newId f = do
  v <- asks f
  liftIO $ getNextTVar v

getNextTVar :: Enum a => TVar a -> IO a
getNextTVar v = atomically $ do
  a <- readTVar v
  -- Evaluate this strictly so that thunks cannot build up
  writeTVar v $! succ a
  return a

sendReq :: Req -> JSM' ()
sendReq r = do
  s <- asks _env_sendReq
  liftIO $ s r

eval :: Text -> JSM' Val
eval script = do
  val <- newVal
  withValId val $ \valId -> do
    sendReq $ Req_Eval script valId
  return val

newVal :: JSM' Val
newVal = do
  -- Bind this strictly in case it would retain anything else in the finalizer
  !valId <- newId _env_nextValId
  wrapVal valId

wrapVal :: ValId -> JSM' Val
wrapVal valId = do
  valRef <- liftIO $ newIORef valId
  -- Bind this strictly to avoid retaining the whole Env in the finalizer
  !sendReq' <- asks _env_sendReq
  when (valId /= jsUndefinedId) $ do
    void $ liftIO $ mkWeakIORef valRef $ do
      sendReq' $ Req_FreeVal valId
  return $ Val valRef

-- | Run a computation with the given ValId available; the value will not be freed during this computation
--
-- WARNING: Do not allow the ValId to escape the scope, or it may be freed while a reference still exists
withValId :: (MonadIO m, PrimMonad m) => Val -> (ValId -> m a) -> m a
withValId val f = do
  valId <- liftIO $ readIORef $ unVal val
  result <- f valId
  touch val -- Ensure that the value is not freed before the end of the action
  return result

getJson :: Val -> JSM' A.Value
getJson val = do
  resultVar <- withValId val $ \valId -> do
    getJsonReqId <- newId _env_nextGetJsonReqId
    getJsonReqs <- asks _env_getJsonReqs
    resultVar <- liftIO $ newEmptyMVar
    liftIO $ atomically $ modifyTVar' getJsonReqs $ M.insert getJsonReqId resultVar
    sendReq $ Req_GetJson valId getJsonReqId
    return resultVar
  liftIO $ takeMVar resultVar

jsUndefinedId :: ValId
jsUndefinedId = ValId 0

{-# NOINLINE jsUndefined #-}
jsUndefined :: Val
jsUndefined = unsafePerformIO $ fmap Val $ newIORef jsUndefinedId

-- | Run the given action synchronously in the JS engine, without yielding control
sync_ :: JSM' () -> JSM' ()
sync_ syncBlock = do
  syncBlockId <- newId _env_nextSyncCallbackId
  syncCallbacks <- asks _env_syncCallbacks
  env <- ask
  liftIO $ atomically $ modifyTVar' syncCallbacks $ M.insert syncBlockId $ \_ _ _ -> do
    syncBlock
    return jsUndefined

sync :: JSM' a -> JSM' a
sync syncBlock = do
  resultVar <- liftIO newEmptyMVar
  sync_ $ liftIO . putMVar resultVar =<< syncBlock
  liftIO $ takeMVar resultVar

runJS
  :: (Req -> IO ()) -- ^ Send a request to the JS engine; we assume that requests are performed in the order they are sent; requests received while in a synchronous block must not be processed until the synchronous block ends (i.e. until the JS side receives the final value yielded back from the synchronous block)
  -> IO ( Rsp -> IO () -- Responses must be able to continue coming in as a sync block runs, or else the caller must be careful to ensure that sync blocks are only run after all outstanding responses have been processed
        , SyncCallbackId -> ValId -> ValId -> [ValId] -> IO (Either ValId Req) -- The input valIds here must always be allocated on the JS side
        , IO (Either ValId Req)
        , Env
        )
runJS sendReqAsync = do
  nextValId <- newTVarIO $ ValId 1
  nextGetJsonReqId <- newTVarIO $ GetJsonReqId 1
  getJsonReqs <- newTVarIO M.empty
  nextSyncCallbackId <- newTVarIO $ SyncCallbackId 1
  syncCallbacks <- newTVarIO M.empty
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
      --TODO: Possibly return a batch of requests rather than just one
      runSyncCallback syncCallbackId fun this args = do
        mSyncCallback <- fmap (M.lookup syncCallbackId) $ atomically $ readTVar syncCallbacks
        case mSyncCallback of
          Just syncCallback -> do
            --TODO: Only use use the yield var for requests that someone might block on; e.g., don't do it for FreeVal; however, FreeVal must still wait until the synchronous block has finished, because otherwise it might free the return value of the synchronous block; however, we also don't want to prevent all cleanup in the event of a long sync block
            myDepth <- enterSyncFrame
            _ <- forkIO $ do
              result <- flip runReaderT env $ join $ syncCallback
                <$> wrapVal fun
                <*> wrapVal this
                <*> traverse wrapVal args --TODO: Handle exceptions that occur within the syncCallback
              exitSyncFrame myDepth result
            yield
      continueSyncCallback = yield
      env = Env
        { _env_sendReq = \req -> withMVar syncState $ \case
            -- When no synchronous operation is in progress, send our request asynchronously
            (0, _) -> sendReqAsync req
            -- When a synchronous operation is in progress, make it yield our request
            _ -> putMVar yieldVar $ Right req
        , _env_nextValId = nextValId
        , _env_nextGetJsonReqId = nextGetJsonReqId
        , _env_getJsonReqs = getJsonReqs
        , _env_nextSyncCallbackId = nextSyncCallbackId
        , _env_syncCallbacks = syncCallbacks
        }
  return (processRsp, runSyncCallback, continueSyncCallback, env)

runJavaScript :: (Batch -> IO ()) -> JSM () -> IO (Results -> IO (), Results -> IO Batch, IO ())
runJavaScript sendBatch entryPoint = do
  undefined
{-
    contextId' <- randomIO -- :: Int64
    startTime' <- getCurrentTime
    recvMVar :: MVar (Int, BatchResults) <- newEmptyMVar
    commandChan :: TChan (Either AsyncCommand (Command, MVar Result)) <- newTChanIO
    callbacks :: TVar (M.Map JSValueRef (JSVal -> JSVal -> [JSVal] -> JSM ())) <- newTVarIO M.empty
    nextRef' :: TVar JSValueRef <- newTVarIO 0
    loggingEnabled <- newIORef False
    let ctx = JSContextRef {
            contextId = contextId'
          , startTime = startTime'
          , doSendCommand = \cmd -> cmd `deepseq` do
                result <- newEmptyMVar
                atomically $ writeTChan commandChan (Right (cmd, result))
                unsafeInterleaveIO $
                    takeMVar result >>= \case
                        (ThrowJSValue v) -> do
                            jsval <- wrapJSVal' ctx v
                            throwIO $ JSException jsval
                        r -> return r
          , doSendAsyncCommand = \cmd -> cmd `deepseq` atomically (writeTChan commandChan $ Left cmd)
          , addCallback = \(Object (JSVal ioref)) cb -> do
                val <- readIORef ioref
                atomically $ modifyTVar' callbacks $ M.insert val cb
--          , removeCallback = \var -> do
--                atomically $ modifyTVar' callbacks $ M.delete val
          , nextRef = nextRef'
          , doEnableLogging = atomicWriteIORef loggingEnabled
          }
        processResults :: Bool -> Results -> IO ()
        processResults syncCallbacks = \case
            (ProtocolError err) -> error $ "Protocol error : " <> T.unpack err
            (Callback n br (JSValueReceived fNumber) f this a) -> do
                putMVar recvMVar (n, br)
                f' <- runReaderT (unJSM $ wrapJSVal f) ctx
                this' <- runReaderT  (unJSM $ wrapJSVal this) ctx
                args <- runReaderT (unJSM $ mapM wrapJSVal a) ctx
                logInfo (("Call " <> show fNumber <> " ") <>)
                (M.lookup fNumber <$> liftIO (readTVarIO callbacks)) >>= \case
                    Nothing -> liftIO $ putStrLn "Callback called after it was freed"
                    Just cb -> void . forkIO $ do
                        runReaderT (unJSM $ cb f' this' args) ctx
                        when syncCallbacks $
                            doSendAsyncCommand ctx EndSyncBlock
            Duplicate nBatch nExpected -> do
                putStrLn $ "Error : Unexpected Duplicate. syncCallbacks=" <> show syncCallbacks <>
                    " nBatch=" <> show nBatch <> " nExpected=" <> show nExpected
                void $ doSendCommand ctx Sync
            BatchResults n br -> putMVar recvMVar (n, br)
        asyncResults :: Results -> IO ()
        asyncResults results =
            void . forkIO $ processResults False results
        syncResults :: Results -> IO Batch
        syncResults results = do
            void . forkIO $ processResults True results
        logInfo s =
            readIORef loggingEnabled >>= \case
                True -> do
                    currentBytesUsedStr <- getGCStatsEnabled >>= \case
                        True  -> show . currentBytesUsed <$> getGCStats
                        False -> return "??"
                    cbCount <- M.size <$> readTVarIO callbacks
                    putStrLn . s $ "M " <> currentBytesUsedStr <> " CB " <> show cbCount <> " "
                False -> return ()
    _ <- forkIO . numberForeverFromM_ 1 $ \nBatch ->
        readBatch nBatch commandChan >>= \case
            (batch@(Batch cmds _ _), resultMVars) -> do
                logInfo (\x -> "Sync " <> x <> show (length cmds, last cmds))
                _ <- tryTakeMVar lastAsyncBatch
                putMVar lastAsyncBatch batch
                sendBatch batch
                takeResult recvMVar nBatch >>= \case
                    (n, _) | n /= nBatch -> error $ "Unexpected jsaddle results (expected batch " <> show nBatch <> ", got batch " <> show n <> ")"
                    (_, Success results)
                           | length results /= length resultMVars -> error "Unexpected number of jsaddle results"
                           | otherwise -> do
                        zipWithM_ putMVar resultMVars results
                    (_, Failure results exception) -> do
                        -- The exception will only be rethrown in Haskell if/when one of the
                        -- missing results (if any) is evaluated.
                        putStrLn "A JavaScript exception was thrown! (may not reach Haskell code)"
                        zipWithM_ putMVar resultMVars $ results <> repeat (ThrowJSValue exception)
    return (asyncResults, syncResults, runReaderT (unJSM entryPoint) ctx)
  where
    numberForeverFromM_ :: (Monad m, Enum n) => n -> (n -> m a) -> m ()
    numberForeverFromM_ !n f = do
      _ <- f n
      numberForeverFromM_ (succ n) f
    takeResult recvMVar nBatch =
        takeMVar recvMVar >>= \case
            (n, _) | n < nBatch -> takeResult recvMVar nBatch
            r -> return r
    readBatch :: Int -> TChan (Either AsyncCommand (Command, MVar Result)) -> IO (Batch, [MVar Result])
    readBatch nBatch chan = do
        first <- atomically $ readTChan chan -- We want at least one command to send
        loop first ([], [])
      where
        loop :: Either AsyncCommand (Command, MVar Result) -> ([Either AsyncCommand Command], [MVar Result]) -> IO (Batch, [MVar Result])
        loop (Left asyncCmd@(SyncWithAnimationFrame _)) (cmds, resultMVars) =
            atomically (readTChan chan) >>= \cmd -> loopAnimation cmd (Left asyncCmd:cmds, resultMVars)
        loop (Right (syncCmd, resultMVar)) (cmds', resultMVars') = do
            let cmds = Right syncCmd:cmds'
                resultMVars = resultMVar:resultMVars'
            atomically (tryReadTChan chan) >>= \case
                Nothing -> return (Batch (reverse cmds) False nBatch, reverse resultMVars)
                Just cmd -> loop cmd (cmds, resultMVars)
        loop (Left asyncCmd) (cmds', resultMVars) = do
            let cmds = Left asyncCmd:cmds'
            atomically (tryReadTChan chan) >>= \case
                Nothing -> return (Batch (reverse cmds) False nBatch, reverse resultMVars)
                Just cmd -> loop cmd (cmds, resultMVars)
        -- When we have seen a SyncWithAnimationFrame command only a synchronous command should end the batch
        loopAnimation :: Either AsyncCommand (Command, MVar Result) -> ([Either AsyncCommand Command], [MVar Result]) -> IO (Batch, [MVar Result])
        loopAnimation (Right (Sync, resultMVar)) (cmds, resultMVars) =
            return (Batch (reverse (Right Sync:cmds)) True nBatch, reverse (resultMVar:resultMVars))
        loopAnimation (Right (syncCmd, resultMVar)) (cmds, resultMVars) =
            atomically (readTChan chan) >>= \cmd -> loopAnimation cmd (Right syncCmd:cmds, resultMVar:resultMVars)
        loopAnimation (Left asyncCmd) (cmds, resultMVars) =
            atomically (readTChan chan) >>= \cmd -> loopAnimation cmd (Left asyncCmd:cmds, resultMVars)

addThreadFinalizer :: ThreadId -> IO () -> IO ()
addThreadFinalizer t@(ThreadId t#) (IO finalizer) =
    IO $ \s -> case mkWeak# t# t finalizer s of { (# s1, _ #) -> (# s1, () #) }

-}

wrapJSVal :: JSValueReceived -> JSM JSVal
wrapJSVal v = do
    ctx <- JSM ask
    liftIO $ wrapJSVal' ctx v

wrapJSVal' :: JSContextRef -> JSValueReceived -> IO JSVal
wrapJSVal' ctx (JSValueReceived n) = do
    ref <- liftIO $ newIORef n
    when (n >= 5 || n < 0) $
        void . mkWeakIORef ref $ do
            doSendAsyncCommand ctx $ FreeRef n
    return (JSVal ref)
#endif

