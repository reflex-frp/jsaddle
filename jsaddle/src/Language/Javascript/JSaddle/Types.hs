{-# LANGUAGE CPP                        #-}
#ifdef ghcjs_HOST_OS
{-# OPTIONS_GHC -Wno-dodgy-exports      #-}
#else
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ImplicitParams             #-}
#endif
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# OPTIONS_GHC -Wno-deprecated      #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Types
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Types (

  -- * The JSM Monad
    JSM(..)
  , MonadJSM(..)
  , liftJSM
  , askJSM

  -- * pure GHCJS functions
  , GHCJSPure(..)
  , ghcjsPure
  , ghcjsPureMap
  , ghcjsPureId

  -- * JavaScript Value Types
  , JSVal(..)
  , IsJSVal(..)
  , jsval
  , SomeJSArray(..)
  , JSArray
  , MutableJSArray
  , STJSArray
  , Object(..)
  , JSString(..)
  , Nullable(..)
  , JSCallAsFunction

  -- * Debugging
  , JSadddleHasCallStack

#ifndef ghcjs_HOST_OS

  -- * JavaScript Context Commands
  , MutabilityType(..)
  , Mutable
  , Immutable
  , IsItMutable(..)
  , Mutability

  , JSContextRef (..)
  , Req (..)
  , Val (..)
  , ValId (..)
  , Ref (..)
  , RefId (..)
  , LazyVal (..)
  , lazyValFromStrict
  , getLazyVal
  , Rsp (..)
  , SyncCallbackId (..)
  , CallbackId
  , GetJsonReqId (..)
  , PrimVal (..)
  , runJSM
#endif
) where

import Control.Monad.IO.Class (MonadIO(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array.Internal (SomeJSArray(..), JSArray, MutableJSArray, STJSArray)
import GHCJS.Nullable (Nullable(..))
#else
import GHCJS.Prim.Internal
import Data.JSString.Internal.Type (JSString(..))
import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadThrow, MonadCatch(..), MonadMask(..))
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Trans.Error (Error(..), ErrorT(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import Control.Monad.Trans.State.Lazy as Lazy (StateT(..))
import Control.Monad.Trans.State.Strict as Strict (StateT(..))
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Fix (MonadFix)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.MVar (MVar)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Typeable (Typeable)
import Data.Coerce (coerce, Coercible)
import Data.Aeson
       (defaultOptions, genericToEncoding, ToJSON(..), FromJSON(..), Value)
import GHC.Generics (Generic)
import Data.Int
import qualified Data.Aeson as A
import Data.Map (Map)
import Data.IORef (IORef)
import Data.Scientific
import Data.Foldable
import System.IO.Unsafe
import Data.IORef
import Control.Monad.Ref (MonadRef (newRef, readRef, writeRef), MonadAtomicRef(..))
import qualified Control.Monad.Ref as MonadRef
#endif

#if MIN_VERSION_base(4,9,0) && defined(CHECK_UNCHECKED)
import GHC.Stack (HasCallStack)
#else
import GHC.Exts (Constraint)
#endif

-- | The 'JSM' monad keeps track of the JavaScript execution context.
--
--   When using GHCJS it is `IO`.
--
--   Given a 'JSM' function and a 'JSContextRef' you can run the
--   function like this...
--
-- > runJSM jsmFunction javaScriptContext
#ifdef ghcjs_HOST_OS
type JSM = IO
#else
#endif

-- | Type we can give to functions that are pure when using ghcjs, but
--   live in JSM when using jsaddle.
--
--   Some functions that can be pure in GHCJS cannot be implemented in
--   a pure way in JSaddle (because we need to know the JSContextRef).
--   Instead we implement versions of these functions in that return
--   `GHCJSPure a` instead of `a`.  To call them in a way that will
--   work when compiling with GHCJS use `ghcjsPure`.
#ifdef ghcjs_HOST_OS
type GHCJSPure a = a
#else
newtype GHCJSPure a = GHCJSPure (JSM a)
#endif

-- | Used when you want to call a functions that is pure in GHCJS, but
--   lives in the JSM in jsaddle.
ghcjsPure :: GHCJSPure a -> JSM a
#ifdef ghcjs_HOST_OS
ghcjsPure = pure
#else
ghcjsPure (GHCJSPure x) = x
#endif
{-# INLINE ghcjsPure #-}

ghcjsPureMap :: (a -> b) -> GHCJSPure a -> GHCJSPure b
#ifdef ghcjs_HOST_OS
ghcjsPureMap = id
#else
ghcjsPureMap f (GHCJSPure x) = GHCJSPure (f <$> x)
#endif
{-# INLINE ghcjsPureMap #-}

ghcjsPureId :: a -> GHCJSPure a
#ifdef ghcjs_HOST_OS
ghcjsPureId = id
#else
ghcjsPureId = GHCJSPure . return
#endif
{-# INLINE ghcjsPureId #-}

-- | The 'MonadJSM' is to 'JSM' what 'MonadIO' is to 'IO'.
--   When using GHCJS it is 'MonadIO'.
#ifdef ghcjs_HOST_OS
type MonadJSM = MonadIO
#else
class (Applicative m, MonadIO m) => MonadJSM m where
    liftJSM' :: JSM a -> m a

    default liftJSM' :: (MonadJSM m', MonadTrans t, m ~ t m') => JSM a -> m a
    liftJSM' = lift . (liftJSM' :: MonadJSM m' => JSM a -> m' a)
    {-# INLINE liftJSM' #-}

instance MonadJSM JSM where
    liftJSM' = id
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ContT r m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Error e, MonadJSM m) => MonadJSM (ErrorT e m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ExceptT e m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (IdentityT m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ListT m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (MaybeT m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ReaderT r m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Lazy.RWST r w s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Strict.RWST r w s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (Lazy.StateT s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (Strict.StateT s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Lazy.WriterT w m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Strict.WriterT w m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}
#endif

-- | The 'liftJSM' is to 'JSM' what 'liftIO' is to 'IO'.
--   When using GHCJS it is 'liftIO'.
liftJSM :: MonadJSM m => JSM a -> m a
#ifdef ghcjs_HOST_OS
liftJSM = liftIO
#else
liftJSM = liftJSM'
#endif
{-# INLINE liftJSM #-}

-- | Type used for Haskell functions called from JavaScript.
type JSCallAsFunction = JSVal      -- ^ Function object
                     -> JSVal      -- ^ this
                     -> [JSVal]    -- ^ Function arguments
                     -> JSM ()     -- ^ Only () (aka 'JSUndefined') can be returned because
                                   --   the function may need to be executed in a
                                   --   different thread.  If you need to get a
                                   --   value out pass in a continuation function
                                   --   as an argument and invoke it from haskell.

#ifndef ghcjs_HOST_OS

class IsJSVal a where
  jsval_ :: a -> GHCJSPure JSVal

  default jsval_ :: Coercible a JSVal => a -> GHCJSPure JSVal
  jsval_ = GHCJSPure . return . coerce
  {-# INLINE jsval_ #-}

jsval :: IsJSVal a => a -> GHCJSPure JSVal
jsval = jsval_
{-# INLINE jsval #-}

data MutabilityType s = Mutable_ s
                      | Immutable_ s
                      | STMutable s

type Mutable   = Mutable_ ()
type Immutable = Immutable_ ()

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = IsImmutable
  Mutability Mutable       = IsMutable
  Mutability (STMutable s) = IsMutable

newtype SomeJSArray (m :: MutabilityType s) = SomeJSArray JSVal
  deriving (Typeable)
instance IsJSVal (SomeJSArray m)

-- | See 'JavaScript.Array.Internal.JSArray'
type JSArray        = SomeJSArray Immutable
-- | See 'JavaScript.Array.Internal.MutableJSArray'
type MutableJSArray = SomeJSArray Mutable

-- | See 'JavaScript.Array.Internal.STJSArray'
type STJSArray s    = SomeJSArray (STMutable s)

-- | See 'JavaScript.Object.Internal.Object'
newtype Object = Object JSVal

-- | See 'GHCJS.Nullable.Nullable'
newtype Nullable a = Nullable a
#endif

-- | Like HasCallStack, but only when jsaddle cabal flag check-unchecked is set
#if MIN_VERSION_base(4,9,0) && defined(CHECK_UNCHECKED)
type JSadddleHasCallStack = HasCallStack
#else
type JSadddleHasCallStack = (() :: Constraint)
#endif


--TODO: We know what order we issued SyncBlock, GetJson, etc. requests in, so we can probably match them up without explicit IDs

newtype GetJsonReqId = GetJsonReqId { unGetJsonReqId :: Int32 } deriving (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

type CallbackId = SyncCallbackId --TODO: Either rename or distinguish these

newtype SyncCallbackId = SyncCallbackId { unSyncCallbackId :: Int32 } deriving (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

aesonOptions :: String -> A.Options
aesonOptions typeName = A.defaultOptions
  { A.constructorTagModifier = drop (length typeName + 1)
  , A.fieldLabelModifier = drop (length typeName + 2)
  }

data Req
   = Req_Eval Text RefId -- ^ Evaluate the given JavaScript code and save the result as the given RefId
   | Req_FreeRef RefId
   | Req_NewJson A.Value RefId
   | Req_GetJson RefId GetJsonReqId
   | Req_SyncBlock SyncCallbackId -- ^ Ask JS to begin a synchronous block
   | Req_NewSyncCallback SyncCallbackId RefId -- ^ Create a new sync callback; note that we don't inform the JS side when we dispose of a callback - it's an error to call a disposed callback, so we just detect and throw that error on the Haskell side
   | Req_NewAsyncCallback SyncCallbackId RefId -- ^ Create a new async callback; note that we don't inform the JS side when we dispose of a callback - it's an error to call a disposed callback, so we just detect and throw that error on the Haskell side
   deriving (Show, Read, Eq, Generic)

instance ToJSON Req where
  toEncoding = A.genericToEncoding $ aesonOptions "Req"

instance FromJSON Req where
  parseJSON = A.genericParseJSON $ aesonOptions "Req"

data Rsp
   = Rsp_GetJson GetJsonReqId A.Value
   | Rsp_Result RefId (PrimVal ())
   | Rsp_CallAsync CallbackId (PrimVal RefId) [PrimVal RefId]
   deriving (Show, Read, Eq, Generic)

instance ToJSON Rsp where
  toEncoding = A.genericToEncoding $ aesonOptions "Rsp"

instance FromJSON Rsp where
  parseJSON = A.genericParseJSON $ aesonOptions "Rsp"


data JSContextRef = JSContextRef
  { _jsContextRef_sendReq :: !(Req -> IO ())
  , _jsContextRef_nextRefId :: !(TVar RefId)
  , _jsContextRef_nextGetJsonReqId :: !(TVar GetJsonReqId)
  , _jsContextRef_getJsonReqs :: !(TVar (Map GetJsonReqId (MVar A.Value))) -- ^ The GetJson requests that are currently in-flight
  , _jsContextRef_nextSyncCallbackId :: !(TVar SyncCallbackId)
  , _jsContextRef_syncCallbacks :: !(TVar (Map SyncCallbackId (Val -> [Val] -> JSM Val)))
  , _jsContextRef_pendingResults :: !(TVar (Map RefId (MVar (PrimVal ()))))
  }

newtype JSM a = JSM { unJSM :: ReaderT JSContextRef IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow)

instance MonadRef JSM where
    type Ref JSM = MonadRef.Ref IO
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

instance MonadAtomicRef JSM where
    atomicModifyRef r = liftIO . atomicModifyRef r

--TODO: Figure out what syncAfter was doing in MonadCatch and MonadMask, and do that
instance MonadCatch JSM where
    t `catch` c = JSM (unJSM t `catch` \e -> unJSM (c e))

instance MonadMask JSM where
  mask a = JSM $ mask $ \unmask -> unJSM (a $ q unmask)
    where q :: (ReaderT JSContextRef IO a -> ReaderT JSContextRef IO a) -> JSM a -> JSM a
          q unmask (JSM b) = JSM $ unmask b
  uninterruptibleMask a =
    JSM $ uninterruptibleMask $ \unmask -> unJSM (a $ q unmask)
      where q :: (ReaderT JSContextRef IO a -> ReaderT JSContextRef IO a) -> JSM a -> JSM a
            q unmask (JSM b) = JSM $ unmask b

runJSM :: MonadIO m => JSM a -> JSContextRef -> m a
runJSM a ctx = liftIO $ runReaderT (unJSM a) ctx

askJSM :: MonadJSM m => m JSContextRef
askJSM = liftJSM $ JSM ask
