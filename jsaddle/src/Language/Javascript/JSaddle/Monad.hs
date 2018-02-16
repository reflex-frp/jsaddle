{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
#ifdef ghcjs_HOST_OS
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-dodgy-imports #-}
#endif
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Monad
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | JSM monad keeps track of the JavaScript context
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Monad (
  -- * Types
    JSM(..)
  , MonadJSM
  , liftJSM
  , askJSM
  , runJSM
  , syncPoint
  , syncAfter
  , waitForAnimationFrame
  , nextAnimationFrame
  , animationFrameHandlers

  -- * Exception Handling
  , catch
  , bracket
) where

#ifndef ghcjs_HOST_OS
import Control.Monad.Trans.Reader (runReaderT, ask)
#endif
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (catch, bracket)
import Language.Javascript.JSaddle.Types (JSM(..), MonadJSM, liftJSM, askJSM, JSContextRef, runJSM)
import Control.Concurrent.MVar (MVar)

syncPoint :: Applicative m => m ()
syncPoint = pure ()

syncAfter :: Applicative m => m ()
syncAfter = pure ()

waitForAnimationFrame :: m () -> m ()
waitForAnimationFrame = id

nextAnimationFrame :: m () -> m ()
nextAnimationFrame = id

animationFrameHandlers :: JSContextRef -> MVar [Double -> JSM ()]
animationFrameHandlers = undefined
