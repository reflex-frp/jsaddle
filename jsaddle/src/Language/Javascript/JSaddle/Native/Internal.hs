{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Native
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Native.Internal (
    setPropertyByName
  , setPropertyAtIndex
  , stringToValue
  , numberToValue
  , jsonValueToValue
  , getPropertyByName
  , getPropertyAtIndex
  , callAsFunction
  , callAsConstructor
  , newEmptyObject
  , newAsyncCallback
  , newSyncCallback
  , newArray
  , evaluateScript
  , valueToBool
  , valueToNumber
  , valueToString
  , valueToJSON
  , valueToJSONValue
  , isNull
  , isUndefined
  , strictEqual
  , instanceOf
  , propertyNames
) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO(..))

import Data.Aeson (Value)
import qualified Data.Aeson as A

import GHCJS.Prim.Internal
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Run
import GHC.IORef (IORef(..), readIORef)
import GHC.STRef (STRef(..))
import GHC.IO (IO(..))
import GHC.Base (touch#)

setPropertyByName :: JSString -> JSVal -> Object -> JSM ()
setPropertyByName name val this = undefined
{-# INLINE setPropertyByName #-}

setPropertyAtIndex :: Int -> JSVal -> Object -> JSM ()
setPropertyAtIndex index val this = undefined
{-# INLINE setPropertyAtIndex #-}

stringToValue :: JSString -> JSM JSVal
stringToValue = return . primToJSVal . PrimVal_String . unJSString
{-# INLINE stringToValue #-}

numberToValue :: Double -> JSM JSVal
numberToValue = return . primToJSVal . PrimVal_Number
{-# INLINE numberToValue #-}

jsonValueToValue :: Value -> JSM JSVal
jsonValueToValue = undefined
{-# INLINE jsonValueToValue #-}

getPropertyByName :: JSString -> Object -> JSM JSVal
getPropertyByName name this = undefined
{-# INLINE getPropertyByName #-}

getPropertyAtIndex :: Int -> Object -> JSM JSVal
getPropertyAtIndex index this = undefined
{-# INLINE getPropertyAtIndex #-}

callAsFunction :: Object -> Object -> [JSVal] -> JSM JSVal
callAsFunction f this args = undefined
{-# INLINE callAsFunction #-}

callAsConstructor :: Object -> [JSVal] -> JSM JSVal
callAsConstructor f args = undefined
{-# INLINE callAsConstructor #-}

newEmptyObject :: JSM Object
newEmptyObject = fmap (Object . JSVal) $ lazyValResult =<< newJson (A.Object mempty)
{-# INLINE newEmptyObject #-}

newAsyncCallback :: JSCallAsFunction -> JSM CallbackId
newAsyncCallback f = undefined
{-# INLINE newAsyncCallback #-}

newSyncCallback :: JSCallAsFunction -> JSM SyncCallbackId
newSyncCallback f = undefined
{-# INLINE newSyncCallback #-}

newArray :: [JSVal] -> JSM JSVal
newArray xs = undefined
{-# INLINE newArray #-}

evaluateScript :: JSString -> JSM JSVal
evaluateScript (JSString str) = eval str
{-# INLINE evaluateScript #-}

valueToBool :: JSVal -> JSM Bool
valueToBool val = case getPrimJSVal val of
  PrimVal_Undefined -> return False
  PrimVal_Null -> return False
  PrimVal_Bool b -> return b
  PrimVal_Number n -> return $ n /= 0 --TODO: NaN, although it should come across as "null", so this should work accidentally
  PrimVal_String s -> return $ s /= ""
  PrimVal_Ref _ -> return True -- Always truthy, because all falsey values are primitive
  -- I think what we want to do is:
  --   Whenever the JS side fills in a reference for us, it automatically sends a response saying what it filled it in with - a PrimVal (), which represents whether it was filled in with a primitive value or with something else.  Until that response arrives, we can keep using the reference value; once it arrives, we can finalize our reference (sending the FreeVal command) and then simply send the simple value.
{-# INLINE valueToBool #-}

valueToNumber :: JSVal -> JSM Double
valueToNumber val = case getPrimJSVal val of
  PrimVal_Undefined -> return $ 0/0 -- NaN
  PrimVal_Null -> return 0
  PrimVal_Bool False -> return 0
  PrimVal_Bool True -> return 1
  PrimVal_Number n -> return n
  PrimVal_String s -> undefined --TODO: Can we do this conversion safely here?
  PrimVal_Ref _ -> undefined --TODO: run Number() on it
{-# INLINE valueToNumber #-}

valueToString :: JSVal -> JSM JSString
valueToString val = case getPrimJSVal val of
  PrimVal_Undefined -> return "undefined"
  PrimVal_Null -> return "null"
  PrimVal_Bool False -> return "false"
  PrimVal_Bool True -> return "true"
  PrimVal_Number n -> undefined --TODO
  PrimVal_String s -> return $ JSString s
  PrimVal_Ref _ -> undefined --TODO: When we have a negative value, we always have a truthy value, beca
{-# INLINE valueToString #-}

valueToJSON :: JSVal -> JSM JSString
valueToJSON value = undefined --TODO: Define in terms of valueToJSONValue
{-# INLINE valueToJSON #-}

valueToJSONValue :: JSVal -> JSM Value
valueToJSONValue value = undefined
{-# INLINE valueToJSONValue #-}

isNull :: JSVal -> JSM Bool
isNull val = case getPrimJSVal val of
  PrimVal_Null -> return True
  _ -> return False
{-# INLINE isNull #-}

isUndefined :: JSVal -> JSM Bool
isUndefined val = case getPrimJSVal val of
  PrimVal_Undefined -> return True
  _ -> return False
{-# INLINE isUndefined #-}

strictEqual :: JSVal -> JSVal -> JSM Bool
strictEqual a b = undefined
{-# INLINE strictEqual #-}

instanceOf :: JSVal -> Object -> JSM Bool
instanceOf value constructor = undefined
{-# INLINE instanceOf #-}

propertyNames :: Object -> JSM [JSString]
propertyNames this = undefined
{-# INLINE propertyNames #-}
