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

module Language.Javascript.JSaddle.Native (
    module Language.Javascript.JSaddle.Native.Internal
) where

import GHCJS.Marshal.Internal (ToJSVal(..))
import Language.Javascript.JSaddle.Types
       (JSM(..))
import Language.Javascript.JSaddle.Native.Internal
