{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebSockets
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.WebSockets (
  -- * Running JSM over WebSockets
    jsaddleOr
  , jsaddleApp
  , jsaddleWithAppOr
  , jsaddleAppPartial
  , jsaddleJs
--  , debug
--  , debugWrapper
) where

import Control.Monad (when, void, forever, join)
import Control.Concurrent (killThread, forkIO, threadDelay)
import Control.Exception (handle, AsyncException, throwIO, fromException, finally)

import Data.Monoid ((<>))
import Data.Aeson (encode, decode)

import Network.Wai
       (Middleware, lazyRequestBody, Application, Request, Response,
        ResponseReceived)
import Network.WebSockets
       (defaultConnectionOptions, ConnectionOptions(..), sendTextData,
        receiveDataMessage, acceptRequest, ServerApp, sendPing)
import qualified Network.WebSockets as WS (DataMessage(..))
import Network.Wai.Handler.WebSockets (websocketsOr)

import Language.Javascript.JSaddle.Types (JSM(..), JSContextRef(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo)
import qualified Data.Text as T (pack)
import qualified Network.HTTP.Types as H
       (status403, status200)
import Language.Javascript.JSaddle.Run (syncPoint, runJS)
import Language.Javascript.JSaddle.Run.Files (indexHtml, runBatch, ghcjsHelpers, initState)
import Language.Javascript.JSaddle.Debug
       (removeContext, addContext)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M (empty, insert, lookup)
import Data.IORef
       (readIORef, newIORef, atomicModifyIORef', writeIORef)
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.MVar
       (tryTakeMVar, MVar, tryPutMVar, modifyMVar_, putMVar, takeMVar,
        readMVar, newMVar, newEmptyMVar, modifyMVar)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Foreign.Store (newStore, readStore, lookupStore)
import Language.Javascript.JSaddle (askJSM, Rsp, SyncCallbackId, ValId)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleOr opts entryPoint otherApp = do
    processSyncResultRef <- newIORef $ error "processSyncResult not yet set up"
    let processSyncResult callback this args = do
          f <- readIORef processSyncResultRef
          f callback this args
    continueSyncCallbackRef <- newIORef $ error "continueSyncCallback not yet set up"
    let continueSyncCallback = do
          join $ readIORef continueSyncCallbackRef
    let wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            (processResult, processSyncResult', continueSyncCallback', env) <- runJS (sendTextData conn . encode)
            writeIORef processSyncResultRef processSyncResult'
            writeIORef continueSyncCallbackRef continueSyncCallback'
            _ <- forkIO . forever $
                receiveDataMessage conn >>= \case
                    (WS.Text t) ->
                        case decode t of
                            Nothing -> error $ "jsaddle Results decode failed : " <> show t
                            Just r  -> processResult r
                    _ -> error "jsaddle WebSocket unexpected binary data"
            runReaderT (unJSM entryPoint) env
            waitTillClosed conn

        -- Based on Network.WebSocket.forkPingThread
        waitTillClosed conn = ignore `handle` go 1
          where
            go :: Int -> IO ()
            go i = do
                threadDelay (1 * 1000 * 1000)
                sendPing conn (T.pack $ show i)
                go (i + 1)

        ignore e = case fromException e of
            Just async -> throwIO (async :: AsyncException)
            Nothing    -> return ()

        syncHandler :: Application
        syncHandler req sendResponse = case (W.requestMethod req, W.pathInfo req) of
            ("POST", ["sync"]) -> do
                body <- lazyRequestBody req
                case decode body :: Maybe (Maybe (SyncCallbackId, ValId, [ValId])) of
                    Nothing -> error $ "jsaddle sync message decode failed : " <> show body
                    Just parsed -> do
                      result <- case parsed of
                        Just (callback, this, args) -> processSyncResult callback this args
                        Nothing -> continueSyncCallback
                      sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] $ encode result
            _ -> otherApp req sendResponse
    return $ websocketsOr opts wsApp syncHandler


jsaddleApp :: Application
jsaddleApp = jsaddleAppWithJs $ jsaddleJs False

jsaddleAppWithJs :: ByteString -> Application
jsaddleAppWithJs js req sendResponse =
    fromMaybe
        (sendResponse $  W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden")
        (jsaddleAppPartialWithJs js req sendResponse)

jsaddleWithAppOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleWithAppOr opts entryPoint otherApp = jsaddleOr opts entryPoint $ \req sendResponse ->
  (fromMaybe (otherApp req sendResponse)
     (jsaddleAppPartial req sendResponse))

jsaddleAppPartial :: Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartial = jsaddleAppPartialWithJs $ jsaddleJs False

jsaddleAppPartialWithJs :: ByteString -> Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartialWithJs js req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] js
    _ -> Nothing

jsaddleCore :: ByteString
jsaddleCore = "\
    \function jsaddle(global, sendRsp, startSyncCallback, continueSyncCallback) {\n\
    \  var vals = new Map();\n\
    \  vals.set(1, global);\n\
    \  var nextValId = -1;\n\
    \  var unwrapVal = function(valId) {\n\
    \    if(typeof valId === 'object') {\n\
    \      if(valId.length === 0) {\n\
    \        return undefined;\n\
    \      } else {\n\
    \        return vals.get(valId[0]);\n\
    \      }\n\
    \    } else {\n\
    \      return valId;\n\
    \    }\n\
    \  };\n\
    \  var wrapVal = function(val, def) {\n\
    \    switch(typeof val) {\n\
    \    case 'undefined':\n\
    \      return [];\n\
    \    case 'boolean':\n\
    \    case 'number':\n\
    \    case 'string':\n\
    \      return val;\n\
    \    case 'object':\n\
    \      if(val === null) {\n\
    \        return null;\n\
    \      }\n\
    \      // Fall through\n\
    \    default:\n\
    \      if(def) {\n\
    \        return def;\n\
    \      }\n\
    \      var valId = nextValId--;\n\
    \      vals.set(valId, val);\n\
    \      return [valId];\n\
    \    }\n\
    \  };\n\
    \  var result = function(ref, val) {\n\
    \    vals.set(ref, val);\n\
    \    sendRsp({\n\
    \      'tag': 'Result',\n\
    \      'contents': [\n\
    \        ref,\n\
    \        wrapVal(val, [])\n\
    \      ]\n\
    \    });\n\
    \  };\n\
    \  var runSyncCallback = function(callback, that, args) {\n\
    \    var rsp = startSyncCallback(callback, that, args)\n\
    \    while(rsp.Right) {\n\
    \      processReq(rsp.Right);\n\
    \      rsp = continueSyncCallback();\n\
    \    }\n\
    \    return rsp.Left;\n\
    \  };\n\
    \  var processReq = function(req) {\n\
    \    switch(req.tag) {\n\
    \    case 'Eval':\n\
    \      result(req.contents[1], eval(req.contents[0]));\n\
    \      break;\n\
    \    case 'FreeRef':\n\
    \      vals.delete(req.contents[0]);\n\
    \      break;\n\
    \    case 'NewJson':\n\
    \      result(req.contents[1], req.contents[0]);\n\
    \      break;\n\
    \    case 'GetJson':\n\
    \      sendRsp({\n\
    \        'tag': 'GetJson',\n\
    \        'contents': [\n\
    \          unwrapVal(req)\n\
    \        ]\n\
    \      });\n\
    \      break;\n\
    \    case 'SyncBlock':\n\
    \      //TODO: Continuation\n\
    \      runSyncCallback(req.contents[0], [], []);\n\
    \      break;\n\
    \    case 'NewSyncCallback':\n\
    \      result(req.contents[1], function() {\n\
    \        return runSyncCallback(req.contents[0], wrapVal(this), Array.prototype.slice.call(arguments).map(wrapVal));\n\
    \      });\n\
    \      break;\n\
    \    case 'NewAsyncCallback':\n\
    \      var callbackId = req.contents[0];\n\
    \      result(req.contents[1], function() {\n\
    \        sendRsp({\n\
    \          'tag': 'CallAsync',\n\
    \          'contents': [\n\
    \            callbackId,\n\
    \            wrapVal(this),\n\
    \            Array.prototype.slice.call(arguments).map(wrapVal)\n\
    \          ]\n\
    \        });\n\
    \      });\n\
    \      break;\n\
    \    case 'SetProperty':\n\
    \      unwrapVal(req.contents[2])[unwrapVal(req.contents[0])] = unwrapVal(req.contents[1]);\n\
    \      break;\n\
    \    case 'GetProperty':\n\
    \      result(req.contents[2], unwrapVal(req.contents[1])[unwrapVal(req.contents[0])]);\n\
    \      break;\n\
    \    case 'CallAsFunction':\n\
    \      result(req.contents[3], unwrapVal(req.contents[0]).apply(unwrapVal(req.contents[1]), req.contents[2].map(unwrapVal)));\n\
    \      break;\n\
    \    case 'CallAsConstructor':\n\
    \      result(req.contents[2], new (Function.prototype.bind.apply(unwrapVal(req.contents[0]), req.contents[1].map(unwrapVal))));\n\
    \      break;\n\
    \    default:\n\
    \      throw 'processReq: unknown request tag ' + JSON.stringify(req.tag);\n\
    \    }\n\
    \  };\n\
    \  return {\n\
    \    processReq: processReq\n\
    \  };\n\
    \}\n\
    \"


-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' data/jsaddle.js | pbcopy
jsaddleJs :: Bool -> ByteString
jsaddleJs refreshOnLoad = jsaddleCore <> "\
    \if(typeof global !== \"undefined\") {\n\
    \    global.window = global;\n\
    \    global.WebSocket = require('ws');\n\
    \}\n\
    \\n\
    \var connect = function() {\n\
    \    var wsaddress = window.location.protocol.replace('http', 'ws')+\"//\"+window.location.hostname+(window.location.port?(\":\"+window.location.port):\"\");\n\
    \\n\
    \    var ws = new WebSocket(wsaddress);\n\
    \    var sync = function(v) {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', '/sync', false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send(JSON.stringify(v));\n\
    \      return JSON.parse(xhr.response);\n\
    \    };\n\
    \    var core = jsaddle(window, function(a) {\n\
    \      ws.send(JSON.stringify(a));\n\
    \    }, function(callback, that, args) {\n\
    \      return sync([callback, that, args]);\n\
    \    }, function() {\n\
    \      return sync(null);\n\
    \    });\n\
    \    var syncKey = \"\";\n\
    \\n\
    \    ws.onopen = function(e) {\n\
    \\n\
    \        ws.onmessage = function(e) {\n\
    \          core.processReq(JSON.parse(e.data));\n\
    \        }\n\
    \    };\n\
    \    ws.onerror = function() {\n\
    \        setTimeout(connect, 1000);\n\
    \    };\n\
    \}\n\
    \\n\
    \ " <> ghcjsHelpers <> "\
    \connect();\n\
    \"

{-
-- | Start or restart the server.
-- To run this as part of every :reload use
-- > :def! reload (const $ return "::reload\nLanguage.Javascript.JSaddle.Warp.debug 3708 SomeMainModule.someMainFunction")
debug :: Int -> JSM () -> IO ()
debug port f = do
    debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
            jsaddleOr defaultConnectionOptions (registerContext >> f >> syncPoint) (withRefresh $ jsaddleAppWithJs $ jsaddleJs True)
    putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"
-}

refreshMiddleware :: ((Response -> IO ResponseReceived) -> IO ResponseReceived) -> Middleware
refreshMiddleware refresh otherApp req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("POST", ["reload", _syncKey]) -> refresh sendResponse
    _ -> otherApp req sendResponse

{-
debugWrapper :: (Middleware -> JSM () -> IO ()) -> IO ()
debugWrapper run = do
    reloadMVar <- newEmptyMVar
    reloadDoneMVars <- newMVar []
    contexts <- newMVar []
    let refresh sendResponse = do
          reloadDone <- newEmptyMVar
          modifyMVar_ reloadDoneMVars (return . (reloadDone:))
          readMVar reloadMVar
          r <- sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] ("reload" :: ByteString)
          putMVar reloadDone ()
          return r
        start :: Int -> IO (IO Int)
        start expectedConnections = do
            serverDone <- newEmptyMVar
            ready <- newEmptyMVar
            let registerContext :: JSM ()
                registerContext = do
                    uuid <- contextId <$> askJSM
                    browsersConnected <- liftIO $ modifyMVar contexts (\ctxs -> return (uuid:ctxs, length ctxs + 1))
                    addContext
                    when (browsersConnected == expectedConnections) . void . liftIO $ tryPutMVar ready ()
            thread <- forkIO $
                finally (run (refreshMiddleware refresh) registerContext)
                    (putMVar serverDone ())
            _ <- forkIO $ threadDelay 10000000 >> void (tryPutMVar ready ())
            when (expectedConnections /= 0) $ takeMVar ready
            return $ do
                putMVar reloadMVar ()
                ctxs <- takeMVar contexts
                mapM_ removeContext ctxs
                takeMVar reloadDoneMVars >>= mapM_ takeMVar
                tryTakeMVar serverDone >>= \case
                    Nothing -> do
                        killThread thread
                        takeMVar serverDone
                    Just _ -> return ()
                return $ length ctxs
        restarter :: MVar (Int -> IO (IO Int)) -> IO Int -> IO ()
        restarter mvar stop = do
             start' <- takeMVar mvar
             n <- stop
             start' n >>= restarter mvar
    lookupStore shutdown_0 >>= \case
        Nothing -> do
            restartMVar <- newMVar start
            void . forkIO $ restarter restartMVar (return 0)
            void $ newStore restartMVar
        Just shutdownStore -> do
            restartMVar :: MVar (Int -> IO (IO Int)) <- readStore shutdownStore
            void $ tryTakeMVar restartMVar
            putMVar restartMVar start
  where shutdown_0 = 0
-}
