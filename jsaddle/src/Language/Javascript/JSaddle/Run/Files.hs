{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Run.Files
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@gmail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Run.Files (
    indexHtml
  , jsaddleCoreJs
  , ghcjsHelpers
) where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

indexHtml :: ByteString
indexHtml =
    "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body>\n\
    \</body>\n\
    \<script src=\"jsaddle.js\"></script>\n\
    \</html>"

-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' js/jsaddle-core.js | pbcopy
-- (on linux, use xsel -bi instead of pbcopy)
jsaddleCoreJs :: ByteString
jsaddleCoreJs = "\
    \function jsaddle(global, sendRsp, startSyncCallback, continueSyncCallback) {\n\
    \  var vals = new Map();\n\
    \  vals.set(1, global);\n\
    \  var nextValId = -1;\n\
    \  var unwrapVal = function(valId) {\n\
    \    if(typeof valId === 'object') {\n\
    \      if(valId === null) {\n\
    \        return null;\n\
    \      } else if(valId.length === 0) {\n\
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
    \          req.contents[1],\n\
    \          unwrapVal(req.contents[0])\n\
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

ghcjsHelpers :: ByteString
ghcjsHelpers = "\
    \function h$isNumber(o) {\
    \    return typeof(o) === 'number';\n\
    \}\n\
    \\n\
    \// returns true for null, but not for functions and host objects\n\
    \function h$isObject(o) {\n\
    \    return typeof(o) === 'object';\n\
    \}\n\
    \\n\
    \function h$isString(o) {\n\
    \    return typeof(o) === 'string';\n\
    \}\n\
    \\n\
    \function h$isSymbol(o) {\n\
    \    return typeof(o) === 'symbol';\n\
    \}\n\
    \\n\
    \function h$isBoolean(o) {\n\
    \    return typeof(o) === 'boolean';\n\
    \}\n\
    \\n\
    \function h$isFunction(o) {\n\
    \    return typeof(o) === 'function';\n\
    \}\n\
    \\n\
    \function h$jsTypeOf(o) {\n\
    \    var t = typeof(o);\n\
    \    if(t === 'undefined') return 0;\n\
    \    if(t === 'object')    return 1;\n\
    \    if(t === 'boolean')   return 2;\n\
    \    if(t === 'number')    return 3;\n\
    \    if(t === 'string')    return 4;\n\
    \    if(t === 'symbol')    return 5;\n\
    \    if(t === 'function')  return 6;\n\
    \    return 7; // other, host object etc\n\
    \}\n\
    \\n\
    \function h$jsonTypeOf(o) {\n\
    \    if (!(o instanceof Object)) {\n\
    \        if (o == null) {\n\
    \            return 0;\n\
    \        } else if (typeof o == 'number') {\n\
    \            if (h$isInteger(o)) {\n\
    \                return 1;\n\
    \            } else {\n\
    \                return 2;\n\
    \            }\n\
    \        } else if (typeof o == 'boolean') {\n\
    \            return 3;\n\
    \        } else {\n\
    \            return 4;\n\
    \        }\n\
    \    } else {\n\
    \        if (Object.prototype.toString.call(o) == '[object Array]') {\n\
    \            // it's an array\n\
    \            return 5;\n\
    \        } else if (!o) {\n\
    \            // null \n\
    \            return 0;\n\
    \        } else {\n\
    \            // it's an object\n\
    \            return 6;\n\
    \        }\n\
    \    }\n\
    \\n\
    \}\n\
    \function h$roundUpToMultipleOf(n,m) {\n\
    \  var rem = n % m;\n\
    \  return rem === 0 ? n : n - rem + m;\n\
    \}\n\
    \\n\
    \function h$newByteArray(len) {\n\
    \  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);\n\
    \  var buf = new ArrayBuffer(len0);\n\
    \  return { buf: buf\n\
    \         , len: len\n\
    \         , i3: new Int32Array(buf)\n\
    \         , u8: new Uint8Array(buf)\n\
    \         , u1: new Uint16Array(buf)\n\
    \         , f3: new Float32Array(buf)\n\
    \         , f6: new Float64Array(buf)\n\
    \         , dv: new DataView(buf)\n\
    \         }\n\
    \}\n\
    \function h$wrapBuffer(buf, unalignedOk, offset, length) {\n\
    \  if(!unalignedOk && offset && offset % 8 !== 0) {\n\
    \    throw (\"h$wrapBuffer: offset not aligned:\" + offset);\n\
    \  }\n\
    \  if(!buf || !(buf instanceof ArrayBuffer))\n\
    \    throw \"h$wrapBuffer: not an ArrayBuffer\"\n\
    \  if(!offset) { offset = 0; }\n\
    \  if(!length || length < 0) { length = buf.byteLength - offset; }\n\
    \  return { buf: buf\n\
    \         , len: length\n\
    \         , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)\n\
    \         , u8: new Uint8Array(buf, offset, length)\n\
    \         , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)\n\
    \         , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)\n\
    \         , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)\n\
    \         , dv: new DataView(buf, offset, length)\n\
    \         };\n\
    \}\n\
    \function h$newByteArrayFromBase64String(base64) {\n\
    \  var bin = window.atob(base64);\n\
    \  var ba = h$newByteArray(bin.length);\n\
    \  var u8 = ba.u8;\n\
    \  for (var i = 0; i < bin.length; i++) {\n\
    \    u8[i] = bin.charCodeAt(i);\n\
    \  }\n\
    \  return ba;\n\
    \}\n\
    \function h$byteArrayToBase64String(off, len, ba) {\n\
    \  var bin = '';\n\
    \  var u8 = ba.u8;\n\
    \  var end = off + len;\n\
    \  for (var i = off; i < end; i++) {\n\
    \    bin += String.fromCharCode(u8[i]);\n\
    \  }\n\
    \  return window.btoa(bin);\n\
    \}\n\
    \"
