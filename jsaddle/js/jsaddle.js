function jsaddle(global, sendRsp, startSyncCallback, continueSyncCallback) {
  var vals = new Map();
  vals.set(1, global);
  var nextValId = -1;
  var unwrapVal = function(valId) {
    if(typeof valId === 'object') {
      if(valId.length === 0) {
        return undefined;
      } else {
        return vals.get(valId[0]);
      }
    } else {
      return valId;
    }
  };
  var wrapVal = function(val, def) {
    switch(typeof val) {
    case 'undefined':
      return [];
    case 'boolean':
    case 'number':
    case 'string':
      return val;
    case 'object':
      if(val === null) {
        return null;
      }
      // Fall through
    default:
      if(def) {
        return def;
      }
      var valId = nextValId--;
      vals.set(valId, val);
      return [valId];
    }
  };
  var result = function(ref, val) {
    vals.set(ref, val);
    sendRsp({
      'tag': 'Result',
      'contents': [
        ref,
        wrapVal(val, [])
      ]
    });
  };
  var runSyncCallback = function(callback, that, args) {
    var rsp = startSyncCallback(callback, that, args)
    while(rsp.Right) {
      processReq(rsp.Right);
      rsp = continueSyncCallback();
    }
    return rsp.Left;
  };
  var processReq = function(req) {
    switch(req.tag) {
    case 'Eval':
      result(req.contents[1], eval(req.contents[0]));
      break;
    case 'FreeRef':
      vals.delete(req.contents[0]);
      break;
    case 'NewJson':
      result(req.contents[1], req.contents[0]);
      break;
    case 'GetJson':
      sendRsp({
        'tag': 'GetJson',
        'contents': [
          unwrapVal(req)
        ]
      });
      break;
    case 'SyncBlock':
      //TODO: Continuation
      runSyncCallback(req.contents[0], [], []);
      break;
    case 'NewSyncCallback':
      result(req.contents[1], function() {
        return runSyncCallback(req.contents[0], wrapVal(this), Array.prototype.slice.call(arguments).map(wrapVal));
      });
      break;
    case 'NewAsyncCallback':
      var callbackId = req.contents[0];
      result(req.contents[1], function() {
        sendRsp({
          'tag': 'CallAsync',
          'contents': [
            callbackId,
            wrapVal(this),
            Array.prototype.slice.call(arguments).map(wrapVal)
          ]
        });
      });
      break;
    case 'SetProperty':
      unwrapVal(req.contents[2])[unwrapVal(req.contents[0])] = unwrapVal(req.contents[1]);
      break;
    case 'GetProperty':
      result(req.contents[2], unwrapVal(req.contents[1])[unwrapVal(req.contents[0])]);
      break;
    case 'CallAsFunction':
      result(req.contents[3], unwrapVal(req.contents[0]).apply(unwrapVal(req.contents[1]), req.contents[2].map(unwrapVal)));
      break;
    case 'CallAsConstructor':
      result(req.contents[2], new (Function.prototype.bind.apply(unwrapVal(req.contents[0]), req.contents[1].map(unwrapVal))));
      break;
    default:
      throw 'processReq: unknown request tag ' + JSON.stringify(req.tag);
    }
  };
  return {
    processReq: processReq
  };
}
