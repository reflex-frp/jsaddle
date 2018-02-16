function jsaddle(sendRsp, runSyncCallback) {
  var vals = new Map();
  var nextValId = -1;
  var unwrapVal = function(valId) {
    if(typeof val === 'object') {
      if(val.length === 0) {
        return undefined;
      } else {
        return vals.get(valId[0]);
      }
    } else {
      return valId;
    }
  };
  var wrapVal = function(val) {
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
      var valId = nextValId--;
      vals.set(valId, val);
      return [valId];
    }
  };
  var result(ref, val) {
    jsaddle_values.set(ref, val);
    sendRsp({
      'tag': 'Result',
      'contents': [
        ref,
        wrapVal(val)
      ]
    });
  };
  return {
    processReq: function(req) {
      switch(req.tag) {
      case 'Eval':
        result(req.contents[1], eval(req.contents[0]));
        break;
      case 'FreeRef':
        jsaddle_values.delete(req.contents[0]);
        break;
      case 'NewJson':
        result(req.contents[1], req.contents[0]);
        break;
      case 'GetJson':
        sendRsp({
          'tag': 'GetJson',
          'contents': [
            jsaddle_values.get(req)
          ]
        });
        break;
      case 'SyncBlock':
        runSyncCallback(req.contents[0], 0, []);
        break;
      case 'NewSyncCallback':
        var callbackId = req.contents[0];
        result(req.contents[1], function() {
          runSyncCallback(callbackId, wrapVal(this), Array.prototype.slice.call(arguments).map(wrapVal));
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
      default:
        throw 'processReq: unknown request tag ' + JSON.stringify(req.tag);
      }
    }
  };
}
