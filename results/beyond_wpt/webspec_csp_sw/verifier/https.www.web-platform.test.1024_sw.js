
let STASH_RESPONDER = "wss://web-platform.test:8666/stash_responder_blocking";

class StashUtils {
  static putValue(key, value) {
    return new Promise(resolve => {
        const ws = new WebSocket(STASH_RESPONDER);
        ws.onopen = () => {
          ws.send(JSON.stringify({action: 'set', key: key, value: value}));
        };
        ws.onmessage = e => {
          ws.close();
          resolve();
        };
    });
  }

  static takeValue(key) {
    return new Promise(resolve => {
      const ws = new WebSocket(STASH_RESPONDER);
      ws.onopen = () => {
        ws.send(JSON.stringify({action: 'get', key: key}));
      };
      ws.onmessage = e => {
        ws.close();
        resolve(JSON.parse(e.data).value);
      };
    });
  }
}

caches.open("obghtagajq"); 

asserts = {

  'https://www.web-platform.test:1024/verifier/server.py' : {'uuid' : 'a463d3ae-16ec-4c83-a114-33323d8dd499', 'val' : 'https://www.web-platform.test:1024/verifier/server.py', 'lock' : 'cbc7b74b-30cf-4f35-bf18-6b86e1f84605', 'next' : ''},

};


 



self.addEventListener('fetch', (event) => {
    // check if request is meant to be matched
    let ass_match = asserts[event.request.url];
    if (ass_match) {
        event.respondWith((async () => {
          await StashUtils.takeValue(ass_match['lock'])
          let mycache = await caches.open("obghtagajq")
          let response = await mycache.match(event.request, options={ignoreVary:true})
          StashUtils.putValue(ass_match['uuid'], ass_match['val'])
          if (ass_match['next'] != '') {
              StashUtils.putValue(ass_match['next'], 'dummy')
          }
          return response
        })())
    } else {
        // not meant to be matched, return as if the cache.match failed
        return
    }
});
