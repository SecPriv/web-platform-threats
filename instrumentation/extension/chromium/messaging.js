var port = chrome.runtime.connect();

window.addEventListener("extension_log", function(event) {
    try {
        console.log("received extension_log", event.source, window, event)
    } catch(err){
        console.log(err)
    }
    chrome.runtime.sendMessage({
        type: event.detail.type,
        content : event.detail.content,
        ts: event.detail.ts
    });
}, false);