const filter = { urls: [ "<all_urls>" ] };

URL.prototype.toJSON = function() { return {
            host: this.host,
            hostname: this.hostname,
            href: this.href,
            origin: this.origin,
            pathname: this.pathname,
            port: this.port,
            protocol: this.protocol,
            site: this.hostname.split("\.").slice(-2).join(".")
        }; }


var events = {network: {requests: {}, responses: {}, redirects: {}}, cookieStore: {}, onChanged: {}, events: [], tests: {}}

var prev_cookies = null
var cookie_logs = []

chrome.cookies.getAll({domain: 'web-platform.test'}).then((cookies) => {
    prev_cookies = cookies
})

function poll_cookies(){
    var dt = Date.now()
    
    chrome.cookies.getAll({domain: 'web-platform.test'}).then((cdom1) => {
        chrome.cookies.getAll({domain: 'not-web-platform.test'}).then((cdom2) => {
            cookies = cdom1.concat(cdom2)
            if (JSON.stringify(cookies) != JSON.stringify(prev_cookies)){
                cookie_logs.push({ts: dt, cookies:cookies, prev: prev_cookies})
                prev_cookies = cookies
            }
        })
    })
}


var poll_id = setInterval(poll_cookies, 1)

function logStores(details, ts) {
    events.onChanged[ts] = details
    console.log("COOKIEJAR", ts, details)
}

chrome.cookies.onChanged.addListener(
    (details) => { logStores(details, Date.now()) }
)


function convert_csevents(cookies) {
    function dff(obj1, obj2) {
        const result = {};
        if (Object.is(obj1, obj2)) {
            return undefined;
        }
        if (!obj2 || typeof obj2 !== 'object') {
            return obj2;
        }
        Object.keys(obj1 || {}).concat(Object.keys(obj2 || {})).forEach(key => {
            if(obj2[key] !== obj1[key] && !Object.is(obj1[key], obj2[key])) {
                result[key] = obj2[key];
            }
            if(typeof obj2[key] === 'object' && typeof obj1[key] === 'object') {
                const value = dff(obj1[key], obj2[key]);
                if (value !== undefined) {
                    result[key] = value;
                }
            }
        });
        return result;
    }

    let res = {}
    cookies.forEach(function(el, idx) {
        // assume the cookieJar starts empty
        res[el.ts] = []
        try{
            let d = dff(el.prev, el.cookies)
            // d will have multiple entries: 
            // - [index] = Object() -> unchanged (no event)
            // - [index] = undefined -> removed (removed: true)
            // - [index] = Object(...) -> changed (removed: false)

            Object.keys(d).forEach(function(e, i){
                if (!d[e]){
                    res[el.ts].push({removed: true, cookie: el.prev[e]})
                } else if(Object.keys(d[e]).length > 0){ 
                    // event, removed=false
                    res[el.ts].push({removed: false, cookie: el.cookies[e]})
                }
            })
        } catch(error){
            console.log(error)
        }
    })
    return res
}

chrome.runtime.onMessage.addListener(
async function(request, sender, sendResponse) {
    console.log("[EXTENSION]", request)

    switch(request.type) {
        case "START":
            start = request.ts;
            if (!events.tests[request.content.name]) {
                events.tests[request.content.name] = {'start': request.ts, 'end': undefined};
            }
            break;

        case "END":
            events.tests[request.content.name]['end'] = request.ts;
            break;
        case "DOWNLOAD":
            await new Promise(r => setTimeout(r, 1000)); 
            // convert cookieStore events
            let cookieStoreEvents = convert_csevents(cookie_logs)
            cookie_logs = []


            let proxy_data = null;
            try {
                proxy_data = await (await fetch('http://b1v253vpqwutupjwntfmpc9a4m94s5zm.mitm.it/download-trace')).json()

                proxy_data.responses.forEach(function(el, idx) {
                    el.body = atob(proxy_data.responses[idx].body)
                })
                proxy_data.requests.forEach(function(el, idx) {
                    el.body = atob(proxy_data.requests[idx].body)
                })
            } catch (e) {
                console.error(e);
            }

            // add redirects as a list of url's 
            for (const rid in events.network.redirects){
                events.network.requests[rid].redir = events.network.redirects[rid].map((red) => { console.log(red.url); return new URL(red.url) })
            }

            let out = {
                'network': events.network,
                'events': events.events,
                'cookies': cookieStoreEvents,
                'tests': events.tests
            };
            if (proxy_data) out['proxy'] = proxy_data;

            // Add DOWNLOAD timestamp if it is missing
            for (let t in out.tests) {
                if (!out.tests[t]['end'])
                    out.tests[t]['end'] = request.ts
            };


            let data = JSON.stringify(out);
            let u = 'data:application/json;base64,' + btoa(data)

            chrome.downloads.download({url: u, filename: `${Date.now()}_chromium.json`})

            events = {network: {requests: {}, responses: {}, redirects: {}}, cookieStore: {}, onChanged: {}, events: [], tests: {}}
            break;

        case "EVENT":
        case "ASSERT":
            request.content.timeStamp = request.ts
            console.log("ADD", request.content.timeStamp, request.content.type);
            try{
                request.content.orig = new URL(request.content.orig)
            }catch(e){ }

            events.events.push(request.content)
            break;

        default:
            console.log("Unkown event type");
    } 
});

function getAncestorOrigins(){
    return document.location.ancestorOrigins;
}

function buildFrameAncestors(frames, start, acc=[]){
    try{
        if(Object.keys(frames).length == 0){
            return acc
        }
        let parent = frames[start]?.parentFrameId
        if (!parent || parent == -1){
            return acc
        }
        let u = new URL(frames[parent].url)
        u.site = u.hostname.split("\.").slice(-2).join(".")
        acc.unshift({ url: u });
        return buildFrameAncestors(frames, parent, acc)
    } catch(e) {
        console.error("error building frame ancestors", e)
        return []
    }
}

function getFrameCallback(d, details){
    // build a map keyed on the frameId
    try{
        var frames = undefined
        if(d){
            frames = d.reduce(function(map, obj) {
                map[obj.frameId] = obj;
                return map;
            }, {});
        } 

        if(!details.originUrl && frames){
            // if iframe and about:blank, then its the parent
            if(details.type == "sub_frame" && frames[details.frameId] && frames[details.parentFrameId]  && frames[details.frameId].url == "about:blank"){
                details.originUrl = frames[details.parentFrameId].url
            } else{
                details.originUrl = details.initiator
            }
        }

        if(!details.documentUrl){
            if(details.type == "sub_frame" && frames && frames[details.parentFrameId]){
                details.documentUrl = frames[details.parentFrameId].url
            } else {
                details.documentUrl = details.originUrl
            }
        }
        
        // recursively build the frameAncestors list
        if (frames){
            let ancestors = buildFrameAncestors(frames, details.frameId)
            details["frameAncestors"] = ancestors
        } else {
            details["frameAncestors"] = []
        }

        details.url = new URL(details.url)
        details.url.site = details.url.hostname.split("\.").slice(-2).join(".")

        try{
            details.originUrl = new URL(details.originUrl)
            details.originUrl.site = details.originUrl.hostname.split("\.").slice(-2).join(".")
        }catch(e){
            details.originUrl = undefined
        }

        try{
            details.documentUrl = new URL(details.documentUrl)
            details.documentUrl.site = details.documentUrl.hostname.split("\.").slice(-2).join(".")
        }catch(e){
            details.documentUrl = undefined
        }


        events.network.requests[details.requestId] = details;
    } catch(e){
        console.error(e)
    }
}

function onSendHeaders(details){
    tabId = details.tabId;
    
    if (details.url.startsWith("chrome-extension://")){
        return;
    }

    events.network.requests[details.requestId] = details;
    events.network.requests[details.requestId].url = new URL(details.url);
    events.network.requests[details.requestId].url.site = details.url.hostname.split("\.").slice(-2).join(".");

    try{
        if(!details.originUrl) details.originUrl = details.initiator
        details.originUrl = new URL(details.originUrl)
        details.originUrl.site = details.originUrl.hostname.split("\.").slice(-2).join(".")
        details.documentUrl = new URL(details.documentUrl)
        details.documentUrl.site = details.documentUrl.hostname.split("\.").slice(-2).join(".")
    }catch(e){
        console.error("[onSendHeaders] preemptive documentUrl population", e)
    }

    if (tabId <0){
        details["frameAncestors"] = []
    } else{
        chrome.webNavigation.getAllFrames(
            {tabId: tabId},
            (d) => {getFrameCallback(d, details)}
        )
    }
}

function onCompleted(details){
    details.url = new URL(details.url)
    details.url.site = details.url.hostname.split("\.").slice(-2).join(".")

    try{
        details.originUrl = new URL(details.originUrl)
        details.originUrl.site = details.originUrl.hostname.split("\.").slice(-2).join(".")
    }catch(e){
        details.originUrl = undefined
    }

    try{
        details.documentUrl = new URL(details.documentUrl)
        details.documentUrl.site = details.documentUrl.hostname.split("\.").slice(-2).join(".")
    }catch(e){
        details.documentUrl = undefined
    }
    events.network.responses[details.requestId] = details;
    if(!details.responseHeaders){
        events.network.responses[details.requestId].responseHeaders = []
    }
}
function onBeforeRedirect(details){
    console.log("[onBeforeRedirect]", details);

    if(details.requestId in events.network.redirects){
        events.network.redirects[details.requestId].push(details);
    } else {
        events.network.redirects[details.requestId] = [details];
    }
}

chrome.webRequest.onBeforeRedirect.addListener(
    (details) => {onBeforeRedirect(details)}, 
    filter,["responseHeaders", "extraHeaders"]);

chrome.webRequest.onCompleted.addListener(
    (details) => {onCompleted(details)}, 
    filter,["responseHeaders", "extraHeaders"]);


chrome.webRequest.onSendHeaders.addListener(
    (details) => {onSendHeaders(details)},
    filter,["requestHeaders", "extraHeaders"]);


chrome.scripting.registerContentScripts([
{
    id: 'instr',
    matches: ['http://*/*', 'https://*/*'],
    js: ['instr.js'],
    runAt: 'document_start',
    world: 'MAIN',
}],() => {console.log("registerd")});

