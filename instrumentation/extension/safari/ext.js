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

browser.cookies.getAll({domain: 'web-platform.test'}).then((cdom1) => {
    browser.cookies.getAll({domain: 'not-web-platform.test'}).then((cdom2) => {
        cookies = cdom1.concat(cdom2)
        prev_cookies = cookies
    })
})

function poll_cookies(){
    var dt = Date.now()

    browser.cookies.getAll({domain: 'web-platform.test'}).then((cdom1) => {
        browser.cookies.getAll({domain: 'not-web-platform.test'}).then((cdom2) => {
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

browser.cookies.onChanged.addListener(
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

browser.runtime.onMessage.addListener(
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

            let blob = new Blob([JSON.stringify(out)], {type: 'application/octet-stream'});
            let u = URL.createObjectURL(blob);

            window.open(u);

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


function buildFrameAncestors(frames, start, acc = []) {
  try {
    let parent = !frames[start] ? -1 : frames[start].parentFrameId;

    if (parent == -1) {
      return acc;
    }
    let u = !frames[parent] ? new URL(frames[0].url) : new URL(frames[parent].url)

    u.site = u.hostname.split("\.").slice(-2).join(".")
    acc.unshift({ url: u });
    return buildFrameAncestors(frames, parent, acc);
  } catch (e) {
    console.log("DED", e);
  }
}

function getFrameResponseCallback(d, details){
    try{
        var frames = undefined
        if(d){
            frames = d.reduce(function(map, obj) {
                map[obj.frameId] = obj;
                return map;
            }, {});
        }

        console.log("[getFrameResponseCallback] frames", frames)

        let content_type = details.responseHeaders.filter(hd => hd.name == "Content-Type")[0].value

        let parentId = (details.parentFrameId != -1 && !frames[details.parentFrameId]) ? 0 : details.parentFrameId

        if (content_type.includes("html") && frames[parentId]){
            events.network.requests[details.requestId].documentUrl = new URL(frames[parentId].url);
            events.network.requests[details.requestId].documentUrl.site = events.network.requests[details.requestId].documentUrl.hostname.split("\.").slice(-2).join(".")
        }
    } catch (e){
        console.error("[getFrameResponseCallback]", e)
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
            if(details.type == "sub_frame" && frames[details.frameId] && frames[details.frameId].url == "about:blank" && frames[details.parentFrameId]){
                details.originUrl = frames[details.parentFrameId].url
            } else if (frames[details.frameId]){
                // otherwise, its just the URL
                details.originUrl = frames[details.frameId].url
            } else {
                // in this case, we are the parent
                details.originUrl = frames[details.frameId].url
            }
        }

        if(!details.documentUrl && frames){
            if(details.type == "sub_frame" && frames[details.parentFrameId]){
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
        console.error("[getFrameCallback]", e)
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
    console.log("COMPLETED", details);
    let tabId = details.tabId;

    details.url = new URL(details.url);
    details.url.site = details.url.hostname.split("\.").slice(-2).join(".");

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

    browser.webNavigation.getAllFrames({ tabId: tabId }, (d) => {
        getFrameResponseCallback(d, details);
    });

    console.log("details after mod", details)
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

browser.webRequest.onBeforeRedirect.addListener(
    (details) => {onBeforeRedirect(details)},
    filter,["responseHeaders", "extraHeaders"]);

browser.webRequest.onCompleted.addListener(
    (details) => {onCompleted(details)},
    filter,["responseHeaders", "extraHeaders"]);


browser.webRequest.onSendHeaders.addListener(
    (details) => {onSendHeaders(details)},
    filter,["requestHeaders", "extraHeaders"]);
