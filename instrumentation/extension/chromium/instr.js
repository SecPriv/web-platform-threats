(function setup_logs(w, inner = false) {

    //Function to get an UUID
    function token() {
        var uuid = [to_hex(rand_int(32), 8),
            to_hex(rand_int(16), 4),
            to_hex(0x4000 | rand_int(12), 4),
            to_hex(0x8000 | rand_int(14), 4),
            to_hex(rand_int(48), 12)].join("-")
            return uuid;
    }

    function rand_int(bits) {
        if (bits < 1 || bits > 53) {
            throw new TypeError();
        } else {
            if (bits >= 1 && bits <= 30) {
                return 0 | ((1 << bits) * Math.random());
            } else {
                var high = (0 | ((1 << (bits - 30)) * Math.random())) * (1 << 30);
                var low = 0 | ((1 << 30) * Math.random());
                return high + low;
            }
        }
    }

    function to_hex(x, length) {
        var rv = x.toString(16);
        while (rv.length < length) {
            rv = "0" + rv;
        }
        return rv;
    }

    // if window is null or has been instrumented before
    var _postMessage = false;
    var _opener = false;
    var _fetch = true;
    var _xhr = true;
    var _js_proxy = false;
    var _document_cookie = true;

    if (!w) { return }
    if (w.__id__ != undefined) { return }

    w.__id__ = token()
    console.log("[setup_logs] start", w.__id__)

    function sleep(delay) {
        const start = Date.now();
        let currentDate = null;
        do {
            currentDate = Date.now();
        } while (currentDate - start < delay);
    }

    function csFramed(){
        try{
            if(w.self !== w.top){
                var l = w.parent.location.href + w.top.location.href;
            }
        } catch(e){
            return true
        }
        return false
    }

    function log_to_extension(type, args = [], ret = null, ts = Date.now()) {
        let content = {
            'isSecure': w.isSecureContext,
            'wid': w.__id__,
            'orig': w.location.href,
            'type': type,
            'args': args,
            'ret': ret,
            'csFramed': csFramed()
        };
        w.dispatchEvent(new CustomEvent('extension_log', { detail: { type: "EVENT", content: content, ts: ts } }))

    }

    class ProxyFactory {
        value_object_map = {}

        create = (object, type="get") => {
            let val = object.valueOf()
            console.log("this", this)

            if (this.value_object_map.hasOwnProperty(val)) {
                // an object with this value has already been proxied. return it
                return this.value_object_map[val]
            }

            let proxy = type == "get" ? this.proxy_get(object, this) : this.proxy_apply(object)

            this.value_object_map[val] = proxy
            proxy['proxy_ssdgg7frbdo8UoZP8d4L8wjUkVgSSu1ABTECGCo0'] = true
            return proxy
        }

        proxy_get = (object, parent) => {
            return new Proxy({ value: object }, {
                valueOf() {
                    console.log("[get] calling valueOf")
                    return value.valueOf()
                },
                get(target, prop, receiver) {
                    try {
                        var prim = Reflect.get(target, 'value');
                        const value = prim[prop];


                        if (prop != "toString") {
                            let type = `GET (${prim}).${prop.toString()} => ${value}`
                            let args = [prop.toString()]

                            if (typeof value === 'function') {
                                log_to_extension(type, args, value.toString())
                                return parent.create(value.bind(prim), "apply")
                            }
                            log_to_extension(type, args, value)
                            return value

                        } else {
                            return typeof value === 'function' ? value.bind(prim.valueOf()) : value;
                        }
                    } catch (err) {
                        console.log("[proxy get]", err)
                    }
                }
            })
        }

        proxy_apply = (object) => {
            return new Proxy(object, {
                apply(target, that, args) {
                    try {
                        sleep(20);
                        log_to_extension(`BEGIN APPLY (${that.toString()}).${target.name}`, [args])

                        let res = Reflect.apply(target, that, args);

                        sleep(20);
                        log_to_extension(`END APPLY (${that.toString()}).${target.name} => ${res}`, [args], res)

                        return res
                    } catch (err) {
                        console.log("oops", err)
                    }
                },
                valueOf() {
                    console.log("[apply] calling valueof")
                    return v.valueOf()
                }
            });
        }
    };

    function strip_proxy(object) {
        if (!js_proxy) return
        let keys = Object.keys(object)

        if (!keys) {
            console.log("returning object")
            return object
        }

        if (keys.length == 1 && keys[0] == "0") { // weird case for when the value is a string
            return object
        }

        if (keys.indexOf('proxy_ssdgg7frbdo8UoZP8d4L8wjUkVgSSu1ABTECGCo0') != -1) {
            console.log("returning vaueof", object.valueOf())
            return object.valueOf()
        } else {
            keys.forEach(function (el, idx) {
                console.log("iterating", object, object[el])
                object[el] = strip_proxy(object[el])

            });
            return object
        }
    }

    var proxy_factory = new ProxyFactory()

    var cookie_setter_orig = w.document.__lookupSetter__("cookie").bind(w.document);
    var cookie_getter_orig = w.document.__lookupGetter__("cookie").bind(w.document);
    if(_document_cookie){
        try {
            Object.defineProperty(w.document, "cookie", {
                get: function () {
                    sleep(20)
                    let a = cookie_getter_orig();
                    log_to_extension('DocumentCookieGet', [], a);
                    return !_js_proxy || minimal ? a : proxy_factory.create(a)
                },
                set: function (val) {
                    sleep(20)
                    let res = cookie_setter_orig(val)

                    log_to_extension('DocumentCookieSet', [val], res)

                    return res
                }
            }
            )
        } catch (error) {
            console.log(`[setup_logs: ${w.__id__}]could overwrite document.cookie:`, error)
        }
    }

    // WINDOW.OPEN
    let original_open = w.open.bind(w)
    let new_open = function (url, name, features) {
        new_open.instrumented = true

        log_to_extension('WindowOpen', [url, name, features])

        return setup_logs(original_open(url, name, features));
    }


    w.open = w.open.instrumented ? w.open : new_open

    // WINDOW.POSTMESSAGE
    let original_opener = w.opener;
    let original_opener_pm = (w.opener != null) ? w.opener.postMessage.bind(w.opener) : null
    let original_pm = w.postMessage



    let new_pm = function (message, targetorigin) {
        new_pm.instrumented = true
        try {
            message = strip_proxy(message)
            log_to_extension('PostMessage', [message, targetorigin])
            return original_pm(message, targetorigin)
        } catch (err) {
            console.log("[new_pm]", err)
        }
    }

    w.postMessage = (w.postMessage.instrumented || !_postMessage) ? w.postMessage : new_pm

    const newWindowOpener = {
        // Custom `postMessage` method
        postMessage: function(message,targetorigin="*") {
            try{
                message = strip_proxy(message)

                log_to_extension('PostMessage', [message, targetorigin])

                return original_opener_pm(message, targetorigin);

            }catch(err){
                console.log("[new_pm]", err)
            }
        }
    };

    // copy every property from w.opener to the new object
    if(w.opener != null && _opener){
        w.opener = newWindowOpener;

        try{
            for(let el in original_opener){
                if(!w.opener[el]){
                    w.opener[el] = original_opener[el]
                }
            }
        } catch (err){
            console.error("DEEED", err)
        }
    }

    let original_fetch = w.fetch.bind(w);
    let new_fetch = function(url, options){
        new_fetch.instrumented = true
        console.log("[fetch]", url, options)
        log_to_extension('Fetch', [url, options])
        if (!options){
            return original_fetch(url);
        } else {
            return original_fetch(url, options);
        }
    }
    w.fetch = (w.fetch.instrumented || !_fetch) ? w.fetch : new_fetch;

    var original_XHR = XMLHttpRequest;
    new_XHR = function() {
        new_XHR.instrumented = true
        var xhr = new original_XHR();
        var _open = xhr.open;

        xhr.open = function() {
            console.log("[XHR]", arguments);
            log_to_extension('XHR', [arguments[0], arguments[1]]);
            return _open.apply(this, arguments);
        }

        return xhr;
    }
    w.XMLHttpRequest = (w.XMLHttpRequest.instrumented || !_xhr) ? w.XMLHttpRequest : new_XHR;

    var dom_watcher = (mutationList, observer) => {
        for (const mutation of mutationList) {
            for(const added of mutation.addedNodes ){

                if (added.tagName == "IFRAME"){
                    try{
                        log_to_extension("IFRAME", [added.src])
                    } catch(err){
                        console.log("[dom_watcher] error:", err)
                    }
                }
            }
        }
    };

    console.log("[setup_logs] end", w.__id__, w.location, w.origin, w.document, w)
    return w
})(window);
