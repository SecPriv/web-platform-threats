const HTTP = 0;
const HTTPS = 2;
const DOMAIN = 0;
const SUBDOMAIN = 4;
const JS = 8;

function delay(n) {
    return new Promise(resolve => setTimeout(resolve, n));
}

async function getAndExpireCookies() {
    return new Promise((resolve, reject) => {
        try {
            const iframe = document.createElement('iframe');
            iframe.style = 'display: none';
            iframe.src = '/crumbles/resources/echo-cookie.html';
            iframe.addEventListener('load', (e) => {
                const win = e.target.contentWindow;
                const iframeCookies = win.getCookies();
                win.expireCookies().then(() => {
                    document.documentElement.removeChild(iframe);
                    resolve(iframeCookies);
                });
            }, {once: true});
            document.documentElement.appendChild(iframe);
        } catch (e) {
            reject(e);
        }
    });
}

function mkSetURL (cookie, conf) {
    return (
        ((conf & HTTPS) ? "https://" : "http://") +
               ((conf & SUBDOMAIN) ? "{{domains[www]}}" : "{{host}}") +
               ((conf & HTTPS) ? ":{{ports[https][0]}}" : ":{{ports[http][0]}}") +
        "/crumbles/resources/cookie.py?" + ((conf & JS) ? "js-set=" : "set=") +
        encodeURIComponent(JSON.stringify(cookie)) );
}
function mkGetURL (conf) {
    return (
        ((conf & HTTPS) ? "https://" : "http://") +
                ((conf & SUBDOMAIN) ? "{{domains[www]}}" : "{{host}}") +
                ((conf & HTTPS) ? ":{{ports[https][0]}}" : ":{{ports[http][0]}}") +
        "/crumbles/resources/read-cookie.py" );
}


function crumblesTest(name, json) {
    return promise_test((t) => {
        return new Promise(async (resolve, reject) => {
            await getAndExpireCookies();
            await test_driver.delete_all_cookies();
            t.add_cleanup(test_driver.delete_all_cookies);

            let preurl = mkSetURL(json.pre[0], json.pre[1]);
            let target = window.open(preurl);
            await delay(2000);
            target.close();
            await delay(400);

            let seturl = mkSetURL(json.set[0], json.set[1]);
            target = window.open(seturl);
            await delay(2000);
            target.close();
            await delay(400);

            target = window.open(mkGetURL(HTTP | DOMAIN));
            await delay(1000);
            target.close();

            target = window.open(mkGetURL(HTTPS | DOMAIN));

            let listener = (e) => {
                window.removeEventListener('message', listener);
                target.close();
                resolve(e.data);
            }
            window.addEventListener('message', listener);
        }).then((cookies) => {
            console.log(cookies);
            assert_true(Object.values(cookies.headers).length > 0, 'Cookies are present');
            if (typeof json.success === 'string' || json.success instanceof String) {
                assert_not_equals(cookies.headers[json.success.split('=')[0]], json.success, 'The cookie was not overwritten.');
            } else {
                assert_false(json.success(cookies), 'Cookie not evicted.');
            }
        });
    }, name);
}
