import json

from cookies.resources.helpers import setNoCacheAndCORSHeaders
from wptserve.utils import isomorphic_decode
from wptserve.utils import isomorphic_encode


def main(request, response):
    headers = setNoCacheAndCORSHeaders(request, response)

    if b'set' in request.GET:
        cookie = isomorphic_decode(request.GET[b'set'])
        cookie = json.loads(cookie)
        cookies = cookie if isinstance(cookie, list) else [cookie]
        for c in cookies:
            headers.append((b'Set-Cookie', isomorphic_encode(c)))
        return headers, b'{"success": true}'
            
    if b'js-set' in request.GET:
        headers.extend([(b"Content-Type", b"text/html"),
                       (b"Access-Control-Allow-Credentials", b"true")])
        cookie = isomorphic_decode(request.GET[b'js-set'])
        cookie = json.loads(cookie)
        cookies = cookie if isinstance(cookie, list) else [cookie]
        js_cookies = '\n'.join([ f'document.cookie = "{isomorphic_encode(c).decode()}";' for c in cookies ])
        body = f'''
                <!DOCTYPE html>
                <script>
                {js_cookies}
                </script>
        '''
        return headers, body
        
