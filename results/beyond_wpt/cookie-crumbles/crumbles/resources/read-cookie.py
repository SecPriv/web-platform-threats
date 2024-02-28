from wptserve.utils import isomorphic_decode

def main(request, response):
    headers = [(b"Content-Type", b"text/html"),
               (b"Access-Control-Allow-Credentials", b"true")]

    if b"origin" in request.headers:
        headers.append((b"Access-Control-Allow-Origin", request.headers[b"origin"]))

    values = []
    for key in request.cookies:
        for value in request.cookies.get_list(key):
            values.append(u"\"%s\": \"%s\"" % (isomorphic_decode(key), value))
    json = u"{ %s }" % u",".join(values)

    body = f'''
    <!DOCTYPE html>
    <script>
        window.opener.postMessage({{headers: {json}, js: document.cookie.split("; ").filter(x=>x)}}, "*");
    </script>
    '''
    
    return headers, body
