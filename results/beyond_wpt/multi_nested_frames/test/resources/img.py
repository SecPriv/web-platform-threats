from cookies.resources.helpers import setNoCacheAndCORSHeaders


def main(request, response):
    headers = setNoCacheAndCORSHeaders(request, response)

    headers.append((b"Content-Type", b"image/png"))
    headers.append((b'Set-Cookie', b'cookie_from_insecure_resource=1; Path=/; Domain=' + request.headers.get(b"origin", b'web-platform.test')))
    body = open("images/smiley.png", "rb").read();

    return headers, body
