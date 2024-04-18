
from wptserve.utils import isomorphic_decode, isomorphic_encode
from urllib.parse import parse_qs
from urllib.parse import urlparse
import json
import ssl, websocket, asyncio


GLOBALS = {'uuid' : "", "stash" : ""}

def update_globals(parsed_req):
    global GLOBALS

    for g in GLOBALS.keys():
        if g in parsed_req:
            GLOBALS[g] = parsed_req[g][0]

def stash_add(uuid, data):
	try:
		url = "wss://web-platform.test:8666/stash_responder_blocking"
		ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
		cert_path = "../tools/certs/cacert.pem"
		ssl_context.load_verify_locations(cert_path)
		ws = websocket.create_connection(url, sslopt={'context' : ssl_context}) #sslopt={"cert_reqs": ssl.CERT_NONE})
		print(ws)

		ws.send(json.dumps({'action': 'set', 'key': uuid, 'value': data}, separators=(',', ':')))
	except Exception as e:
		print("oops", e)


def main(request, response):
    query = parse_qs(urlparse(request.url).query)
    print(query)
    if 't' in query and query['t'][0] == 'csp':
        csp = request.headers.get(b'content-security-policy', None)
        response.headers.append(b"Content-Type", b"text/html; charset=utf-8")
        # allow the script to read the response text
        response.headers.append(b'Access-Control-Allow-Origin:', b'*')
        # reply with the csp in the body
        response.content = csp
        response.code = 200
