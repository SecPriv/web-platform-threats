from wptserve.utils import isomorphic_decode, isomorphic_encode
from urllib.parse import parse_qs
from urllib.parse import urlparse
import json, os
import ssl, websocket, asyncio


GLOBALS = {'uuid' : "", "stash" : ""}

def update_globals(parsed_req):
    global GLOBALS

    for g in GLOBALS.keys():
        if g in parsed_req:
            GLOBALS[g] = parsed_req[g][0]

def calculate_sw_filename(request):
    query = parse_qs(urlparse(request.url).query)

    domain = request.url_parts.netloc.split(":")[0]
    port = int(request.url_parts.netloc.split(":")[1])
    scheme = request.url_parts.scheme

    filename = f"{scheme}.{domain}.{port}_sw"

    return filename

def caulculate_filename(request):
    query = parse_qs(urlparse(request.url).query)

    domain = request.url_parts.netloc.split(":")[0]
    port = int(request.url_parts.netloc.split(":")[1])
    scheme = request.url_parts.scheme
    resource = None
    try :
      resource = query['res'][0]
    except:
      pass

    filename = f"{request.method}.{scheme}.{domain}.{port}.{resource if resource else ''}"


    return filename

def build_response(response, headers, body):
  headers = json.loads(headers)

  response.code = int(headers['code'])

  for k in headers['headers']:
    if headers['headers'][k]:
      response.headers.append(k.encode(), headers['headers'][k].encode())
  
  response.content = body
  
  return response

def stash_add(uuid, data):
	print("STASH ADD")
	try:
		url = "wss://web-platform.test:8666/stash_responder_blocking"
		ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
		print("loading cert!", os.getcwd())
		cert_path = "/home/pedro/wip/wpt-webspec/tools/certs/cacert.pem"
		ssl_context.load_verify_locations(cert_path)
		ws = websocket.create_connection(url, sslopt={'context' : ssl_context}) #sslopt={"cert_reqs": ssl.CERT_NONE})
		print("CREATED WS", ws)

		ws.send(json.dumps({'action': 'set', 'key': uuid, 'value': data}, separators=(',', ':')))

	except Exception as e:
		print("oops", e)


def main(request, response):
	print("request", request)
	
	query = parse_qs(urlparse(request.url).query)
	if 'sw' in query:
		# asking for the service worker registratin
		filename = calculate_sw_filename(request)
		print("IN A SW REQUEST", filename)
		with open(f"verifier/responses/{filename}.html", "r") as f:
			response.content = f.read()

		response.headers.append(b"Content-Type", b"text/html; charset=utf-8")
		return

	filename = caulculate_filename(request)
	print("fname", filename)

	headers = None
	body = None
	try:
		with open(f"verifier/responses/{filename}.headers", "r") as f:
			headers = f.read()

		with open(f"verifier/responses/{filename}.body", "r") as f:
			body = f.read()

		with open(f"verifier/responses/{filename}.ver", "r") as f:
			uuid = f.read().strip()

		build_response(response, headers, body)

		try:
			stash_add(uuid, filename)
		except Exception as e:
			print("aaah...", e)

		return
	except:
		response.code = 500
