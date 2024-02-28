
from wptserve.utils import isomorphic_decode, isomorphic_encode
from wptserve.handlers import handler
from wptserve.router import Router
from urllib.parse import parse_qs
from urllib.parse import urlparse
import json
from filelock import FileLock
import ssl, websocket, asyncio, threading, pickle
import os

REPORT_UUID = 'b28a182a-b4c1-47ac-93a7-9667c56bf641'
DATA_FILE = "verifier/csp_state.dat"
LOCK_FILE = "verifier/lock"
LOCK = FileLock(LOCK_FILE)


def update_globals(parsed_req):
    global GLOBALS

    for g in GLOBALS.keys():
        if g in parsed_req:
            GLOBALS[g] = parsed_req[g][0]

"""
TODO:
1. get response
2. create filename (method.proto.domain.port.corr)
  - filter the domain until ".test"
3. parse content by "\n\n" to separate the header from the body (should work, if not, adapt the verifier)
4. split each header by ":" and add response header
"""

def stash_add(uuid, data):
	try:
		url = "wss://web-platform.test:8666/stash_responder_blocking"
		ssl_context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
		# localhost_pem = pathlib.Path().with_name("localhost.pem")
		cert_path = "../tools/certs/cacert.pem"
		ssl_context.load_verify_locations(cert_path)
		# print(dir(ssl_context))
		ws = websocket.create_connection(url, sslopt={'context' : ssl_context}) #sslopt={"cert_reqs": ssl.CERT_NONE})
		# ws = websocket.create_connection(url, sslopt={"cert_reqs": ssl.CERT_NONE})
		print(ws)

		ws.send(json.dumps({'action': 'set', 'key': uuid, 'value': data}, separators=(',', ':')))

	except Exception as e:
		print("oops", e)


def caulculate_filename(request, allowed=1):
    query = parse_qs(urlparse(request.url).query)

    domain = request.url_parts.netloc.split(":")[0]
    port = int(request.url_parts.netloc.split(":")[1])
    scheme = request.url_parts.scheme
    resource = None

    filename = f"{request.method}.{scheme}.{domain}.{port}.{allowed}"

    return filename

def reset_data_file():
	with open(DATA_FILE, "wb") as f:
		pickle.dump({'0' : {str(k) : "P" for k in range(5)} , 'counter': 0}, f)


def main(request, response):
	with LOCK:
		if not os.path.exists(DATA_FILE):
			reset_data_file()

		test = '0'
		with open(DATA_FILE, "rb") as f:
			test_results = pickle.load(f)
		
		test_results['counter'] += 1

		print(request.method, request.url)
		print(test_results)

		if request.method == 'POST':
			print("[BLOCKED]", request.body)

		elif request.method == 'GET':
			query = parse_qs(urlparse(request.url).query)
			order = query.get('order', None)

			if order and ( order[0] in test_results[test] ):
				test_results[test][order[0]] = 'G'

		with open(DATA_FILE, "wb") as f:
			pickle.dump(test_results, f)

		if test_results['counter'] == 5:
			# write to stash
			sorted_keys = sorted(test_results[test].keys())
			csp_signature = ''.join([test_results[test][k] for k in sorted_keys])
			os.remove(DATA_FILE)
			stash_add(REPORT_UUID, csp_signature)

	# query = parse_qs(urlparse(request.url).query)
	# print(query)
	# if 'allowed' in query and query['allowed'][0] == '1':
	# 	print("GOT A NORMAL ONE", request)
	# 	# fname = caulculate_filename(request, allowed=1)
	# 	# print("FILENAME", fname)
	# 	with open(f"verifier/responses/csp_test", "r") as f:
	# 		uuid = f.read().strip()

	# 	stash_add(uuid, "allowed")

	# elif request.method == 'POST' and 'allowed' in query and query['allowed'][0] == '0':
	# 	print("GOT A DISALLOWED", request)
	# 	# filename = caulculate_filename(request, allowed=0)
	# 	# print("FILENAAME", filename)

	# 	with open(f"verifier/responses/csp_test", "r") as f:
	# 		uuid = f.read().strip()

	# 	stash_add(uuid, "disallowed")
