#!/usr/bin/env python3

from mitmproxy import http
import logging
import json, time, base64

class TraceHTTP:

    def __init__(self):
        self.req = []
        self.resp = []
        self.ignore_list = ["detectportal.firefox.com"]

    def configure(self, updated):
        pass

    def request(self, flow: http.HTTPFlow) -> None:
        # here flow.response is None

        # To get the body in plain text we might need to remove the encoding gzip
        flow.request.headers['Accept-Encoding'] = 'identity'

        if (flow.request.host == 'b1v253vpqwutupjwntfmpc9a4m94s5zm.mitm.it' and
            flow.request.path == '/download-trace'):

            content = json.dumps({'requests': self.req, 'responses': self.resp}).encode('utf-8')
            # for now clear the data
            self.req = []
            self.resp = []

            flow.response = http.Response.make(
                200,  # (optional) status code
                content,  # (optional) content
                {"Content-Type": "application/json",
                 "Access-Control-Allow-Origin" : "*"},  # (optional) headers
            )
        elif flow.request.host not in self.ignore_list:
            self.req.append({'url': flow.request.pretty_url,
                             'request_start': flow.request.timestamp_start,
                             'request_end': flow.request.timestamp_end,
                             'ts': time.time(),
                             'body': base64.b64encode(flow.request.get_content()).decode('utf-8')})

    def response(self, flow: http.HTTPFlow) -> None:
        if (flow.request.host not in self.ignore_list and
            flow.request.host != 'b1v253vpqwutupjwntfmpc9a4m94s5zm.mitm.it' and
            flow.request.path !='/download-trace'):

            self.resp.append({'url': flow.request.pretty_url,
                              'request_start': flow.request.timestamp_start,
                              'response_start': flow.response.timestamp_start,
                              'request_end': flow.request.timestamp_end,
                              'response_end': flow.response.timestamp_end,
                              'ts': time.time(),
                              'body': base64.b64encode(flow.response.get_content()).decode('utf-8')})

addons = [
    TraceHTTP()
]
