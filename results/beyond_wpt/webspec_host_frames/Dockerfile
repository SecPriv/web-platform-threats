FROM ghcr.io/secpriv/wpt-runner:latest

FROM node:18 as ext_builder
USER root

RUN npm install -g web-ext
ADD ./instrumentation/extension/firefox /extension
ADD ./results/beyond_wpt/webspec_host_frames/extension/ /extension/

WORKDIR /extension/
RUN sed -i 's#//.*$##g' manifest.json
RUN web-ext build

FROM ghcr.io/secpriv/wpt-runner:latest

USER root
ADD ./results/beyond_wpt/webspec_host_frames/extension_chromium/ /home/test/extension_chromium/

WORKDIR /home/test/web-platform-tests
ADD ./results/beyond_wpt/webspec_host_frames/verifier verifier

# change the websocket port for the Stash to work
run sed -i 's/"ws": \[8888\],/"ws": \[8665\],/' /home/test/web-platform-tests/tools/wptrunner/wptrunner/environment.py
run sed -i 's/"wss": \[8889\],/"wss": \[8666\],/' /home/test/web-platform-tests/tools/wptrunner/wptrunner/environment.py
run echo "websocket-client" >> tools/wptrunner/requirements.txt

USER test

# Add Firefox Extension
COPY --from=ext_builder /extension/web-ext-artifacts/chrome_webrequest_test-0.1.zip /home/test/chrome_webrequest_test-0.1.zip
ENV FIREFOX_EXTENSIONS=/home/test/chrome_webrequest_test-0.1.zip
