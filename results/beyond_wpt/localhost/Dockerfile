FROM ghcr.io/secpriv/wpt-runner:latest 

FROM node:18 as ext_builder
USER root
RUN npm install -g web-ext
ADD ./instrumentation/extension/firefox /extension
WORKDIR /extension/
# Patch Firefox extension and rebuild it
RUN sed -i "s/'web-platform.test'/'localhost'/" ./ext.js
RUN sed -i 's#//.*$##g' manifest.json
RUN web-ext build

FROM ghcr.io/secpriv/wpt-runner:latest

USER root

# Add new test
ADD results/beyond_wpt/localhost/test/localhost_cookies.html /home/test/web-platform-tests/cookies/prefix/localhost_cookies.html

# Patch chrome extension
RUN sed -i 's/web-platform.test/localhost/' /home/test/extension_chromium/ext.js
# Patch wpt to use "localhost" has the main host instead of "web-platform.test"
RUN sed -i 's/"browser_host": "web-platform.test",/"browser_host": "localhost",/' /home/test/web-platform-tests/tools/serve/serve.py
User test

# Add Firefox Extension
COPY --from=ext_builder /extension/web-ext-artifacts/chrome_webrequest_test-0.1.zip /home/test/chrome_webrequest_test-0.1.zip
ENV FIREFOX_EXTENSIONS=/home/test/chrome_webrequest_test-0.1.zip

