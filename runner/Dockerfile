FROM node:18 as ext_builder
RUN npm install -g web-ext
COPY ./instrumentation/extension/firefox/ /extension/firefox/
WORKDIR /extension/firefox
# JSON does not support comments!
RUN sed -i 's#//.*$##g' manifest.json
RUN web-ext build

FROM webplatformtests/wpt:0.52 as base
ENV PIP_DISABLE_PIP_VERSION_CHECK=1
# Dependencies
USER root
RUN apt-get update && apt-get install -y inotify-tools && rm -rf /var/lib/apt/lists/*
RUN curl -o /tmp/mitmproxy-9.0.1-linux.tar.gz https://snapshots.mitmproxy.org/9.0.1/mitmproxy-9.0.1-linux.tar.gz && tar -x -C /usr/local/bin/ -f /tmp/mitmproxy-9.0.1-linux.tar.gz && chmod +x /usr/local/bin/mitm* && rm /tmp/mitmproxy-9.0.1-linux.tar.gz
USER test
# Install WPT
RUN sed -i 's/depth=50/depth=1/' start.sh
RUN ./start.sh https://github.com/SecPriv/wpt.git wpt-security
ADD ./runner/firefox_addons.patch ./
WORKDIR web-platform-tests
RUN git apply ~/firefox_addons.patch
# Init WPT and install Browsers
RUN ./wpt run --help >/dev/null
RUN sed -i 's/firefox-latest-ssl/firefox-116.0.3-ssl/g' /home/test/web-platform-tests/tools/wpt/browser.py
RUN ./wpt install --channel stable firefox browser && ./wpt install --channel stable firefox webdriver
RUN ./wpt install --channel stable chromium browser --revision 1185653 && ./wpt install --channel stable chromium webdriver --revision 1185653
ENV CHROMIUM_BINARY=/home/test/web-platform-tests/_venv3/browsers/stable/chrome-linux/chrome
ENV CHROMIUM_DRIVER=/home/test/web-platform-tests/_venv3/bin/chromium/chromedriver
ENV FIREFOX_BINARY=/home/test/web-platform-tests/_venv3/browsers/stable/firefox/firefox
RUN ./wpt manifest

# Add Firefox Extension
COPY ./instrumentation/extension/firefox/ /home/test/extension_firefox
COPY --from=ext_builder /extension/firefox/web-ext-artifacts/chrome_webrequest_test-0.1.zip /home/test/chrome_webrequest_test-0.1.zip
ENV FIREFOX_EXTENSIONS=/home/test/chrome_webrequest_test-0.1.zip

# Add Chrome Extension
COPY ./instrumentation/extension/chromium/ /home/test/extension_chromium
ENV CHROME_EXTENSIONS=/home/test/extension_chromium

# Add Proxy script
COPY ./instrumentation/proxy/log_addon.py /home/test/log_addon.py

# Entrypoint
USER root
RUN apt-get update && apt-get install -y redis-tools && rm -rf /var/lib/apt/lists/*
ADD ./runner/docker-entrypoint.sh /sbin
RUN chmod +x /sbin/docker-entrypoint.sh
USER test
VOLUME /home/test/Downloads/
ENTRYPOINT ["/sbin/docker-entrypoint.sh"]
