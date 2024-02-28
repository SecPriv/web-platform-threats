FROM ghcr.io/secpriv/wpt-runner:latest

USER root

WORKDIR /home/test/web-platform-tests
ADD ./verifier verifier

# change the websocket port for the Stash to work
RUN sed -i 's/"ws": \[8888\],/"ws": \[8665\],/' /home/test/web-platform-tests/tools/wptrunner/wptrunner/environment.py
RUN sed -i 's/"wss": \[8889\],/"wss": \[8666\],/' /home/test/web-platform-tests/tools/wptrunner/wptrunner/environment.py

# add HTTP ports used in test to the HTTP list
# RUN sed -i 's/"http": \[8000, "auto"\],/"http": [8000, "auto", 1025\],/' /home/test/web-platform-tests/tools/serve/serve.py
RUN sed -i 's/"https": \[8443, 8444\],/"https": [1024, 1025\],/' /home/test/web-platform-tests/tools/serve/serve.py
RUN sed -i 's/"https": \[8443, 8444\],/"https": [1024, 1025\],/' /home/test/web-platform-tests/tools/wptrunner/wptrunner/environment.py

run echo "websocket-client" >> tools/wptrunner/requirements.txt

USER test
