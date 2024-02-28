FROM ghcr.io/secpriv/wpt-runner:latest
USER root
RUN sed -i 's/export HTTP_PROXY=.*//' /sbin/docker-entrypoint.sh
USER test
COPY ./crumbles /home/test/web-platform-tests/crumbles


