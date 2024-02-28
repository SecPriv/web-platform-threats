FROM ubuntu:20.04
RUN apt-get update && apt-get install -y openssh-client redis-tools netcat && rm -rf /var/lib/apt/lists/*
COPY ./runner.sh /runner.sh
COPY id_ed25519 /id_ed25519
CMD ["/runner.sh"]
