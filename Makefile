.PHONY: build clean runner runner-safari trace-matching push pull deploy
SHELL=/usr/bin/env bash

REGISTRY=ghcr.io/secpriv

all: build

runner: instrumentation/extension instrumentation/proxy runner/
	docker compose -f runner/docker-compose.yaml build
	docker tag wpt-runner:latest $(REGISTRY)/wpt-runner:latest

runner-safari: runner/safari
	cd runner/safari; docker build -t $(REGISTRY)/wpt-safari-runner:latest .

trace-matching: trace_matching/
	docker run --rm -i -v `pwd`/trace_matching:/mnt:ro --workdir /tmp nixos/nix sh -c "nix-build /mnt/default.nix -A docker 1>&2 && cat result" | docker load
	docker tag wpt-trace-matching:latest $(REGISTRY)/wpt-trace-matching:latest

build: runner runner-safari trace-matching
push:
	docker push $(REGISTRY)/wpt-runner:latest
	docker push $(REGISTRY)/wpt-safari-runner:latest
	docker push $(REGISTRY)/wpt-trace-matching:latest
pull:
	docker pull $(REGISTRY)/wpt-runner:latest
	docker pull $(REGISTRY)/wpt-safari-runner:latest
	docker pull $(REGISTRY)/wpt-trace-matching:latest

deploy: kubernetes/pipeline.yaml kubernetes/safari_runner.yaml
	kubectl apply -f kubernetes/pipeline.yaml kubernetes/safari_runner.yaml


clean:
	docker rmi wpt-runner:latest wpt-trace-matching:latest
	docker rmi $(REGISTRY)/wpt-runner:latest $(REGISTRY)/wpt-safari-runner:latest $(REGISTRY)/wpt-trace-matching:latest
