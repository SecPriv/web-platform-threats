# WPT Security

Web Platform Threats: Automated Detection of Web Security Issues With WPT.

## Repo Structure

```
├── docs
│   └── report.pdf          Extended version of the USENIX'24 paper
├── instrumentation/        Browser Instrumentation used by the runner
│   ├── extension/          Extension for tracing browser actions
│   │   ├── chromium/       Chromium extension
│   │   ├── firefox/        Firefox extension
│   │   └── safari/         Safari extension
│   └── proxy/              Proxy for logging requests/responses
├── kubernetes/             Kubernetes definitions to deploy the pipeline in a cluster
├── results/                Results of running the pipeline (USENIX'24 Paper)
│   ├── wpt/                Verification results of the WPT suite (Sec. 5.1)
│   └── beyond_wpt/         Verification results of the separate test suite of Sec. 5.3
├── runner/                 Docker container for running a browser and producing a trace
│   └── safari/             Safari-specific Docker container
├── trace_matching/         Trace parser and SMT-LIB generator
│   ├── trace_matching.smt  Definition of datatypes and invariants in SMT-LIB format
│   ...
├── Makefile                Makefile for automating common tasks
├── README.md               This file
└── wpt-check               Local pipeline executable
```

## Usage

WPT Security requires a working [docker](https://docker.io) installation, the bash shell and GNU make.

### Download or build the pipeline docker images

```sh
./wpt-check pull    # or make pull
```

The command downloads three images from the github docker registry
- `wpt-runner` Browser runner for Chromium and Firefox
- `wpt-safari-runner` Browser runner for Safari (Note: this image does not include MacOS, which needs to be run separately)
- `wpt-trace-matching` Trace verifier, including trace parsing and Z3

Alternatively, the images can be built locally with the following command.

```sh
./wpt-check build   # or make
```

Note that the build **may take up to 30 minutes**, as a specific version of z3 is compiled from source.

### Verify all Web Invariants on the execution of WPT tests

```sh
./wpt-check run firefox '/cookies/secure/set-from-dom.https.sub.html'
```

The script runs the instrumented `firefox` browser, converts the execution trace to SMT-LIB, and verifies it with the Z3 theorem prover.
The verification results are printed on standard output for every subtest and invariant:
- `unsat`: the invariant is valid;
- `sat`: the invariant does not hold, i.e., Z3 could find a counterexample.


The `wpt-check` script supports running Firefox and Chromium. 
For Safari, refer to the [runner/safari/README.md](runner/safari/README.md) file for a guide on (i) how to install the Safari instrumentation on a MacOS system, and (ii) generate an execution trace in JSON format.
The JSON trace can be verified using the `wpt-check verify` command, as described in the following.

### Verify all Web Invariants on an execution trace in JSON format

```sh
./wpt-check verify safari_1710427255.json
```

The sript converts the trace to SMT-LIB and verifies it using Z3.
The output is printed in the same format as the `wpt-check run` command.
