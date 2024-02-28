# Verification of WPT Violations

This directory contains the traces and Z3 outputs collected by running the trace verification pipeline on Chromium, Firefox and Safari, and the tools to validate the SAT results of Table 2, Sec 5.1 (see [../../docs/report.pdf](../../docs/report.pdf)).

## Directory Structure

```
├── chromium/                             Results of running the pipeline (Sec 5.1) - Chromium 
│   ├── cookie-serialization-invariant/   Execution traces (:json) and corresponding Z3 outputs (:z3) deemded SAT 
│   ...
├── firefox/                              Results of running the pipeline (Sec 5.1) - Firefox
│   ├── blockable-mixed-content-filtered/ Execution traces (:json) and corresponding Z3 outputs (:z3) deemded SAT 
│   ...
├── safari/                               Results of running the pipeline (Sec 5.1) - Safari 
│   ├── blockable-mixed-content-filtered/ Execution traces (:json) and corresponding Z3 outputs (:z3) deemded SAT
│   ...
├── check-results.py        Local experiment runner
├── results.json            JSON summary of the results (Sec 5.1) keyed by [browser][invariant][test]
└── README.md               This file
```

## Usage

These experiments requires Python 3, the `wpt-check` tool (see [../../README.md](../../README.md)), and the runner `wpt-runner:latest` and `wpt-trace-matching:latest` docker containers, which can be built or downloaded using `wpt-check`.

The `check-results.py` script automates the execution of the experiments, and can be called with the following arguments:

```
usage: check-results.py [-h] [-b BROWSER] [-i INVARIANT] [--no-runner]

options:
  -h, --help            show this help message and exit
  -b BROWSER, --browser BROWSER
  -i INVARIANT, --invariant INVARIANT
  --no-runner
```

- The `-b` option specifies which browser should be targeted. Omitting this option will perform the experiments for Chromium, Firefox and Safari listed in the `results.json` file.
- The `-i` option specifies which invariant should be checked. Omitting this option will check all invariants listed in the `results.json` file.
- The `--no-runner` option skips the excution of the runner. The traces under `./<browser>/<invariant>/`, listed in the `results.json` file, are validated instead.

### Examples
Execute the WPT tests listed under `[firefox][cookie-serialization-invariant]` in the `results.json` file using the instrumented Firefox to collect execution traces, and check whether the invariant `cookie-serialization-invariant` is SAT for all traces.
```sh
./check-results.py -b firefox -i cookie-serialization-invariant
```

Check if `cookie-serialization-invariant` is SAT for all traces under `<browser>/cookie-serialization-invariant`, for all browsers 

```sh
./check-results.py -i cookie-serialization-invariant --no-runner
```

Run all tests using all browsers specified in the `results.json` file and check all invariants for SAT.  

```sh
./check-results.py 
```

## Result Verification (Sec 5.1)
To verify the results of Table 2, Sec 5.1 of the paper execute the following:

```sh
./check-results.py
```

Alternatively, run the experiments for each browser individually:
```sh
./check-results.py -b <browser> 
```

> Important: Make sure to build or pull the required docker containers using `<project_root>/wpt-check pull` or `<project_root>/wpt-check build`


Note: Since we do not distribute an instrumented Safari runner, the Safari experiments in this directory test the provided traces for invariant violation, instead of collecting new traces. This can also be done for Chromium and Firefox by specifying the `--no-runner` option to `check-results.py`. See [../../runner/safari/README.md](../../runner/safari/README.md) for instructions on how to collect execution traces from Safari.


### Expected results
```
🌍 chromium
cookie-serialization-invariant: 15/15 SAT
samesite-cookies-confidentiality: 10/10 SAT

🌍 firefox
cookie-serialization-invariant: 9/9 SAT
upgradeable-mixed-content-filtered: 18/18 SAT
blockable-mixed-content-filtered: 24/24 SAT
samesite-cookies-integrity: 1/1 SAT
samesite-cookies-confidentiality: 6/6 SAT

🌍 safari
blockable-mixed-content-filtered: 21/21 SAT
```
