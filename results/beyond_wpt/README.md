# Comprehensiveness of Tests

This folder includes the additional tests presented in Sec. 5.3 (see [../../docs/report.pdf](../../docs/report.pdf)), together with the traces containing the expected violations.

## Validation of the results

The results can be validated using the following command:
```sh
./check-results.py
```

The script will execute the new tests using an instrumented browser and verify that all SAT results of Table 5 are still SAT.
For each test the script will print the invariant, the browser, the expected and obtained result as shown in the following.
```
[webspec_host_frames - firefox] ⚙ running verifier/launcher.html...
[webspec_host_frames - firefox] ✔ host-invariant expected: "sat" got: "sat"
[cookie-crumbles - firefox] ⚙ running /crumbles/tossing.sub.https.html...
[cookie-crumbles - firefox] ✔ cookie-serialization-invariant expected: "sat" got: "sat"
...
```

The script does not support any command line argument and will run a new browser instance for every execution except for Safari, where the trace collected during the paper experiments is verified.

## Directory Structure

```
├── cookie-crumbles/
├── localhost/
├── multi_nested_frames/
├── webspec_csp_blob/
├── webspec_csp_sop/
├── webspec_csp_sw/
├── webspec_host_frames/
├── webspec_tt_frames/
├── check-results.py*
└── README.md
```

Every subfolder includes a `traces` folder containing the execution trace in JSON format where a violation was discovered.
