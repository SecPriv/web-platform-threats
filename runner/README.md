# WPT Runner

Docker container for executing WPT tests with an instrumented browser.

## Updating Firefox

To use the last stable version, comment line 24 of Dockerfile and rebuild the container.

## Updating Chromium

To use the last stable version, remove the `--revision` argument from line 26 of the Dockerfile

## Updating WPT

Change the repository URL passed to `start.sh` in line 18 of the Dockerfile. Note that some modifications are required for the instrumentation to detect the start and end of each test. These changes can be obtained by comparing the `wpt-security` branch of the https://github.com/SecPriv/wpt repo with the upstream `main`.

## Safari

Refer to [safari/README.md](safari/README.md).
