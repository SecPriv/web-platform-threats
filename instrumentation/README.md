# Browser Instrumentation

Our browser instrumentation combines a browser extension with a network proxy in order to sidstep the limitations of the extension API with respect to their ability to inspect network traffic.

## Extension

The `extension` folder includes the source code of the browser extension for each of the three major browsers: Chromium, Firefox and Safari.
As the three browsers implement the Web extension API with some minor differences, the code requires minor browser-specific modifications.

## Proxy

The network proxy is implemented as a plugin for the [mitmproxy](https://mitmproxy.org) Python proxy.
It logs the requests and responses it intercepts in a JSON file that can be downloaded by sending a GET request to the `http://b1v253vpqwutupjwntfmpc9a4m94s5zm.mitm.it/download-trace` non-existent URL, so that the extension can fetch it and merge it with the logged execution trace.
