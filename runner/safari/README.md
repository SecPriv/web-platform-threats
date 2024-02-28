# Safari WPT Runner

For licensing reasons, we cannot redistribute a MacOS VM with Safari. 
We provide in the following the instructions to install our browser instrumentation on Safari. These steps have been tested on MacOS Ventura 13.3.1, running Safari 16.4 (18615.1.26.11.23).

- Install the latest version of XCode and generate an XCode project based on the Web Extension in this repository
  ```sh
  xcode-select -r
  xcrun safari-web-extension-converter --macos-only ./instrumentation/extension/safari
  ```
  After compiling and executing the resulting project, the extension can be loaded in Safari.
- Enable *Unsigned Extensions* in the Developer Tools of Safari.
  - Enable the Develop menu by selecting, from the menu bar, "Settings", then "Advanced" and "Show Develop menu in menu bar"
  - Select "Allow Unsigned Extensions" from the Develop menu. This step must be repeated every time Safari is restarted.
  - Alternatively, after enabling the Develop menu, the [unsigned-extensions.workflow](unsigned-extensions.workflow) Automator script can be used to automate the click on "Allow Unsigned Extensions". Note that this step requires an admin password that needs to be changed in the workflow.
- In Safari, click "Settings" from the menu bar, then "Extensions", select the "Chrome webrequest test" extension, click on "Edit Websites..." and select "For other websites" "Allow". This setting allows the extension to read the content of all websites.
- In Safari, click "Settings" from the menu bar, then "Websites", select "Downloads" and "When visiting other websites" "Allow". This setting disables the confirmation dialog when the extension will save (download) the execution trace.
- Clone the WPT repository in the home folder.
  ```sh
  cd ~/
  git clone --depth 1 https://github.com/SecPriv/wpt.git -b wpt-security
  ```
- Install virtualenv using pip
  ```sh
  pip3 install --user virtualenv
  ```
- [Optional, required for kubernetes] Install the `fswatch` utility using Brew to enable the runner container to monitor the test execution.
  ```sh
  brew install fswatch
  ```
- Enable `safaridriver` to allow WPT to control the browser.
  ```sh
  safaridriver --enable
  ```

## Obtaining and verifyng an execution trace

When the extension is installed, enabled and authorized in Safari, run a test using the following commands
```sh
export PATH=\$PATH:/Users/user/Library/Python/3.9/bin
export PATH=/Library/Frameworks/Python.framework/Versions/3.11/bin:\$PATH

cd ~/wpt
time ./wpt run --yes \
               --log-mach-level=debug \
               --log-mach=- \
               --log-wptreport=/dev/stderr \
               --pause-after-test \
               --channel stable \
                safari '/cookies/secure/set-from-dom.https.sub.html'
```

Upon completion the execution trace is automatically downloaded to the Downloads folder with the `Unknwon` name.
The file can then be checked using the `wpt-check verify` command from any Linux machine.

## Executing the runner with Kubernetes

Refer to the [../../kubernetes/README.md](../../kubernetes/README.md)  file.
