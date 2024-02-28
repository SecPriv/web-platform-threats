#!/usr/bin/env bash
set -euo pipefail

BROWSER="$1"
shift

# Make the traces volume rw by all
sudo chmod a+rw /home/test/Downloads

# Add hosts
./wpt make-hosts-file | sudo tee -a /etc/hosts >/dev/null
# Disable cert verification
sudo rm -f /usr/bin/certutil

if [[ "$BROWSER" == "list-tests" ]]; then
    ./wpt run --yes --list-tests --channel stable firefox "$@" --test-types testharness | sed 's/\?.*$//' > /tmp/firefox
    ./wpt run --yes --list-tests --channel stable chrome "$@" --binary $CHROMIUM_BINARY --test-types testharness | sed 's/\?.*$//' > /tmp/chrome
    cat /tmp/{firefox,chrome} | sort | uniq
    exit 0
fi

setup () {
    # Kill the browser when the trace is saved to the Download folder
    ( inotifywait -e modify /home/test/Downloads/ && sleep 5 && pkill -f "$BROWSER" ) &

    ( mitmdump -s  /home/test/log_addon.py -v -k --listen-host 0 --listen-port 65480 --ssl-insecure ) &
    export HTTP_PROXY=http://127.0.0.1:65480
}

case "$BROWSER" in
    firefox)
        setup
        xvfb-run --auto-servernum \
        ./wpt run --yes \
                 --log-mach-level=debug \
                 --log-mach=- \
                 --log-wptreport=/dev/stderr \
                 --channel stable \
                 --binary $FIREFOX_BINARY \
                 --no-headless \
                 firefox "$@"
    ;;
    chromium)
        setup
        xvfb-run --auto-servernum \
        ./wpt run --yes \
                 --log-mach-level=debug \
                 --log-mach=- \
                 --log-wptreport=/dev/stderr \
                 --channel stable \
                 --webdriver-binary $CHROMIUM_DRIVER \
                 --binary $CHROMIUM_BINARY \
                 --binary-arg no-sandbox \
                 --binary-arg ignore-certificate-errors \
                 --binary-arg "load-extension=$CHROME_EXTENSIONS" \
                 --binary-arg "proxy-server=http://127.0.0.1:65480" \
                 chrome "$@"
    ;;
    *)
        echo Supported Browsers: firefox, chromium.
    ;;
esac
