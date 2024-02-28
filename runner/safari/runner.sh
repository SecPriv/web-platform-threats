#!/usr/bin/env bash
set -euo pipefail

# REDIS_URL, TIMEOUT

BROWSER=safari

SSH_ENDPOINT=localhost
SSH_PORT=10022
SSH_ID=id_ed25519

if [[ -z "${SSH_USER+x}" ]]; then
    SSH_USER=user
fi


while true; do
    timeout 10s nc -w 1 "${SSH_ENDPOINT}" "${SSH_PORT}" -vvv | grep -i ssh && break
    sleep 1
done

# execute safari and enable the extension
ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p "${SSH_PORT}" -i "${SSH_ID}" "${SSH_USER}@${SSH_ENDPOINT}" "bash -s" <<EOF
  while true; do 
        pkill Safari
        sleep 5
        automator ./unsigned-extensions.workflow && break
        sleep 1
  done
EOF
# needs the workflow, the extension and safaridriver
# see https://developer.apple.com/documentation/safariservices/safari_web_extensions/converting_a_web_extension_for_safari
#    xcode-select -r
#    xcrun safari-web-extension-converter --macos-only ./extension_safari
# safaridriver --enable

run_safari() {
    local test_name=$1
    ( ( ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p "${SSH_PORT}" -i "${SSH_ID}" "${SSH_USER}@${SSH_ENDPOINT}" "bash -s" <<EOF
        /usr/local/bin/fswatch -1 -o ~/Downloads | xargs -n1 -I{} sh -c "pkill -f wpt fswatch; sleep 2; pkill -f wpt safaridriver Python; ls -lah ~/Downloads"
EOF
) || true ) & 
    ( ( timeout $TIMEOUT ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p "${SSH_PORT}" -t -i "${SSH_ID}" "${SSH_USER}@${SSH_ENDPOINT}" "bash -s" <<EOF
        export PATH=\$PATH:/Users/${SSH_USER}/Library/Python/3.9/bin
        export PATH=/Library/Frameworks/Python.framework/Versions/3.11/bin:\$PATH
        cd ~/wpt
        time ./wpt run  --yes \
                 --log-mach-level=debug \
                 --log-mach=- \
                 --log-wptreport=/dev/stderr \
                 --pause-after-test \
                 --channel stable \
                  safari ${test_name}
        echo DONE
        exit
EOF
) || true )
}

while true; do
    TEST_NAME="$(redis-cli -u "$REDIS_URL"  --raw BLPOP "runner:${BROWSER}" 0 | tail -n1 | tr -d '\n')"
    redis-cli  -u "$REDIS_URL" --raw RPUSH runner:wip "${BROWSER}:${TEST_NAME}"
    redis-cli -u "$REDIS_URL" --raw SETEX "lease:runner:${BROWSER}:${TEST_NAME}" $TIMEOUT 1

    START_TIME="$(date +%s.%N)"

    # execute test via ssh
    run_safari "$TEST_NAME"

    DURATION="$(awk -v "start_time=${START_TIME}" -v "end_time=$(date +%s.%N)"  'BEGIN { print end_time - start_time }')"
    
    KEY_NAME="${BROWSER}:${TEST_NAME}:$(date +%s)"
    echo $KEY_NAME
    ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p "${SSH_PORT}" -i "${SSH_ID}" "${SSH_USER}@${SSH_ENDPOINT}" "cat ~/Downloads/Unknown" | redis-cli -u  "$REDIS_URL" --raw -x SET "results:${KEY_NAME}:json"
    ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p "${SSH_PORT}" -i "${SSH_ID}" "${SSH_USER}@${SSH_ENDPOINT}" "rm ~/Downloads/Unknown*"
    redis-cli -u "$REDIS_URL" --raw SET "results:${KEY_NAME}:runner-time" "${DURATION}"
    redis-cli -u "$REDIS_URL" --raw RPUSH analyzer "$KEY_NAME"
    redis-cli -u "$REDIS_URL" --raw LREM runner:wip 1 "${BROWSER}:${TEST_NAME}"
done
