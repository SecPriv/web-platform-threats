#!/usr/bin/env python3


import os
import time
import subprocess
import traceback
import json
import re
import tempfile
import shutil

RED = 31
GREEN = 32
BLUE = 34


tests = {
    "cookie-crumbles": {"browsers": ["firefox", "chromium"],
                        "tests": ["/crumbles/tossing.sub.https.html"],
                        "expected": {"firefox": ["cookie-serialization-invariant"], "chromium": ["cookie-serialization-invariant"]}},
    "localhost": {"browsers": ["firefox", "chromium"],
                  "tests": ["cookies/prefix/localhost_cookies.html"],
                  "expected": {"chromium": ["secure-cookies-invariant"]},
                  "rerun-on-failure": True},
    "multi_nested_frames": {"browsers": ["firefox", "chromium", "safari"],
                            "tests": ["/test/multi_nested_frames.sub.html"],
                            "expected": {"firefox": ["blockable-mixed-content-filtered", "upgradeable-mixed-content-filtered"], "safari": ["blockable-mixed-content-filtered"]}},
    "webspec_host_frames": {"browsers": ["firefox", "chromium"],
                            "tests": ["verifier/launcher.html"],
                            "expected": {"firefox": ["host-invariant"]},
                            "rerun-on-failure": True},
}


def main():
    folder = os.path.dirname(os.path.realpath(__file__))
    for d in os.listdir(folder):
        if not os.path.isdir(d): continue
        if not d in tests: continue
        
        testdir = os.path.join(folder, d)
        for browser in tests[d]['browsers']:
            for test in tests[d]['tests']:
                if browser not in tests[d]['expected']: continue
                should_repeat = True
                while should_repeat:
                    with tempfile.TemporaryDirectory() as tmp:
                        if browser == 'safari':
                            # we use the previous trace as we cannot run/redistribute safari
                            print(f'\033[{BLUE};1m[{d} - {browser}] ⚙ using trace traces/1707930111_safari.json...\033[0m')
                            shutil.copyfile(os.path.join(testdir, "traces/1707930111_safari.json"), os.path.join(tmp, "1707930111_safari.json"))
                        else:
                            print(f'\033[{BLUE};1m[{d} - {browser}] ⚙ running {test}...\033[0m')
                            subprocess.check_output(f'docker compose -f "{testdir}/docker-compose.yml" down', shell=True, stderr=subprocess.DEVNULL)
                            subprocess.check_output(f'docker compose -f "{testdir}/docker-compose.yml" build', shell=True, stderr=subprocess.DEVNULL)
                            subprocess.run(f'docker compose -f "{testdir}/docker-compose.yml" run --rm -v "{tmp}:/home/test/Downloads" runner {browser} {test}', shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
                        json = os.listdir(tmp)[0]
                        res = subprocess.check_output(f'../../wpt-check verify {tmp}/{json}', shell=True, stderr=subprocess.DEVNULL)
                        res = { e.split("\n")[1]: e.split("\n")[2] for e in re.split(r"=+ trace[0-9]+ =+", res.decode('utf-8'))[1:] }
                        should_repeat = False
                        for inv in tests[d]['expected'].get(browser, []):
                            if res[inv] != 'sat' and 'rerun-on-failure' in  tests[d] and  tests[d]['rerun-on-failure']:
                                should_repeat = True
                                time.sleep(5)
                                print('\033[1A\x1b[2K', end='\r')
                            else:
                                print(f'\033[{RED if res[inv] != "sat" else GREEN};1m[{d} - {browser}] {"❌" if res[inv] != "sat" else "✔"} {inv} expected: "sat" got: "{res[inv]}"\033[0m')



if __name__ == '__main__':
    main()
