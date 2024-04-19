#!/usr/bin/env python3

import atexit
import io
import os
import time
import subprocess
import traceback
import json
import re
import argparse
import contextlib

TRIES = 20
LINE_UP = '\033[1A'
LINE_CLEAR = '\x1b[2K'
RED = 31
GREEN = 32
BLUE = 34

is_sat = False

class KillDocker(RuntimeError):
    pass

def rt_parse(proc, invariant):
    global is_sat
    is_sat = False
    next_up = False

    while not is_sat:
        realtime_output = proc.stdout.readline()

        if realtime_output == b'' and (proc.poll() is not None):
            break

        if next_up and realtime_output.strip() == b'sat':
            is_sat = True
            raise KillDocker

        next_up = realtime_output.strip().decode('utf-8') == invariant

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-b', '--browser')
    parser.add_argument('-i', '--invariant')
    parser.add_argument('--no-runner', action='store_true')
    args = parser.parse_args()
    proc = None
    global is_sat 


    results_dir = os.path.dirname(os.path.realpath(__file__))

    with open(os.path.join(results_dir,'results.json')) as f:
        results = json.load(f)

    expected = { br: {i: len(results[br][i]) for i in results[br] } for br in results.keys()}
    current = { br: {i: 0 for i in results[br].keys() } for br in results.keys()}
    container = None

    try:
        if args.browser and args.browser not in results.keys():
            print(f"{args.browser} invalid browser.")
            print(f"Supported browsers: {', '.join(results.keys())}")

        for browser in results.keys():
            if args.browser and browser != args.browser: continue

            print(f"\033[{BLUE};1m🌍 {browser}\033[0m")

            for invariant in results[browser].keys():
                if args.invariant and invariant != args.invariant: continue

                for i, test in enumerate(results[browser][invariant]):
                    print(f"\033[{BLUE};1m⚙ [{i+1}/{len(results[browser][invariant])} - {browser} - sat: {current[browser][invariant]}/{len(results[browser][invariant])}] Checking {invariant}: {test}\033[0m")

                    is_sat = False
                    for _ in range(TRIES):
                        container = f"wpt-check-{time.time()}"
                        try:
                            if browser == "safari" or args.no_runner:
                                lst = os.listdir(os.path.join(results_dir, browser, invariant))
                                trace = [ x for x in lst if x.startswith(test.replace("/","|")) and x.endswith('json') ][0]
                                proc = subprocess.Popen(f"../../wpt-check -n {container} verify '{os.path.join(results_dir, browser, invariant)}/{trace}'", stderr=subprocess.DEVNULL, stdout=subprocess.PIPE, bufsize=0, shell=True)
                                rt_parse(proc, invariant)
                            else:
                                proc = subprocess.Popen(f"../../wpt-check -n {container} run {browser} {test}", stderr=subprocess.DEVNULL, stdout=subprocess.PIPE, bufsize=0, shell=True)
                                rt_parse(proc, invariant)

                        except subprocess.CalledProcessError:
                            traceback.print_exc()
                            continue
                        except KillDocker:
                            subprocess.run(["docker", "kill", container], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)   
                            break


                    current[browser][invariant] += 1 if is_sat else 0

                color = GREEN if current[browser][invariant] == expected[browser][invariant] else RED
                print(f"\r\r\033[{color};1m{invariant}: {current[browser][invariant]}/{expected[browser][invariant]} SAT\033[0m")

    except:
        traceback.print_exc()
        if proc is not None:
            subprocess.run(["docker", "kill", container], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)   
        print(f"\033[{RED};1m❌❌❌ FAILED ❌❌❌ \033[0m")


if __name__ == '__main__':
    main()
