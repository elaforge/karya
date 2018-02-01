#!/usr/bin/env python3
# Copyright 2018 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

# Simple UI to scan the profiling stats as written by profile_verify.py

import sys, os

# Read json results from this directory.
timing_dir = 'prof/timing'

def main():
    scores = set(sys.argv[1:])
    timings = []
    for fn in os.listdir(timing_dir):
        timings.extend(read(os.path.join(timing_dir, fn)))

    timings.sort(key=lambda json: json['patch']['date'])
    for t in timings:
        if not scores or t['score'] in scores:
            print(format(t))

def format(t):
    cpu = t['cpu']
    cpu = ', '.join('%s: %s' % (k, v) for (k, v) in sorted(cpu.items()))
    gc = t['gc']
    gc = 'alloc: %.2f / %.2f' % (gc['max alloc'], gc['total alloc'])
    return '%s - %s - %s - %s' % (
        t['patch']['date'], t['score'], gc, cpu)

def read(fn):
    return [eval(line, None, None) for line in open(fn)]

if __name__ == '__main__':
    main()
