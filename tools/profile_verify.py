#!/usr/bin/env python3
# Copyright 2018 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""usage: profile_verify.py score1 score2 ...

Run verify_performance, combine its timing results with per-run metadata,
and collect in timing_dir.
"""

import datetime
import json
import os
import re
import socket
import sys
import subprocess


# If true, it's ok if the verify fails.  Otherwise, I abort, because if it's
# really broken then that might affect timing.
fail_ok = False

# Run each score this time, to get a range of timing values.
run_times = 6

verify_binary = 'build/profile/verify_performance'
scratch_dir = 'build/verify-profile'

# Write json results to this directory.
timing_dir = 'prof/timing'


def main():
    scores = sys.argv[1:]
    if not scores:
        print(__doc__)
        return 0
    run(['bin/mk', verify_binary])
    empty_dir(scratch_dir)
    os.makedirs(timing_dir, exist_ok=True)

    metadata = metadata_json()
    timings = []
    for score in scores:
        timing = verify(score)
        timing.update(metadata)
        timings.append(timing)
    # I want to name files by patch time, so just ls will be in logical time
    # order.
    fn = '%s_run:%s.json' % (metadata['patch']['date'], metadata['run_date'])
    fn = os.path.join(timing_dir, fn)
    with open(fn, 'w') as fp:
        for timing in timings:
            fp.write(json.dumps(timing) + '\n')

def fix_old_json():
    # Add a field to old JSONs.
    # Used as a one-off, I'll keep it around in case I need it again.
    ghc = subprocess.check_output(['ghc', '--numeric-version']).strip() \
        .decode('utf-8')
    for fn in sys.argv[1:]:
        print(fn)
        out = []
        for line in open(fn):
            js = json.loads(line)
            js['ghc'] = ghc
            out.append(json.dumps(js))
        open(fn, 'w').write('\n'.join(out) + '\n')

def metadata_json():
    patch = parse_json(
        subprocess.run(
            [verify_binary, '--mode=PatchInfo'],
            stdout=subprocess.PIPE,
            check=True).stdout)
    ghc = subprocess.check_output(['ghc', '--numeric-version']).strip() \
        .decode('utf-8')
    return {
        'system': socket.gethostname().split('.')[0],
        'ghc': ghc,
        'patch': patch,
        'run_date': datetime.datetime.now().isoformat(),
    }

def verify(score):
    gc, cmd = cmdline(score)
    timings = []
    for _ in range(run_times):
        run(cmd, check=not fail_ok)
        timing_fn = os.path.join(scratch_dir, os.path.basename(score) + '.json')
        timings.append(parse_json(open(timing_fn).read()))
    return {
        'score': score,
        'cpu': merge_dicts(timings),
        'gc': parse_gc(open(gc).readlines()),
    }

def cmdline(score):
    gc = os.path.join(scratch_dir, os.path.basename(score)) + '.gc'
    return gc, [
        verify_binary, '+RTS', '-s' + gc, '-RTS',
        '--out=' + scratch_dir,
        score
    ]

def parse_gc(lines):
    total_alloc = bytes_to_mb(
        extract(lines, r'([0-9.,]+) bytes allocated in the heap'))
    max_alloc = bytes_to_mb(
        extract(lines, r'([0-9.,]+) bytes maximum residency'))
    productivity = float(extract(lines, r'Productivity +([0-9.,]+)%')) / 100
    return {
        'total alloc': total_alloc,
        'max alloc': max_alloc,
        'productivity': productivity,
    }


### util

def parse_json(content):
    return json.loads(content)

def bytes_to_mb(s):
    return float(s.replace(',', '')) / 1024 / 1024
    # return '%.2fmb' % (float(s.replace(',', '')) / 1024 / 1024)

def extract(lines, reg):
    for line in lines:
        m = re.search(reg, line)
        if m:
            return m.group(1)
    raise ValueError('reg %r not found in %s' % (reg, lines))

def merge_dicts(dicts):
    merged = {}
    for k in set.union(*map(set, dicts)):
        merged[k] = list(filter(None, (d.get(k) for d in dicts)))
    return merged

def run(cmd, check=True):
    print('## ' + ' '.join(cmd))
    subprocess.run(cmd, check=check)

def empty_dir(dir):
    try:
        fns = os.listdir(dir)
    except FileNotFoundError:
        pass
    else:
        for fn in fns:
            os.remove(os.path.join(dir, fn))
    os.makedirs(dir, exist_ok=True)


if __name__ == '__main__':
    main()
