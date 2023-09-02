#!/usr/bin/env python3
"""Given a module, print out all symbols used from that module.
    Useful for creating and trimming export lists.

    TODO parse the export list and show which ones are used?
"""
import argparse
import json
import subprocess
import sys


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('module')
    args = parser.parse_args()

    try:
        # Haskell identifiers can include ', but it tends to get false
        # positives due to haddock 'refs', and I don't use 's.
        regex = args.module + '\\.[a-zA-Z_]+'
        lines = subprocess.check_output(
            ['rg', '--type', 'haskell', '--json', regex]
        ).decode('utf8').split('\n')
    except subprocess.CalledProcessError as exc:
        if exc.returncode == 1:
            lines = []
        else:
            raise
    call_to_fn = {}
    for line in lines:
        if not line:
            continue
        js = json.loads(line)
        if js['type'] != 'match':
            continue
        # Skip comments.  I'll still get bogus hits in {- -} commments or
        # strings.
        if js['data']['lines']['text'].lstrip().startswith('--'):
            continue
        call = js['data']['submatches'][0]['match']['text']
        fn = js['data']['path']['text']
        call_to_fn.setdefault(call, set()).add(fn)

    for call, fns in sorted(call_to_fn.items()):
        print(call, '||', *sorted(list(fns)))

if __name__ == '__main__':
    main()
