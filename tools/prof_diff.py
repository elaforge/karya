#!/usr/bin/env python3
"""Format and diff .summary outputs from tools/run_profile.py.
"""

import argparse
import sys
import os
import subprocess
import tempfile


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        '-v', '--vimdiff', action='store_true', help='Use vimdiff.')
    parser.add_argument('old')
    parser.add_argument('new')
    args = parser.parse_args()
    old_gc, old_index = parse(args.old)
    new_gc, new_index = parse(args.new)

    old_out = temp('old')
    new_out = temp('new')
    for (old_name, old), (new_name, new) in zip(old_gc, new_gc):
        assert old_name == new_name, f'{old_name} != {new_name}'
        if float(new) > float(old):
            change = '+'
        elif new == old:
            change = '='
        else:
            change = '-'
        width = len('prof-total-alloc:')
        prefix = f'{change} {old_name:{width}} '
        old_out.write(prefix + old)
        new_out.write(prefix + new)

    old_out.write('\n')
    new_out.write('\n')
    for key, old in sorted(old_index.items()):
        new = new_index.pop(key, '\n')
        old_out.write(old)
        new_out.write(new)
    for _, new in sorted(new_index.items()):
        new_out.write(new)
    old_out.close()
    new_out.close()
    max_width = max(
        max(map(len, old_index.values())),
        max(map(len, new_index.values())),
    ) * 2 + 6
    print(max_width, old_out.name, new_out.name)
    try:
        if args.vimdiff:
            subprocess.call(['vimdiff', old_out.name, new_out.name])
        else:
            subprocess.call([
                'diff', '--side-by-side', f'--width={max_width}',
                old_out.name, new_out.name,
            ])
    finally:
        os.remove(old_out.name)
        os.remove(new_out.name)

def parse(fname):
    d = {}
    in_gc = True
    gc = []
    for line in open(fname):
        if not line.strip():
            in_gc = False
        if in_gc:
            name, val = line.split(': ')
            gc.append((name, val))
        else:
            words = line.split()
            if words:
                d[words[0]] = line
    return gc, d

def temp(name):
    _, name = tempfile.mkstemp(suffix='.'+name)
    return open(name, 'w')


if __name__ == '__main__':
    main()
