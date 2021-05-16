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
    old_fname = infer_filename(args.old)
    new_fname = infer_filename(args.new)
    if os.path.isdir(old_fname) and os.path.isdir(new_fname):
        for old, new in pairs(os.listdir(old_fname), os.listdir(new_fname)):
            if old:
                old = f'{old_fname}/{old}/hc.summary'
            if new:
                new = f'{new_fname}/{new}/hc.summary'
            print(f'=== prof_diff.py {old} {new} ===')
            diff(old, new, vimdiff=args.vimdiff)
            print()
    else:
        diff(old_fname, new_fname, vimdiff=args.vimdiff)


def diff(old_fname, new_fname, vimdiff):
    old_gc, old_index = parse(old_fname)
    new_gc, new_index = parse(new_fname)

    old_out = temp('old')
    new_out = temp('new')
    for (old_name, old), (new_name, new) in zip(old_gc, new_gc):
        assert old_name == new_name, f'{old_name=} != {new_name=}'
        delta = float(new) - float(old)
        delta = f'{delta:+.2f}'.ljust(7)
        width = len('prof-total-alloc:')
        prefix = f'{delta} {old_name:{width}} '
        old_out.write(prefix + old)
        new_out.write(prefix + new)

    old_out.write('\n')
    new_out.write('\n')
    max_width = max(
        max(map(len, old_index.values())),
        max(map(len, new_index.values())),
    ) * 2 + 6
    for key, old in sorted(old_index.items()):
        new = new_index.pop(key, '\n')
        old_out.write(old)
        new_out.write(new)
    for _, new in sorted(new_index.items()):
        new_out.write(new)
    old_out.close()
    new_out.close()
    try:
        if vimdiff:
            subprocess.call(['vimdiff', old_out.name, new_out.name])
        else:
            subprocess.call([
                'diff', '--side-by-side', f'--width={max_width}',
                old_out.name, new_out.name,
            ])
    finally:
        os.remove(old_out.name)
        os.remove(new_out.name)

def infer_filename(arg):
    if os.path.isfile(arg) or os.path.isdir(arg):
        return arg
    elif arg.isdigit():
        matches = [
            fn for fn in os.listdir('data/prof/run_profile')
            if int(fn.split('.', 1)[0]) == int(arg)
        ]
        return f'data/prof/run_profile/{matches[0]}'
    else:
        raise Exception(f'neither file nor number: {arg}')

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

def pairs(xs, ys, key=lambda x: x):
    xs = list(reversed(sorted(xs)))
    ys = list(reversed(sorted(ys)))
    out = []
    while xs or ys:
        if not ys:
            out.append((xs.pop(), None))
        elif not xs:
            out.append((None, ys.pop()))
        elif key(xs[-1]) == key(ys[-1]):
            out.append((xs.pop(), ys.pop()))
        elif key(xs[-1]) < key(ys[-1]):
            out.append((xs.pop(), None))
        else:
            out.append((None, ys.pop()))
    return out


if __name__ == '__main__':
    main()
