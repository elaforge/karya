#!/usr/bin/env python3
# Copyright 2018 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""Simple UI to scan the profiling stats as written by timing/verify.py

If this doesn't suffice, probably the next step is sqlite.
"""

import argparse
import json
import os
import socket
import sys

# Read json results from this directory.
timing_dir = 'data/prof/timing'

patch_name_column = True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--host', default=socket.gethostname().split('.')[0])
    parser.add_argument('--run-date')
    parser.add_argument('--scores', action='store_true')
    parser.add_argument('score', nargs='?', default=None)
    args = parser.parse_args()

    timings = []
    for fn in os.listdir(timing_dir):
        if not fn.endswith('.json'):
            continue
        timings.extend(read(os.path.join(timing_dir, fn)))

    if args.scores:
        for score in sorted(set(t['score'] for t in timings)):
            print(score)
        return

    if args.host:
        timings = [t for t in timings if t['system'] == args.host]
    if args.run_date:
        timings = [t for t in timings if t['run_date'] >= args.run_date]
    if args.score:
        timings = [t for t in timings if t['score'] == args.score]
    timings.sort(key=lambda json: json['patch']['date'])

    systems = sorted(set(t['system'] for t in timings))
    for system in systems:
        if len(systems) > 1:
            print(f'\n{system}:')
        scores = sorted(set(t['score'] for t in timings))
        for score in scores:
            if len(scores) > 1:
                print(f'\n{score}:')
            print(format([
                t for t in timings
                if t['system'] == system and t['score'] == score
            ]))

COLUMNS = [
    ('patch', lambda t: t['patch']['name'][:64]),
    ('date', lambda t: t['patch']['date'].split('T')[0]),
    ('note', lambda t: t.get('note', '')),
    ('failed', lambda t: 'X' if t.get('failed') else ' '),
    ('max mb', lambda t: t['gc']['max alloc']),
    ('total mb', lambda t: t['gc']['total alloc']),
    ('prod', lambda t: t['gc']['productivity']),
    ('derive', lambda t: t['cpu'].get('derive')),
    ('lilypond', lambda t: t['cpu'].get('lilypond')),
    ('perform', lambda t: t['cpu'].get('perform')),
    ('ghc', lambda t: t['ghc']),
]

def format(timings):
    columns = [
        c for c in COLUMNS
        if c[0] != 'patch' or patch_name_column
    ]
    timings = sorted(timings, key=lambda t: (t['patch']['date'], t['run_date']))
    rows = [[c[0] for c in columns]]
    for t in timings:
        rows.append([format_field(c[1](t)) for c in columns])
    return format_columns(rows)

def format_field(f):
    if f is None:
        return ''
    elif type(f) is list:
        return format_range(f)
    elif type(f) is float:
        return '%.2f' % f
    elif type(f) is str:
        return f
    else:
        raise TypeError(str(f))

def format_range(r):
    if not r:
        return ''
    return '%.2f~%.2f' % (min(r), max(r))

def format_columns(rows):
    widths = [list(map(len, r)) for r in rows]
    widths = [max(ws) + 1 for ws in rotate(widths)]
    out = []
    for row in rows:
        out.append(''.join(cell.ljust(w + 1) for w, cell in zip(widths, row)))
    return '\n'.join(out)

def rotate(rows):
    cols = []
    for i in range(max(map(len, rows))):
        cols.append([row[i] for row in rows if i < len(row)])
    return cols

def read(fn):
    return [json.loads(line) for line in open(fn)]

if __name__ == '__main__':
    main()
