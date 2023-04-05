#!/usr/bin/env python3
# Copyright 2018 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""Simple UI to scan the profiling stats as written by timing/verify.py

If this doesn't suffice, probably the next step is sqlite.
"""

import argparse
import datetime
import json
import os
import socket
import sys
import time


# Read json results from this directory.
timing_dir = 'data/prof/timing'

patch_name_column = True

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--host', default=socket.gethostname().split('.')[0])
    parser.add_argument('--date')
    parser.add_argument('--run-date')
    parser.add_argument('--scores', action='store_true')
    parser.add_argument('score', nargs='?', default=None)
    args = parser.parse_args()

    timings = read_timings()
    if args.scores:
        for score in sorted(set(t['score'] for t in timings)):
            print(score)
        return

    if args.host:
        timings = [t for t in timings if t['system'] == args.host]
    if args.run_date:
        timings = [t for t in timings if t['run_date'] >= args.run_date]
    if args.date:
        timings = [t for t in timings if t['patch']['date'] >= args.date]
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

def read_timings():
    timings = []
    for fn in os.listdir(timing_dir):
        if not fn.endswith('.json'):
            continue
        timings.extend(read(os.path.join(timing_dir, fn)))
    for t in timings:
        # 2021-11-27T18:06:35.543414
        t['run_date'] = utc2local(
            datetime.datetime.fromisoformat(t['run_date']))
        # 2021-11-27T23:20:16Z
        t['patch']['date'] = utc2local(
            datetime.datetime.strptime(t['patch']['date'], "%Y-%m-%dT%H:%M:%SZ")
        )
    return timings

def utc2local(date):
    epoch = time.mktime(date.timetuple())
    offset = (
        datetime.datetime.fromtimestamp(epoch)
        - datetime.datetime.utcfromtimestamp(epoch)
    )
    return (date + offset).isoformat()

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
    rows = []
    for t in timings:
        rows.append([format_field(c[1](t)) for c in columns])
    # Omit empty columns.
    empty = set(
        i for i in range(len(columns)) if all(not row[i] for row in rows)
    )
    rows.insert(0, [c[0] for c in columns])
    return format_columns(
        [[c for i, c in enumerate(row) if i not in empty] for row in rows]
    )

def format_field(f):
    if f is None:
        return ''
    elif isinstance(f, list):
        return format_range(f)
    elif isinstance(f, float):
        return '%.2f' % f
    elif isinstance(f, str):
        return f.strip()
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
