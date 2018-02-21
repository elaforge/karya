#!/usr/bin/env python3
# Copyright 2018 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""Simple UI to scan the profiling stats as written by profile_verify.py

If this doesn't suffice, probably the next step is sqlite.
"""

import sys, os, json

# Read json results from this directory.
timing_dir = 'prof/timing'

patch_name_column = True

def main():
    scores = set(sys.argv[1:])
    timings = []
    for fn in os.listdir(timing_dir):
        timings.extend(read(os.path.join(timing_dir, fn)))

    timings.sort(key=lambda json: json['patch']['date'])
    for system in sorted(set(t['system'] for t in timings)):
        print('\n' + system + ': ')
        print(format([t for t in timings if t['system'] == system], scores))

def format(timings, scores):
    cols = []
    if patch_name_column:
        cols.append('patch')
    cols.append('date')
    if not scores:
        cols.append('score')
    cols.extend(['max mb', 'total mb'])
    cols.extend(['derive', 'lily', 'perform'])

    rows = [cols]
    for t in timings:
        if scores and t['score'] not in scores:
            continue
        row = []
        if patch_name_column:
            row.append(t['patch']['name'][:64])
        row.append(t['patch']['date'].split('T')[0])
        if not scores:
            row.append(os.path.basename(t['score']))
        row.extend([t['gc']['max alloc'], t['gc']['total alloc']])
        for field in ['derive', 'lilypond', 'perform']:
            row.append((t['cpu'].get(field, [])))
        rows.append(list(map(format_field, row)))
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
    # Certain lists and []s are necessary or by-default generators make
    # different results.  Wow.  I think no need to bother with python3 any more.
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
