#!/usr/bin/env python3
"""Parse the output of util::timing.
"""

import sys, os, re


start = 'start'
draw = 'draw'
draw_track = 'draw_track'

# start -> [haskell] -> wait -> [draw] -> Block::draw -> [wait ui]
# fltk might skip [draw] and hence Block::draw.  E.g. for a cursor move.

def main():
    prev_ts = None
    prev_by = {}
    for line in open(sys.argv[1]):
        ts, name, val = line.split()
        ts = float(ts)
        val = int(val)
        if name == start:
            prev_ts = None
        if prev_ts is None:
            prev_ts = ts
            prev_by = {draw: ts}
            print('-' * 30)
            continue
        diff = ts - prev_ts

        out = [fmt(diff), name]
        # time to do a complete draw
        if name == 'Block::draw':
            out.extend([draw, fmt(ts - prev_by[draw])])
        elif name == 'EventTrack::draw':
            prev_by[draw_track] = ts
        elif name == 'EventTrack::draw_area':
            out.extend([draw_track, fmt(ts - prev_by[draw_track])])
        if len(out) > 2:
            out.insert(2, '======>')
        if diff > 0.001 or len(out) > 2: # or True:
            print(' '.join(out))
            prev_ts = ts


def fmt(diff):
    return '%.04f' % (diff,)


if __name__ == '__main__':
    main()
