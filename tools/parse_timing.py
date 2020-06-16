#!/usr/bin/env python3
"""Parse the output of util::timing.
"""

import sys, os, re

# Omit timings shorter than this.
min_diff = 0.001

start = 'start'
draw_block = 'Block::draw'
draw_track_start = 'EventTrack::draw-start'
draw_track_end = "EventTrack::selection_overlay"

# Either skip things I don't want, on whitelist things I do.
# Not sure which is better.

skip = set([
    "fl_draw", "draw_text_line", "drawable_pixels", "draw_trigger",
])

wanted = set([
    'EventTrack::find_events',
    'EventTrack::ruler_overlay',
    'EventTrack::draw_upper_layer',
]).union(set([start, draw_block, draw_track_start, draw_track_end]))

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
            prev_by = {draw_block: ts}
            print('-' * 30)
            continue
        diff = ts - prev_ts

        out = [fmt(diff), name]
        # time to do a complete draw
        if name == draw_block:
            out.extend([draw_block, fmt(ts - prev_by[draw_block])])
        elif name == draw_track_start:
            prev_by[draw_track_start] = ts
        elif name == draw_track_end:
            out.extend([draw_track_start, fmt(ts - prev_by[draw_track_start])])
        elif name in skip:
            continue
        if len(out) > 2:
            out.insert(2, '======>')
        if diff > min_diff or len(out) > 2: # or True:
            print(' '.join(out))
            prev_ts = ts


def fmt(diff):
    return '%.04f' % (diff,)


if __name__ == '__main__':
    main()
