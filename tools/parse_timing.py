#!/usr/bin/env python3
"""Parse the output of util::timing.

    See NOTE [ui-loop-timing].
"""

import sys, os, re


class Fltk:
    # Omit timings shorter than this.
    min_diff = 0.001

    draw_block = 'Block::draw'
    draw_track_start = 'EventTrack::draw-start'
    draw_track_end = 'EventTrack::selection_overlay'

    # Either skip things I don't want, on whitelist things I do.
    # Not sure which is better.

    skip = set([
        'fl_draw', 'draw_text_line', 'drawable_pixels', 'draw_trigger',
    ])

    wanted = set([
        'EventTrack::find_events',
        'EventTrack::ruler_overlay',
        'EventTrack::draw_upper_layer',
    ]).union(set([start, draw_block, draw_track_start, draw_track_end]))

class Cmd:
    # Omit timings shorter than this.
    min_diff = 0.001
    start = 'respond'
    end = 'wait'
    skip = set([])

def main():
    cmd_mode(open(sys.argv[1]))

def cmd_mode(file):
    prev_ts = None
    start = None
    mode = Cmd
    for line in file:
        ts, name, val = parse(line)
        diff = ts - prev_ts if prev_ts else 0

        out = [fmt(diff), name]
        if name == mode.start:
            print(' '.join(out), '-' * 10, ts, val)
            start = ts
            prev_ts = ts
            continue
        if name in mode.skip:
            continue
        elif name == mode.end and start:
            out.append(fmt(ts - start))
        if diff > mode.min_diff:
            print(' '.join(out))
            prev_ts = ts


# See NOTE [ui-loop-timing].
def fltk_mode(file):
    prev_ts = None
    prev_by = {}
    mode = Fltk
    for line in file:
        ts, name, _val = parse(line)
        if name.startswith('evt-') or name == 'events':
            prev_ts = None
        if prev_ts is None:
            prev_ts = ts
            prev_by = {mode.draw_block: ts}
            print('-' * 30)
            continue
        diff = ts - prev_ts

        out = [fmt(diff), name]
        # time to do a complete draw
        if name == mode.draw_block:
            out.extend([mode.draw_block, fmt(ts - prev_by[mode.draw_block])])
        elif name == mode.draw_track_start:
            prev_by[mode.draw_track_start] = ts
        elif name == draw_track_end:
            out.extend([
                mode.draw_track_start, fmt(ts - prev_by[mode.draw_track_start])
            ])
        elif name in mode.skip:
            continue
        if len(out) > 2:
            out.insert(2, '======>')
        if diff > mode.min_diff or len(out) > 2: # or True:
            print(' '.join(out))
            prev_ts = ts


def parse(line):
    ts, name, *val = line.split()
    return float(ts), name, ' '.join(val)


def fmt(diff):
    return '%.04f' % (diff,)


if __name__ == '__main__':
    main()
