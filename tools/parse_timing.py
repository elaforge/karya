#!/usr/bin/env python3

"""Parse the output of util::timing.

    See NOTE [ui-loop-timing].
"""

import builtins
import sys, os, re


class Fltk:
    """analyze events from util::timing
    """
    # Omit timings shorter than this.
    min_diff = 0.001

    start = 'haskell'
    draw_block = 'Block::draw'
    draw_track_start = 'EventTrack::draw-start'
    draw_track_end = 'EventTrack::selection_overlay'

    # Either skip things I don't want, on whitelist things I do.
    # Not sure which is better.

    skip = set([
        # 'fl_draw', 'draw_text_line', 'drawable_pixels', 'draw_trigger',
    ])

class Cmd:
    """analyze events for the respond loop, from ghc Debug.Trace.trace
    """
    # Omit timings shorter than this.
    min_diff = 0.001
    start = 'respond'
    end = 'wait'
    skip = set([])


def main():
    mode = cmd_mode
    mode = fltk_mode
    # mode = raw_output
    if len(sys.argv) == 1:
        mode(open('seq.events'), sys.stdout)
    elif len(sys.argv) == 2:
        input = sys.argv[1]
        fltk_mode(open(input), open(f'{input}.times', 'w'))
        raw_output(open(input), open(f'{input}.raw', 'w'))
    else:
        sys.exit('usage: parse_timing [ seq.events ]')

def cmd_mode(file, out_fp):
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
def fltk_mode(file, out_fp):
    def print(*args):
        builtins.print(*args, file=out_fp)
    prev_ts = None
    prev_by = {}
    mode = Fltk

    wanted = set([
        'EventTrack::find_events',
        'EventTrack::find_events-start',
        'EventTrack::ruler_overlay',
        'EventTrack::draw_upper_layer',
        'EventTrack::free_text',
        'EventTrack::Track::draw',
        'EventTrack::Body::draw-start',
        'EventTrack::selection_overlay',
        'EventTrack::draw_waveforms',
        'EventTrack::draw_signal',

        'RulerTrack::draw-start',
        'RulerTrack::Body::draw-start',
        'RulerTrack::ruler_overlay',
        'RulerTrack::selection_overlay',
        'RulerTrack::Track::draw',

        'SelectionOverlay::start',

        'CachedScroll::redraw',
        'CachedScroll::surface-new',
        'CachedScroll::surface.draw',
        'CachedScroll::surface.image',
    ])
    skip = []

    sections = set([
        'RulerTrack::selection_overlay',
        'EventTrack::selection_overlay',
    ])

    histo = {}
    for num, line in enumerate(file):
        num += 1
        ts, name, _val = parse(line)
        if name.startswith('evt-') or name == 'events':
            prev_ts = None
        if prev_ts is None:
            prev_ts = ts
            prev_by = {1: ts}
            print('-' * 30, name)
            continue
        if name not in wanted or name in skip:
            histo[name] = histo.get(name, 0) + 1
            continue
        else:
            if False and histo:
                print('  ', ' '.join(
                    f'{k}:{v}' for k, v in sorted(histo.items()))
                )
            histo = {}

        diff = ts - prev_ts

        print(' '.join([fmt(diff), name, str(num)]))
        if name in sections:
            print('  ===>', fmt(ts - prev_by[1]))
            prev_by[1] = ts
        prev_ts = ts

fat_arrow = '====>'

def raw_output(file, out_fp):
    def print(*args):
        builtins.print(*args, file=out_fp)
    prev_ts = None
    for line in file:
        ts, name, _val = parse(line)
        if prev_ts is not None:
            delta = ts - prev_ts
            print(f'{ts:.04f} {delta:.04f} {name}')
        prev_ts = ts


def parse(line):
    ts, name, *val = line.split()
    return float(ts), name, ' '.join(val)


def fmt(diff):
    return '%.04f' % (diff,)


if __name__ == '__main__':
    main()
