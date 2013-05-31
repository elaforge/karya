#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

import sys, random, time, getopt

# pno note: pnovla/b1.t1
# float dyn: pnovla/b1.t27

def main():
    opts, args = getopt.gnu_getopt(sys.argv[1:], 'cn', [])
    opts = [opt for (opt, _) in opts]
    [times, track_id, low, high] = args
    low = float(low)
    high = float(high)
    if '-c' not in opts:
        emit('set_project_dir "build/test"')
        emit('cmd_save_git')
    for i in range(int(times)):
        generate(track_id, '-n' in opts, low, high)
    # emit('quit')

def generate(track_id, notes, low, high):
    pos = random.random() * (high - low) + low
    if notes:
        text = ''
        dur = 1
    else:
        text = '%.2f' % random.random()
        dur = 0
    emit('insert_event ("%s", %.3f, %.3f, "%s")' % (track_id, pos, dur, text))
    time.sleep(1)
    emit('wait_perf')

def emit(s):
    sys.stdout.write(s + '\n')
    sys.stdout.flush()


if __name__ == '__main__':
    main()
