#!/usr/bin/python
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
