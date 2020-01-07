#!/usr/bin/env python3
"""Delete audio checkpoints with no symlinks, and those over a certain age.
    This means they correspond to some older score state, so I can delete them
    and loading a new score will still likely hit the cache.

    If I update mtimes on cache hits, the minimum age should keep alive the
    previous few generations, so undo continues to hit them.
"""
import datetime
import os
import subprocess
import sys
import time


# Files younger than this always live.
min_age = datetime.timedelta(minutes=10)
default_root = 'data/im/cache'

def main():
    dir_count = 0
    file_count = 0
    total_size = 0
    if sys.argv[1:]:
        roots = sys.argv[1:]
    else:
        roots = [default_root]
    for root in roots:
        for dirpath, dirnames, filenames in os.walk(root):
            if os.path.basename(dirpath) == 'checkpoint':
                garbage = find_garbage(dirpath)
                dir_count += 1
                file_count += len(garbage)
                total_size += sum(map(os.path.getsize, garbage))
                for fn in garbage:
                    os.remove(fn)
    if file_count > 0:
        # Only show total for a complete GC, since a partial is run
        # automatically.
        if roots == [default_root]:
            remaining = ', remaining: ' + str(dir_size(default_root))
        else:
            remaining = ''
        print('%s: %d insts, deleted %d files, freed %.2fmb%s'
            % (sys.argv[0], dir_count, file_count,
                float(total_size) / 1024 / 1024, remaining))

def dir_size(dir):
    size = subprocess.check_output(['du', '-hsc', dir])
    return size.decode('utf8').strip().split('\n')[-1].split()[0].lower()

def find_garbage(dir):
    chunks = listdir(os.path.dirname(dir))
    alive = [
        os.path.splitext(os.path.basename(os.readlink(fn)))[0]
        for fn in chunks if os.path.islink(fn)
    ]
    return [
        os.path.join(dir, fn)
        for fn in os.listdir(dir)
        if not any(fn.startswith(prefix) for prefix in alive)
        if file_age(os.path.join(dir, fn)) > min_age
    ]

def listdir(dir):
    return [os.path.join(dir, fn) for fn in os.listdir(dir)]

def file_age(fn):
    return datetime.timedelta(seconds=time.time() - os.path.getmtime(fn))

if __name__ == '__main__':
    main()
