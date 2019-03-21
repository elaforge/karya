#!/usr/bin/python
"""Delete audio checkpoints with no symlinks.  This means they correspond to
    some older score state, so I can delete them and loading a new score will
    still likely hit the cache.
"""
from __future__ import print_function
import os
import sys
import subprocess

root = 'data/im/cache'

def main():
    dir_count = 0
    file_count = 0
    total_size = 0
    for dirpath, dirnames, filenames in os.walk(root):
        if os.path.basename(dirpath) == 'checkpoint':
            garbage = find_garbage(dirpath)
            dir_count += 1
            file_count += len(garbage)
            total_size += sum(map(os.path.getsize, garbage))
            for fn in garbage:
                os.remove(fn)
    if file_count > 0:
        print('%s: %d insts, deleted %d files, total %.2fmb, remaining: %s'
            % (sys.argv[0], dir_count, file_count,
                float(total_size) / 1024 / 1024, dir_size(root)))

def dir_size(dir):
    size = subprocess.check_output(['du', '-hsc', root])
    return size.decode('utf8').strip().split('\n')[-1].split()[0].lower()

def gc(dir):
    garbage = find_garbage(dir)

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
    ]

def listdir(dir):
    return [os.path.join(dir, fn) for fn in os.listdir(dir)]

if __name__ == '__main__':
    main()
