#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

'''Update all the saved scores with build/opt/update.'''

import os, subprocess, shutil, gzip

dry_run = True


def main():
    update_dir('save', 'save.new')

def update_dir(source, dest):
    subprocess.call(['mkdir', '-p', dest])
    for fn in os.listdir(source):
        update_file(os.path.join(source, fn), os.path.join(dest, fn))

def update_file(source, dest):
    print source, '->', dest
    if source.endswith('.git'):
        dest = dest[:-3] + 'ex-git'
        update(source, dest)
    elif os.path.basename(source) == 'ly':
        copytree(source, dest)
    elif os.path.isdir(source):
        update_dir(source, dest)
    elif is_score(source):
        update(source, dest)
    else:
        copy(source, dest)

def copy(source, dest):
    if dry_run:
        print 'copy', source, dest
    else:
        shutil.copy(source, dest)

def copytree(source, dest):
    if dry_run:
        print 'copytree', source, dest
    else:
        shutil.copytree(source, dest)

def update(source, dest):
    if dry_run:
        print 'update', source, dest
    else:
        code = subprocess.call(['build/opt/update', source, dest])
        if code != 0:
            raise ValueError(code)

def is_score(fn):
    if fn.endswith('.gz'):
        file = gzip.GzipFile
    else:
        file = open
    with file(fn) as fp:
        return fp.read(4) == 'scor'

if __name__ == '__main__':
    main()
