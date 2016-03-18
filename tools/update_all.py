#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

'''Update all the saved scores with build/opt/update.'''

import os, subprocess, shutil, gzip

dry_run = True


def main():
    if dry_run:
        print 'DRY RUN'
    update_dir('save', 'save.new')
    os.path.walk('save.new', rename_git, None)

ex_git = 'ex-git'

def rename_git(_, dir, fns):
    """Put ex-git saves back to the default save.state, but avoid clobbering
    an existing save.state.
    """
    fns = filter(lambda fn: not fn.endswith('.repl'), fns)
    for fn in fns:
        if ex_git in fn:
            safe_rename(dir, fn, fn.replace(ex_git, 'state'))

def safe_rename(dir, source, dest):
    def path(fn):
        return os.path.join(dir, fn)
    if os.path.exists(path(dest)):
        rename(path(dest), path('old-' + dest))
    rename(path(source), path(dest))

def rename(source, dest):
    print 'rename', source, '->', os.path.basename(dest)
    if not dry_run:
        os.rename(source, dest)

def update_dir(source, dest):
    subprocess.call(['mkdir', '-p', dest])
    for fn in os.listdir(source):
        if fn.startswith('.'):
            continue
        update_file(os.path.join(source, fn), os.path.join(dest, fn))

def update_file(source, dest):
    if not dry_run:
        print source, '->', dest
    if source.endswith('.git'):
        dest = dest[:-3] + ex_git
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
    print 'build/opt/update', source, dest
    if not dry_run:
        code = subprocess.call(['build/opt/update', source, dest])
        if code != 0:
            raise ValueError(code)

def is_score(fn):
    try:
        with gzip.GzipFile(fn) as fp:
            # From Cmd.Serialize.score_magic
            # TODO I should have a haskell program for this.
            return fp.read(4) == 'scor'
    except IOError:
        return False

if __name__ == '__main__':
    main()
