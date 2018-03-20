#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""Return all the haskell library modules below this directory.

Args:
    in_repo - only include checked-in files
    notest - exclude *_test.hs and *Test.hs
    nomain - exclude modules with no name, or named Main
    dotted - emit names as dotted haskell modules instead of filenames
"""

from __future__ import print_function

import sys, os, subprocess

extensions = set(['.hs', '.hsc', '.chs'])

def main():
    args = sys.argv[1:]
    in_repo = 'in_repo' in args
    notest = 'notest' in args
    nomain = 'nomain' in args
    dotted = 'dotted' in args
    if in_repo:
        hs_files = get_in_repo()
    else:
        hs_files = get_all()

    fns = sorted(os.path.normpath(fn) for fn in hs_files)
    if notest:
        fns = filter(lambda fn: not is_test(fn), fns)
    if nomain:
        fns = filter(lambda fn: not is_main(fn), fns)
    if dotted:
        fns = map(to_dotted, fns)
    print(' '.join(fns))

def get_in_repo():
    if os.path.exists('.git'):
        files = subprocess.check_output(
            'git ls-tree --name-only -r HEAD'.split())
    else:
        files = subprocess.check_output(['darcs', 'show', 'files'])
    files = files.decode('utf8').split('\n')
    return list(filter(is_hs, map(os.path.normpath, files)))

def get_all():
    files = []
    for dirpath, subdirs, fnames in os.walk('.'):
        join = lambda fn: os.path.join(dirpath, fn)
        files.extend(join(fn) for fn in fnames if is_hs(fn))
        subdirs[:] = filter(capword, subdirs)
    return files

def is_hs(fn):
    return os.path.splitext(fn)[1] in extensions and capword(fn)

def is_test(fn):
    return fn.endswith('_test.hs') or fn.endswith('Test.hs')

def capword(s):
    return s and s[0].isupper()

def is_main(fn):
    for line in open(fn):
        if line.startswith('module Main'):
            return True
        elif line.startswith('module '):
            return False
        elif line.startswith('import '):
            break
    return True

def to_dotted(fn):
    return os.path.splitext(fn)[0].replace('/', '.')

if __name__ == '__main__':
    main()
