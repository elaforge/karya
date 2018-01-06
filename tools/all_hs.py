#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""Return all the haskell library modules below this directory.

Args:
    in_repo - only include checked-in files
    notest - exclude *_test.hs and *Test.hs
    hsc_as_hs - replace .hsc files with their .hs equivalents, in build/hsc
    nomain - exclude modules with no name, or named Main
    dotted - emit names as dotted haskell modules instead of filenames
"""

from __future__ import print_function

import sys, os, subprocess


def main():
    args = sys.argv[1:]
    in_repo = 'in_repo' in args
    notest = 'notest' in args
    hsc_as_hs = 'hsc_as_hs' in args
    nomain = 'nomain' in args
    dotted = 'dotted' in args
    if in_repo:
        hs_files, hsc_files = get_in_repo()
    else:
        hs_files, hsc_files = get_all()

    if hsc_as_hs:
        hsc_files = [os.path.join('build/hsc', fn.replace('.hsc', '.hs'))
            for fn in hsc_files]
    fns = sorted(os.path.normpath(fn) for fn in hs_files + hsc_files)
    if notest:
        fns = filter(lambda fn: not is_test(fn), fns)
    if nomain:
        fns = filter(lambda fn: not is_main(fn), fns)
    if dotted:
        fns = map(to_dotted, fns)
    print(' '.join(fns))

def get_in_repo():
    files = subprocess.check_output(['darcs', 'show', 'files']).split('\n')
    files = map(os.path.normpath, files)
    hs_files = [f for f in files if f.endswith('.hs')]
    hsc_files = [f for f in files if f.endswith('.hsc')]
    return hs_files, hsc_files

def get_all():
    hs_files = []
    hsc_files = []
    for dirpath, subdirs, fnames in os.walk('.'):
        join = lambda fn: os.path.join(dirpath, fn)
        hs_files.extend(
            join(fn) for fn in fnames
            if fn.endswith('.hs') and capword(fn))
        hsc_files.extend(
            join(fn) for fn in fnames
            if fn.endswith('.hsc') and capword(fn))
        subdirs[:] = filter(capword, subdirs)
    return hs_files, hsc_files

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
