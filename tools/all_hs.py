#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""Return all the haskell library modules below this directory.

Args:
    notest - exclude *_test.hs and *Test.hs
    hsc_as_hs - replace .hsc files with their .hs equivalents, in build/hsc
    nomain - exclude modules with no name, or named Main
    dotted - emit names as dotted haskell modules instead of filenames
"""

from __future__ import print_function

import sys, os


def main():
    notest = 'notest' in sys.argv
    hsc_as_hs = 'hsc_as_hs' in sys.argv
    nomain = 'nomain' in sys.argv
    dotted = 'dotted' in sys.argv
    hs_files = []
    hsc_files = []
    for dirpath, subdirs, fnames in os.walk('.'):
        join = lambda fn: os.path.join(dirpath, fn)
        hs_files.extend(
            join(fn) for fn in fnames if fn.endswith('.hs') and capword(fn))
        hsc_files.extend(
            join(fn) for fn in fnames if fn.endswith('.hsc') and capword(fn))
        subdirs[:] = filter(capword, subdirs)

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
