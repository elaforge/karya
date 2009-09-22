#!/usr/bin/python
"""Return all the haskell library modules below this directory.  Automatically
excludes .hs files generated from .hsc ones, and modules with no module
declaration, or a declaration of "Main".
"""

import sys, os

def main():
    if len(sys.argv) > 1 and sys.argv[1] == 'notest':
        notest = True
    else:
        notest = False
    hs_files = []
    hsc_files = []
    def accum(_arg, dirname, fnames):
        join = lambda fn: os.path.join(dirname, fn)
        hs_files.extend([join(fn)
            for fn in fnames if fn.endswith('.hs') and capword(fn)])
        hsc_files.extend([join(fn)
            for fn in fnames if fn.endswith('.hsc') and capword(fn)])
        fnames[:] = filter(capword, fnames)
    os.path.walk('.', accum, None)

    hs_files = [fn for fn in hs_files if fn+'c' not in hsc_files]
    fns = [fn[2:] for fn in hs_files + hsc_files]
    fns.sort()
    fns = filter(lambda fn: fn != 'Data/Map.hs' and not is_main(fn), fns)
    if notest:
        fns = filter(
            lambda fn: not (fn.endswith('_test.hs') or fn.endswith('Test.hs')),
            fns)
    print ' '.join(fns)

def capword(s):
    return s and s[0].isupper()

def is_main(fn):
    for line in open(fn):
        if line.startswith('module Main'):
            return True
        elif line.startswith('module '):
            return False
    return True

if __name__ == '__main__':
    main()
