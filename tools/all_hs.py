#!/usr/bin/python
"""Return all the haskell library modules below this directory.  Automatically
excludes .hs files generated from .hsc ones, and modules with no module
declaration, or a declaration of "Main".
"""

import sys, os

obsolete = set(['Midi/PortMidiC.hs', 'Midi/PortMidi.hsc', 'Midi/TestMidi.hs'])

more_obsolete = set('''
    Cmd/Lang/Environ.hs
    Derive/Call/Sekar.hs
    Derive/Operator.hs
    Derive/Parse.hs
    LogView/Timer.hs
    Midi/TestCoreMidi.hs
    Util/Chart.hs
'''.split())

obsolete = obsolete.union(more_obsolete)


def main():
    notest = 'notest' in sys.argv
    hsc_as_hs = 'hsc_as_hs' in sys.argv
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
    fns = filter(lambda fn: not is_main(fn) and fn not in obsolete, fns)
    if notest:
        fns = filter(lambda fn: not is_test(fn), fns)
    if hsc_as_hs:
        fns = [fn.replace('.hsc', '.hs') for fn in fns]
    print ' '.join(fns)

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

if __name__ == '__main__':
    main()
