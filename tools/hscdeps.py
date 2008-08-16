#!/usr/bin/env python
"""usage: hscdeps.py [ hsc_flags ] a.hsc b.hsc ...
Print makefile style deps for all the headers the given hscs depend on.
"""

import sys, os, re, subprocess

def main():
    hscs, hsc_flags = partition(lambda s: s.endswith('.hsc'), sys.argv[1:])
    assert hscs, "no .hsc files given"
    # intermediate hsc c file to the .hs file which should be rebuilt if the
    # deps change
    intermediate_to_hs = {}

    cmd = ['hsc2hs', '--no-compile'] + hsc_flags
    for hsc in hscs:
        res = subprocess.call(cmd + [hsc])
        if res != 0:
            print >>sys.stderr, 'cmd failed:', cmd
            return 1
        intermediate_to_hs['%s_hsc_make.c' % hsc[:-4]] = hsc[:-1]
    output = []
    try:
        include_flags = [f for f in hsc_flags if f.startswith('-I')]
        cmd = ['g++', '-MM'] + include_flags + intermediate_to_hs.keys()
        deps, _ = subprocess.Popen(cmd, stdout=subprocess.PIPE).communicate()
        for line in deps.split('\n'):
            m = re.match(r'(\S+?): (\S+?) ', line)
            if m:
                fn, first_dep = m.groups()
                output.append('%s: \\' % intermediate_to_hs[first_dep])
            else:
                output.append(line)
    finally:
        for fn in intermediate_to_hs.keys():
            os.remove(fn)

    print '\n'.join(output)


def partition(f, lst):
    return [x for x in lst if f(x)], [x for x in lst if not f(x)]


if __name__ == '__main__':
    main()
