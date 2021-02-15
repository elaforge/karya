#!/usr/bin/env python3
"""Read a cabal.config freeze file and strip out the bootlibs.  Those are
    fixed to the ghc version anyway.
"""

import sys
import re
import subprocess
import json


def main():
    input = sys.argv[1]
    # Omit the bootlibs, since their versions are hardcoded by the compiler.
    bootlibs = get_bootlibs()
    pkgs = {}
    for line in open(input):
        if line.startswith('constraints: '):
            line = line[len('constraints: '):]
        m = re.match(r'\s*(\S+) ==([0-9.]+),?$', line)
        if not m:
            continue
        pkg, version = m.groups()
        if pkg in bootlibs:
            continue
        pkgs[pkg] = version
    with open(input, 'w') as fp:
        fp.write('constraints:\n  ')
        fp.write(',\n  '.join(p + ' ==' + v for p, v in sorted(pkgs.items())))
        fp.write('\n')


def get_bootlibs():
    # This works for me because I don't install into the global namespace,
    # but isn't reliable in general.
    return set(subprocess.check_output(
        ['ghc-pkg', 'list', '--global', '--simple-output', '--names-only']
    ).decode('utf8').split())


if __name__ == '__main__':
    main()
