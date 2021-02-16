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
    pkgs = parse(input)
    # Omit the bootlibs, since their versions are hardcoded by the compiler.
    for pkg in get_bootlibs():
        pkgs.pop(pkg, None)
    unparse(input, pkgs)


def propagate(source_fname, dest_fname):
    """Copy versions from source_fname to dest_fname."""
    source = parse(source_fname)
    dest = parse(dest_fname)
    for pkg in dest:
        dest[pkg] = source[pkg]
    unparse(dest_fname, dest)


def parse(fname):
    pkgs = {}
    for line in open(fname):
        if line.startswith('constraints: '):
            line = line[len('constraints: '):]
        m = re.match(r'\s*(\S+) ==([0-9.]+),?$', line)
        if not m:
            continue
        pkg, version = m.groups()
        pkgs[pkg] = version
    return pkgs


def unparse(fname, pkgs):
    with open(fname, 'w') as fp:
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
