#!/usr/bin/env python3
"""Read a cabal.config freeze file and produce a JSON file, for hackage.nix.
"""

import sys
import re
import subprocess
import json


INPUT = "doc/cabal/all-deps.cabal.config"
OUTPUT = "nix/hackage.json"


def main():
    # Omit the bootlibs, since their versions are hardcoded by the compiler.
    bootlibs = get_bootlibs()
    pkgs = []
    for line in open(INPUT):
        if line.startswith('constraints: '):
            line = line[len('constraints: '):]
        m = re.match(r'\s*(\S+) ==([0-9.]+),?$', line)
        if not m:
            continue
        pkg, version = m.groups()
        if pkg in bootlibs:
            continue
        pkgs.append({'pkg': pkg, 'ver': version})
    pkgs.sort(key=lambda p: p['pkg'])
    with open(OUTPUT, 'w') as fp:
        fp.write(json.dumps(pkgs, indent=2, sort_keys=True))


def get_bootlibs():
    # This works for me because I don't install into the global namespace,
    # but isn't reliable in general.
    return set(subprocess.check_output(
        ['ghc-pkg', 'list', '--global', '--simple-output', '--names-only']
    ).decode('utf8').split())


if __name__ == '__main__':
    main()
