#!/usr/bin/env python3
"""Take a cabal.config freeze file on stdin, and produce a JSON file with the
    appropriate hashes on stdout.
"""

import sys
import re
import subprocess
import json


def main():
    bootlibs = get_bootlibs()
    pkgs = []
    for line in sys.stdin:
        m = re.match(r'\s+(\S+) ==([0-9.]+),?$', line)
        if not m:
            continue
        pkg, version = m.groups()
        if pkg in bootlibs:
            continue
        src = (
            'http://hackage.haskell.org/package'
            + f'/{pkg}-{version}/{pkg}-{version}.tar.gz'
        )
        cabal = (
            'https://raw.githubusercontent.com/commercialhaskell'
            + f'/all-cabal-files/hackage/{pkg}/{version}/{pkg}.cabal'
        )
        src_hash = get_hash(src, unpack=True)
        cabal_hash = get_hash(cabal, unpack=False)
        pkgs.append({
            'pkg': pkg, 'ver': version,
            'src': src, 'srcHash': src_hash,
            'cabal': cabal, 'cabalHash': cabal_hash,
        })
    pkgs.sort(key=lambda p: p['pkg'])
    print(json.dumps(pkgs, indent=2, sort_keys=True))


def get_hash(url, unpack):
    cmd = ['nix-prefetch-url', url] + (['--unpack'] if unpack else [])
    return subprocess.check_output(cmd).decode('utf8').strip()


def get_bootlibs():
    return set(subprocess.check_output(
        ['ghc-pkg', 'list', '--global', '--simple-output', '--names-only']
    ).decode('utf8').split())


if __name__ == '__main__':
    main()
