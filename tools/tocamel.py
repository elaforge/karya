#!/usr/bin/env python3
"""Convert symbols in source files to camelCase."""

import argparse
import sys
import fileinput
import re


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--module')
    parser.add_argument('file', nargs="*")
    args = parser.parse_args()
    for line in fileinput.input(args.file, inplace=True):
        if args.module:
            line = convert_from(args.module)
        else:
            line = convert_all(line)
        sys.stdout.write(line)

def convert_all(line):
    """Convert all underscore words to camel case."""
    return re.sub(
        # Negative lookbehind assertion to avoid matching qualified imports.
        # This will match the M in M.a, but to_camel will ignore those.
        r'(?<!\.)\b([a-zA-Z_]+)\b',
        lambda m: to_camel(m.group(1)),
        line,
    )

def convert_from(module, line):
    """Convert qualified names from some module to camel case."""
    return re.sub(
        r'\b(' + module + r'\.)([a-zA-Z_]+)\b',
        lambda m: m.group(1) + to_camel(m.group(2)),
        line,
    )

def to_camel(cs):
    if cs[0].isupper():
        return cs # Don't mess with module names.
    out = []
    i = 0
    while i < len(cs):
        if cs[i] == '_' and i+1 < len(cs):
            i += 1
            out.append(cs[i].upper())
        else:
            out.append(cs[i])
        i += 1
    return ''.join(out)


if __name__ == '__main__':
    main()
