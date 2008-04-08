#!/usr/bin/env python
"""Substitute various function calls for equivalents that have file names and
line numbers.

I can't use cpp because it doesn't like dots in identifiers.  I didn't use
haskell because its regex support is really complicated and looks like it
doesn't support substitution.
"""

import sys, os, re

# These are substituted everywhere.
global_macros = [
    ('Log.debug', 'Log.debug_line'),
]

# These are only substituted in test modules.
test_macros = [
    ('equal', 'equal_line', False),
    ('io_human', 'io_human_line', True),
]

def main():
    [orig_fn, src_fn, dest_fn] = sys.argv[1:]

    macros = global_macros
    if orig_fn.endswith('_test.hs'):
        macros.extend((src, dest) for (src, dest, _) in test_macros)

    out = open(dest_fn, 'w')
    out.write('{-# LINE 1 "%s" #-}\n' % orig_fn)
    for i, line in enumerate(open(src_fn)):
        for src, dest in macros:
            line = re.sub(r'\b%s\b' % src,
                '%s "%s" %d' % (dest, orig_fn, i+1), line)
        out.write(line)


if __name__ == '__main__':
    main()
