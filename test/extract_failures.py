#!/usr/bin/env python

import sys

failure = []
for line in sys.stdin:
    if line.startswith('++-> ') or line.startswith('__-> '):
        if failure:
            sys.stdout.write(''.join(failure) + '\n')
            failure = []
        if line.startswith('__'):
            failure.append(line)
    elif failure:
        failure.append(line)

if failure:
    sys.stdout.write(''.join(failure))
