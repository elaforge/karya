#!/usr/bin/env python

import sys

def any(iter):
    for v in iter:
        if v:
            return True
    return False

failure = []
for line in sys.stdin:
    if any(line.startswith(pre) for pre in ['++-> ', '__-> ', '----']):
        if failure:
            sys.stdout.write(''.join(failure) + '\n')
            failure = []
        if line.startswith('__'):
            failure.append(line)
    elif failure:
        failure.append(line)

if failure:
    sys.stdout.write(''.join(failure))
