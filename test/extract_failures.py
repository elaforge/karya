#!/usr/bin/env python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
