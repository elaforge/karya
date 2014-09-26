#!/usr/bin/env python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

"""Collect the text before a failure marker and print it with the failure.
  This provides context for the failure.
"""

import sys

failure = []
for line in sys.stdin:
    if any(map(line.startswith, ['++-> ', '__-> '])):
        if failure:
            sys.stdout.write(''.join(failure) + '\n')
            failure = []
        if line.startswith('__'):
            failure.append(line)
    elif failure:
        failure.append(line)

if failure:
    sys.stdout.write(''.join(failure))
