#!/usr/bin/python
# Get the GHC version.  This imitates 'Shake.Shakefile.parseGhcVersion'.

from __future__ import print_function
import subprocess, os, re

path = subprocess.check_output(['ghc', '--print-libdir']).decode('utf8').strip()
path = os.path.split(path)[1]
path = path.split('-', 1)[1]
path = ''.join('0' * (2 - len(c)) + c for c in path.split('.'))
path = path.lstrip('0')[:5]
print(path)
