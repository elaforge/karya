#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

'''Automatically add tiresome licensing boilerplate.'''

import os, sys

copyright = '''\
Copyright 2013 Evan Laforge
This program is distributed under the terms of the GNU General Public
License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
'''

def main():
    for fn in sys.argv[1:]:
        process(fn)

def process(fn):
    if any(fn.endswith(e) for e in ('.hs', '.hsc')):
        comment = '--'
    elif any(fn.endswith(e) for e in ('.cc', '.c', '.h')):
        comment = '//'
    else:
        comment = '#'
    msg = ''.join(
        '%s %s\n' % (comment, line) for line in copyright.split('\n')
        if line)
    with open(fn) as fp:
        line = fp.readline()
        shebang = ''
        if line.startswith('#!'):
            shebang = line
            line = fp.readline()
        if line == msg[:msg.index('\n')+1]:
            return
        contents = line + fp.read()
    print 'add copyright to', fn
    tmp = fn + '.tmp'
    with open(tmp, 'w') as fp:
        fp.write(shebang)
        fp.write(msg)
        if not contents.startswith('\n'):
            fp.write('\n')
        fp.write(contents)
    os.chmod(tmp, os.stat(fn).st_mode)
    os.rename(tmp, fn)

if __name__ == '__main__':
    main()
