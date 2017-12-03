#!/usr/bin/python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

# Automatically add tiresome licensing boilerplate.

from __future__ import print_function
import os, sys, datetime, re

dry_run = True

copyright_template = '''\
Copyright %s Evan Laforge
This program is distributed under the terms of the GNU General Public
License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
'''

def main():
    for fn in sys.argv[1:]:
        process(fn)

def process(fn):
    template = add_comments(fn, copyright_template)
    copyright = template % datetime.datetime.now().year
    copyright_pattern = re.escape(template).replace('\\%s', '201[0-9]')

    with open(fn) as fp:
        contents = fp.read()

    if contents.startswith('#!'):
        (shebang, contents) = contents.split('\n', 1)
    else:
        shebang = ''
    if re.match(copyright_pattern, contents):
        # print(fn, 'already has copyright')
        return
    if dry_run:
        print('would have added copyright to', fn)
    else:
        print('adding copyright to', fn)
    if dry_run:
        return
    tmp = fn + '.tmp'
    with open(tmp, 'w') as fp:
        fp.write(shebang)
        fp.write(copyright)
        if not contents.startswith('\n'):
            fp.write('\n')
        fp.write(contents)
    os.chmod(tmp, os.stat(fn).st_mode)
    os.rename(tmp, fn)

def add_comments(fn, txt):
    if any(fn.endswith(e) for e in ('.hs', '.hsc')):
        comment = '--'
    elif any(fn.endswith(e) for e in ('.cc', '.c', '.h')):
        comment = '//'
    else:
        comment = '#'
    return ''.join(
        '%s %s\n' % (comment, line) for line in txt.split('\n') if line)

if __name__ == '__main__':
    main()
