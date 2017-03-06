#!/usr/bin/env python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

from __future__ import print_function

import sys, subprocess, re, os

def main():
    whatsnew = run(['darcs', 'whatsnew', '--summary'] + sys.argv[1:])
    adds, subtracts = parse_whatsnew(whatsnew.split('\n'))

    normal_by_dir = {}
    test_by_dir = {}
    for path in set(adds).union(set(subtracts)):
        if path.endswith('_test.hs'):
            by_dir = test_by_dir
        else:
            by_dir = normal_by_dir
        dir = os.path.dirname(path) or '.'
        add, subtract = by_dir.get(dir, (0, 0))
        by_dir[dir] = (adds.get(path, 0) + add,
            subtracts.get(path, 0) + subtract)

    if normal_by_dir or test_by_dir:
        longest = max(
            map(len, list(normal_by_dir.keys()) + list(test_by_dir.keys())))
    else:
        longest = 1
    col1 = 20
    print('%-*s %-*s test' % (longest, 'dir', col1, 'normal'))
    for dir in sorted(set(normal_by_dir).union(set(test_by_dir))):
        normal = normal_by_dir.get(dir, (0, 0))
        test = test_by_dir.get(dir, (0, 0))
        print('%-*s %-*s %s' % (
            longest, dir, col1, show_diff(normal), show_diff(test)))
    print()
    print('%-*s %-*s %s' % (longest, 'total', col1,
        show_diff(sum_diffs(normal_by_dir.values())),
        show_diff(sum_diffs(test_by_dir.values()))))

def sum_diffs(diffs):
    return (sum(map(fst, diffs)), sum(map(snd, diffs)))

def show_diff(add_sub):
    (add, sub) = add_sub
    return '+%d-%d %d (%d)' % (add, abs(sub), add+sub, add + abs(sub))

def parse_whatsnew(lines):
    adds = {}
    subtracts = {}
    for line in lines:
        m = re.match(r'[MAR] ([./a-zA-Z0-9_-]+) *(\-\d+)? *(\+\d+)?$', line)
        if not m:
            if m and m[0] in 'MAR':
                print('no match', repr(line))
            continue
        path = os.path.normpath(m.groups()[0])
        if path.endswith('TODO'):
            continue # not actually code changes
        dir = os.path.dirname(path) or '.'

        if line.endswith('/'):
            pass # ignore directories
        if line.startswith('A'):
            # darcs doesn't show lines for new files
            if not os.path.isdir(path):
                adds[path] = len(list(open(path)))
        elif line.startswith('R'):
            diff = run(['darcs', 'diff', path])
            subtracts[path] = -diff.count('\n')
        else:
            for diff in filter(None, m.groups()[1:]):
                if diff.startswith('-'):
                    subtracts[path] = int(diff)
                elif diff.startswith('+'):
                    adds[path] = int(diff)
    return adds, subtracts

def fst(x): return x[0]
def snd(x): return x[1]

def run(cmd):
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    stdout, _ = p.communicate()
    return stdout.decode('utf8')

if __name__ == '__main__':
    main()

