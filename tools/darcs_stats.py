#!/usr/bin/env python
import sys, subprocess, re, os

def main():
    p = subprocess.Popen(['darcs', 'whatsnew', '-l'] + sys.argv[1:],
        stdout=subprocess.PIPE)
    stdout, _ = p.communicate()
    totals = {}
    for line in stdout.split('\n'):
        m = re.match(r'[MA] ([./a-zA-Z_-]+) *(\-\d+)? *(\+\d+)?$', line)
        if not m:
            if m and m[0] in 'MA':
                print 'no match', repr(line)
            continue
        path = os.path.normpath(m.groups()[0])
        if path == 'TODO':
            continue # not actually code changes
        dir = os.path.dirname(path) or '.'

        if line.startswith('A'):
            # darcs doesn't show lines for new files
            if os.path.isdir(path):
                diff = 0
            else:
                diff = len(list(open(path)))
        else:
            diff = sum(map(int, filter(None, m.groups()[1:])))
        nontest_total, test_total = totals.get(dir, (0, 0))
        if path.endswith('_test.hs'):
            test_total += diff
        else:
            nontest_total += diff
        totals[dir] = (nontest_total, test_total)
    print 'dir\tnontest\ttest'
    for dir, (nontest, test) in sorted(totals.items()):
        print '%s\t%d\t%d' % (dir, nontest, test)
    print 'total\t%d\t%d\t= %d' % (sum(map(fst, totals.values())),
        sum(map(snd, totals.values())), sum(map(sum, totals.values())))

def fst((a, b)): return a
def snd((a, b)): return b

if __name__ == '__main__':
    main()

