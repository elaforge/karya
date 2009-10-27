#!/usr/bin/env python
'''Keep track of alloc and free msgs to figure who isn't being freed.
'''
import re, sys

alloc_re = r'alloc "(\w+)": (0x[0-9a-f]+)'
free_re = r'free (0x[0-9a-f]+)'

def main():
    allocs = {}
    while True:
        line = sys.stdin.readline()
        if not line:
            break
        if re.search(alloc_re, line):
            func, addr = re.search(alloc_re, line).groups()
            print 'alloc', func, addr
            allocs.setdefault(func, set()).add(addr)
        elif re.search(free_re, line):
            [addr] = re.search(free_re, line).groups(1)
            for func, addrs in allocs.items():
                if addr in addrs:
                    print 'free', func, addr
                    allocs[func].remove(addr)
                    break
            else:
                print 'not found:', addr
        else:
            sys.stdout.write(line)
            sys.stdout.flush()
        if re.search(alloc_re, line) or re.search(free_re, line):
            stats = ', '.join(
                '%s: %d' % (k, len(v)) for (k, v) in allocs.items())
            print stats

if __name__ == '__main__':
    main()
