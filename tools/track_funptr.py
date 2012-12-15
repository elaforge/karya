#!/usr/bin/python
import sys

def main():
    allocated = {}
    while True:
        line = sys.stdin.readline()
        if not line:
            break
        if line.startswith('+'):
            ptr, name = line[2:].split(' ', 1)
            if ptr in allocated:
                print '** double alloc:', ptr, name
            allocated[ptr] = name.strip()
            show(allocated)
        elif line.startswith('-'):
            ptr = line[2:].strip()
            if ptr not in allocated:
                print '** free without alloc:', ptr
            del allocated[ptr]
            show(allocated)
        else:
            print line.rstrip()
        sys.stdout.flush()

def show(allocated):
    line = '> '
    for ptr, name in sorted(allocated.items()):
        line += '%s %s\t' % (ptr[-4:], name)
    print line.rstrip()


if __name__ == '__main__':
    main()
