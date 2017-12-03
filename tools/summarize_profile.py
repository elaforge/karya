#!/usr/bin/env python
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

from __future__ import print_function
import sys, os, re, datetime, subprocess


write_files = True

profiles = (
    ['Derive_profile.profile_' + n for n in
        ['bloom', 'pnovla', 'viola_sonata']]
    + ['Perform.Midi.Perform_profile.profile_' + n for n in
        ['notes', 'control', 'complex', 'multiplex']]
)

def main():
    if sys.argv[-1] == 'scc':
        with_scc = True
    elif sys.argv[-1] == 'no-scc':
        with_scc = False
    else:
        print('usage: summarize_profile.py [ -n ] [ scc | no-scc ]')
        print('SCC means it was compiled with -auto-all -caf-all')
        sys.exit(1)
    global write_files
    if '-n' in sys.argv[1:]:
        write_files = False

    date = datetime.datetime.now().strftime('%y-%m-%d')
    mach_readable = []
    for prof in profiles:
        if with_scc:
            dir = os.path.join('prof/summary', prof)
        else:
            dir = os.path.join('prof/summary-no-scc', prof)
        subprocess.call(['mkdir', '-p', dir])
        summary = run(prof)
        write_dated_summary(dir, summary)
        summary = [('profile', prof), ('date', date)] + summary
        if with_scc:
            summary = [('with_scc', 'true')] + summary
        mach_readable.append(summary)
    summary = '\n'.join(map(str, mach_readable)) + '\n'
    write('prof/summary/machine_readable', 'a', summary)

def write_dated_summary(dir, summary):
    date = datetime.datetime.now().strftime('%y-%m-%d')
    i = 0
    while True:
        fn = os.path.join(dir, '%s.%02d' % (date, i))
        if not os.path.exists(fn):
            break
        i += 1
    write(fn, 'w', alist_to_str(summary) + '\n')

def write(path, mode, content):
    assert subprocess.call(['mkdir', '-p', os.path.dirname(path)]) == 0
    if write_files:
        fp = open(path, mode)
        fp.write(content)
        fp.close()
    else:
        print('----', path)
        print(content)

def alist_to_str(alist):
    return ', '.join('%s: %s' % (k, v) for (k, v) in alist)
    # str on a list uses repr(), which, before the latest versions, prints
    # unsightly complete precision.
    def s(v):
        if isinstance(v, float):
            return str(v)
        else:
            return repr(v)
    return ', '.join('(%r, %s)' % (k, s(v)) for (k, v) in alist)

def run(prof):
    p = subprocess.Popen(['tools/run_profile', prof], stderr=subprocess.PIPE)
    (_stdout, stderr) = p.communicate()
    stderr = stderr.decode('utf8')
    stem = None
    # Log msgs will also show up on stderr.
    for line in stderr.split('\n'):
        if line.startswith('### '):
            stem = line[4:]
        else:
            sys.stderr.write(line + '\n')
    return stats(stem.strip())

def stats(stem):
    total_time = float(extract(stem + '.prof', r'total time += +([0-9.,]+)'))
    total_alloc = bytes_to_mb(
        extract(stem + '.prof', r'total alloc += +([0-9.,]+)'))
    max_alloc = bytes_to_mb(
        extract(stem + '.gc', r'([0-9.,]+) bytes maximum residency'))
    productivity = float(extract(stem + '.gc', r'Productivity +([0-9.,]+)%'))
    return [
        ('time', total_time),
        ('total alloc', total_alloc),
        ('max alloc', max_alloc),
        ('productivity', productivity),
    ]

def bytes_to_mb(s):
    return '%.2fmb' % (float(s.replace(',', '')) / 1024 / 1024)

def extract(fn, reg):
    for line in open(fn):
        m = re.search(reg, line)
        if m:
            return m.group(1)
    raise ValueError('reg %r not found in %r' % (reg, fn))

if __name__ == '__main__':
    main()
