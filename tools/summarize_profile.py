#!/usr/bin/env python

import sys, os, re, datetime, subprocess


profiles = [
    'Derive_profile.profile_big_block',
    'Derive_profile.profile_nested_controls',
    'Cmd.Responder_profile.profile_selection',
]

def main():
    date = datetime.datetime.now().strftime('%y-%m-%d')
    mach_readable = []
    for prof in profiles:
        dir = os.path.join('prof/summary', prof)
        subprocess.call(['mkdir', '-p', dir])
        summary = run(prof)
        open(os.path.join(dir, date), 'w').write(
            '%s\n' % alist_to_str(summary))
        summary = [('profile', prof), ('date', date)] + summary
        mach_readable.append(summary)
    fp = open('prof/summary/machine_readable', 'a')
    for summary in mach_readable:
        fp.write(str(summary) + '\n')
    fp.close()

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
    (stdout, stderr) = p.communicate()
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
