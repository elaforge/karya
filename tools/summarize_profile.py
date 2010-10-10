#!/usr/bin/env python

import os, re, datetime, subprocess


profiles = [
    'Derive_profile.profile_big_block',
    'Derive_profile.profile_nested_controls',
]

def main():
    subprocess.call(['mkdir', '-p', 'prof/summary'])
    date = datetime.datetime.now().strftime('%y-%m-%d')
    for prof in profiles:
        summary = date + ' ' + run(prof)
        open(os.path.join('prof/summary', prof), 'a').write(summary + '\n')

def run(prof):
    p = subprocess.Popen(['tools/run_profile', prof], stderr=subprocess.PIPE)
    (stdout, stderr) = p.communicate()
    return stats(stderr.strip())

def stats(stem):
    total_time = extract(stem + '.prof', r'total time += +([0-9.,]+)')
    total_alloc = bytes_to_mb(
        extract(stem + '.prof', r'total alloc += +([0-9.,]+)'))
    max_alloc = bytes_to_mb(
        extract(stem + '.gc', r'([0-9.,]+) bytes maximum residency'))
    productivity = extract(stem + '.gc', r'Productivity +([0-9.,]+%)')
    return 'time: %s sec, total alloc: %s, max alloc: %s, productivity: %s' % (
        total_time, total_alloc, max_alloc, productivity)

def bytes_to_mb(s):
    return '%.2fmb' % (float(s.replace(',', '')) / 1024 / 1024,)

def extract(fn, reg):
    for line in open(fn):
        m = re.search(reg, line)
        if m:
            return m.group(1)
    raise ValueError('reg %r not found in %r' % (reg, fn))

if __name__ == '__main__':
    main()
