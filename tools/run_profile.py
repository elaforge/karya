#!/usr/bin/env python3
# Copyright 2020 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
"""
    # Run matching profiles with biographical profiling:
    tools/run_profile.py -hb profile1 profile2

    # Compile Derive/Derive_profile.hs to RunProfile-Derive.Derive:
    tools/run_profile.py -Derive.Derive profile_big_block

    # Profile deriving a particular score:
    tools/run_profile.py derive path/to/score

    Flags for 'heap' env var:
    hc - by producing cost-center stack
    hm - by module
    hd - by closure description
    hy - by type
    hr - by retainer set
    hb - by biography
"""

import os
import sys
import subprocess
import datetime
import re


out_base = 'data/prof'

def main():
    heap_flags, args = partition(lambda arg: arg.startswith('-h'), sys.argv[1:])
    if not heap_flags:
        heap_flags = ['-hc']
    cmd = args[0] if args else ''
    # [(arg | None, subdir)]
    # For each pair, run a profile with the arg, and put the results in a
    # subdir.
    profiles = []
    if cmd == 'seq':
        cmdline = ['build/profile/seq']
        profiles.append((None, 'seq'))
    elif cmd == 'perform':
        cmdline = ['build/profile/verify_performance', '--mode=Profile']
        profiles.extend((p, 'verify.' + os.path.basename(p)) for p in args[1:])
    elif cmd == 'derive':
        cmdline = ['build/profile/verify_performance', '--mode=ProfileDerive']
        profiles.extend((p, 'verify.' + os.path.basename(p)) for p in args[1:])
    else:
        if cmd.startswith('-'):
            cmdline = ['build/profile/RunProfile' + cmd]
            args = args[1:] # remove cmd
        else:
            cmdline = ['build/profile/RunProfile']
        run(['bin/mk', cmdline[0]])
        profiles.extend(
            (p, p.split('-')[1])
            for p in capture([cmdline[0], '--list'] + args).split()
        )

    run(['bin/mk', cmdline[0]])
    for profile_arg, subdir in profiles:
        for i in range(100):
            out_dir = os.path.join(out_base, ymd(), f'{subdir}.{i:02d}')
            try:
                os.makedirs(out_dir)
            except FileExistsError:
                pass
            else:
                break
        stem = os.path.join(out_dir, ''.join(heap_flags).lstrip('-'))
        basename = os.path.basename(cmdline[0])

        rts = [
            # time profiling
            '-s{}.gc'.format(basename), # emit runtime summary
            '-p', # emit .prof file

            # heap profiling
            '-L42', # field length for cost center names in heap profile
            '-xt', # show threads and stacks as TSO and STACK
        ] + heap_flags
        rts = ['+RTS'] + rts + ['-RTS']

        run(cmdline + rts + ([profile_arg] if profile_arg else []),
            tee_to=stem + '.stdout')

        for suf in 'gc hp aux tix stdout prof'.split():
            try:
                os.rename(f'{basename}.{suf}', f'{stem}.{suf}')
            except FileNotFoundError:
                pass
        if os.path.exists(f'{stem}.hp'):
            system(f'hp2ps -b -c < {stem}.hp > {stem}.ps')
            # TODO if ghostscript is installed, there is a ps2pdf, but
            # OS X preview can do the convert itself.
        run_if_exists(['ghc-prof-flamegraph', stem + '.prof',
            '--output', stem + '.flame.svg'])
        run_if_exists(['hp2html', stem + '.hp'])
        summarize(stem)
        print(out_dir, os.path.basename(stem))


### summarize

def summarize(stem):
    """Write .gc and .prof summary to a diffable .summary file."""
    gc = parse_gc(list(open(stem + '.gc')))
    ccs = parse_prof(stem + '.prof')
    with open(stem + '.summary', 'w') as fp:
        for k, v in sorted(gc.items()):
            fp.write(f'{k}: {v:.2f}\n')
        fp.write('\n')
        for cc in sorted(ccs):
            fp.write(cc + '\n')

def parse_prof(fname):
    lines = open(fname)
    for line in lines:
        if line.startswith('COST CENTRE'):
            break
    ccs = []
    for line in lines:
        if re.search(r'^ +individual', line):
            break
        words = line.split()
        if not words:
            continue
        [cc, module, src, time, alloc] = words
        ccs.append(' '.join(
            word.ljust(width) for (word, width) in [
                (cc, 24), (module, 42), (time, 4), (alloc, 0)
            ]
        ))
    return ccs


# From profile_verify.py
def parse_gc(lines):
    total_alloc = bytes_to_mb(
        extract(lines, r'([0-9.,]+) bytes allocated in the heap'))
    max_alloc = bytes_to_mb(
        extract(lines, r'([0-9.,]+) bytes maximum residency'))
    productivity = float(extract(lines, r'Productivity +([0-9.,]+)%')) / 100
    return {
        'total alloc': total_alloc,
        'max alloc': max_alloc,
        'productivity': productivity,
    }

def bytes_to_mb(s):
    return float(s.replace(',', '')) / 1024 / 1024
    # return '%.2fmb' % (float(s.replace(',', '')) / 1024 / 1024)

def extract(lines, reg):
    for line in lines:
        m = re.search(reg, line)
        if m:
            return m.group(1)
    raise ValueError('reg %r not found in %s' % (reg, lines))

### util

def ymd():
    return datetime.datetime.today().strftime('%y-%m-%d')

def run(cmd, tee_to=None):
    print('%', *cmd)
    if tee_to is None:
        subprocess.check_call(cmd)
    else:
        out = open(tee_to, 'w')
        p = subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
        )
        for line in p.stdout:
            sys.stdout.write(line)
            sys.stdout.flush()
            out.write(line)
        out.close()
        if p.wait() != 0:
            raise RuntimeError('non-zero: ' + str(p.wait()))

def run_if_exists(cmd):
    try:
        run(cmd)
    except FileNotFoundError:
        pass

def capture(cmd):
    return subprocess.check_output(cmd).decode('utf8').strip()

def system(cmd):
    print('%', cmd)
    code = subprocess.Popen(cmd, shell=True).wait()
    if code != 0:
        print('=== returned', code)

def partition(f, xs):
    yes, no = [], []
    for x in xs:
        (yes if f(x) else no).append(x)
    return yes, no

if __name__ == '__main__':
    main()
