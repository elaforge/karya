#!/usr/bin/env python3
# Copyright 2020 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
"""
    # Run matching profiles with biographical profiling:
    tools/run_profile.py --heap b RunProfile profile1 profile2

    # Compile Derive/Derive_profile.hs to RunProfile-Derive.Derive:
    tools/run_profile.py Derive.Derive profile_big_block

    # Profile deriving a particular score:
    tools/run_profile.py derive path/to/score

    Args for --heap:
    c - by producing cost-center stack
    m - by module
    d - by closure description
    y - by type
    r - by retainer set
    b - by biography

    -- profiling.html#time-and-allocation-profiling
    -p -P -pa
    The -p option produces a standard time profile report. It is written into
    the file <stem>.prof; the stem is taken to be the program name by default,
    but can be overridden by the -po ⟨stem⟩ flag.

    The -P option produces a more detailed report containing the actual time
    and allocation data as well. (Not used much.)

    The -pa option produces the most detailed report containing all cost
    centres in addition to the actual time and allocation data.
"""

import argparse
import os
import shutil
import sys
import subprocess
import datetime
import re


base_dir = 'data/prof/run_profile'

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        '--heap', default='c', help='Heap profiling, separated by commas.'
    )
    parser.add_argument('--name', default=None)
    parser.add_argument('profile')
    parser.add_argument('args', nargs="*")
    args = parser.parse_args()
    heap_flags = ['-h' + f for f in args.heap.split(',')]

    # [(arg | None, subdir)]
    # For each pair, run a profile with the arg, and put the results in a
    # subdir.
    profiles = []
    if args.profile == 'seq':
        cmdline = ['build/profile/seq']
        profiles.append((None, 'seq'))
    elif args.profile == 'perform':
        cmdline = ['build/profile/verify_performance', '--mode=Profile']
        profiles.extend((p, 'verify.' + os.path.basename(p)) for p in args.args)
    elif args.profile == 'derive':
        cmdline = ['build/profile/verify_performance', '--mode=ProfileDerive']
        profiles.extend((p, 'verify.' + os.path.basename(p)) for p in args.args)
    else:
        if args.profile == 'RunProfile':
            cmdline = ['build/profile/RunProfile']
        else:
            cmdline = ['build/profile/RunProfile-' + args.profile]
        run(['bin/mk', cmdline[0]])
        profiles.extend(
            (p, p.split('-')[1])
            for p in capture([cmdline[0], '--list'] + args.args).split()
        )

    # output is ###.{name | date}/
    run(['bin/mk', cmdline[0]])
    name = args.name if args.name else ymd()
    num = next_num(base_dir)
    for profile_arg, subdir in profiles:
        for heap_flag in heap_flags:
            out_dir = f'{base_dir}/{num:03d}.{name}/{subdir}'
            os.makedirs(out_dir, exist_ok=True)
            open(f'{out_dir}/date', 'w').write(
                datetime.datetime.now().isoformat() + '\n')
            stem = os.path.join(out_dir, heap_flag.lstrip('-'))
            basename = os.path.basename(cmdline[0])

            rts = [
                # time profiling
                '-s{}.gc'.format(basename), # emit runtime summary
                '-p', # emit .prof file

                # heap profiling
                '-L42', # field length for cost center names in heap profile
                heap_flag,
            ]
            if heap_flag == '-hb':
                # Otherwise: -hb cannot be used with multiple capabilities
                rts.append('-N1')
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
            run_if_exists(['profiterole', stem + '.prof'])
            summarize(stem)


def next_num(dir):
    os.makedirs(dir, exist_ok=True)
    existing = os.listdir(dir)
    if existing:
        return int(max(existing).split('.')[0]) + 1
    else:
        return 0


### summarize

def summarize(stem):
    """Write .gc and .prof summary to a diffable .summary file."""
    gc = parse_gc(stem)
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
            # len('Derive.Deriver.Internal.with_stack_region') + 4
            # len('User.Elaforge.Instrument.Vsl') + 4
            # len('10.9') + 1
            word.ljust(width) for (word, width) in [
                (cc, 46), (module, 32), (time, 4), (alloc, 0)
            ]
        ))
    return ccs


# From profile_verify.py
def parse_gc(stem):
    lines = list(open(stem + '.gc'))
    total_alloc = bytes_to_mb(
        extract(lines, r'([0-9.,]+) bytes allocated in the heap'))
    max_alloc = bytes_to_mb(
        extract(lines, r'([0-9.,]+) bytes maximum residency'))
    productivity = float(extract(lines, r'Productivity +([0-9.,]+)%')) / 100
    # RP is retainer profiling time, if I'm doing that I care about memory, not
    # time.
    out = {}
    for type in ['INIT', 'MUT', 'GC', 'PROF', 'Total']:
        out[type] = float(extract(lines, fr'{type}\s+time\s+(\d+\.\d+)s'))
    out.update({
        'total alloc': total_alloc,
        'max alloc': max_alloc,
        'productivity': productivity,
    })

    # Get a few things from .prof.  I don't why these are so different from the
    # .gc output, but lower numbers are better:
    # total time  =        1.42 secs   (5265 ticks @ 1000 us, 8 processors)
    # total alloc = 3,763,731,272 bytes  (excludes profiling overheads)
    for line in open(stem + '.prof'):
        words = line.split()
        if words[:3] == ['total', 'time', '=']:
            out['prof-total-time'] = float(words[3])
        elif words[:3] == ['total', 'alloc', '=']:
            out['prof-total-alloc'] = bytes_to_mb(words[3])
            break
    return out

def bytes_to_mb(s):
    return float(s.replace(',', '')) / 1024 / 1024

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
    if shutil.which(cmd[0]):
        run(cmd)

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
