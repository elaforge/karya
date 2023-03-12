#!/usr/bin/env python3
# Copyright 2013 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

'''Update all the saved scores with build/opt/update.

This will copy everything from save to save.new.
'''

import sys, os, subprocess, gzip

dry_run = True

update = 'build/opt/update'

def main():
    global dry_run
    if sys.argv[1:] == ['for-real']:
        dry_run = False
    elif sys.argv[1:] == ['dry-run']:
        print('DRY RUN')
    else:
        print('usage: %s [ dry-run | for-real ]' % (sys.argv[0],))
        return
    run(['bin/mk', update])
    skipped = update_dir('save', 'save.new')
    os.walk('save.new', rename_git, None)
    print('skipped:')
    print('\n'.join(skipped))

ex_git = 'ex-git'

def rename_git(_, dir, fns):
    """Put ex-git saves back to the default save.state, but avoid clobbering
    an existing save.state.
    """
    fns = filter(lambda fn: not fn.endswith('.repl'), fns)
    for fn in fns:
        if ex_git in fn:
            safe_rename(dir, fn, fn.replace(ex_git, 'state'))

def safe_rename(dir, source, dest):
    def path(fn):
        return os.path.join(dir, fn)
    if os.path.exists(path(dest)):
        rename(path(dest), path('old-' + dest))
    rename(path(source), path(dest))

def rename(source, dest):
    print('rename', source, '->', os.path.basename(dest))
    if not dry_run:
        os.rename(source, dest)

def update_dir(source, dest):
    run(['mkdir', '-p', dest])
    skipped = []
    for fn in os.listdir(source):
        if fn.startswith('.'):
            continue
        skipped += update_file(os.path.join(source, fn), os.path.join(dest, fn))
    return skipped

def update_file(source, dest):
    skipped = []
    # if not dry_run:
    #     print(source, '->', dest)
    if source.endswith('.git'):
        dest = dest[:-3] + ex_git
        run([update, source, dest])
    elif os.path.isdir(source):
        skipped += update_dir(source, dest)
    elif is_score(source) or source.endswith('.ky'):
        if not run([update, source, dest], fail_ok=True):
            skipped.append(source)
    else:
        run(['cp', '-R', source, dest], noisy=False)
    return skipped

def run(cmd, fail_ok=False, noisy=True):
    if noisy:
        print("%", *cmd, flush=True)
    if dry_run:
        return True
    code = subprocess.call(cmd)
    if not fail_ok and code != 0:
        raise ValueError(code)
    return code == 0

def is_score(fn):
    try:
        with gzip.GzipFile(fn) as fp:
            # From Cmd.Serialize.score_magic
            # TODO I should have a haskell program for this.
            return fp.read(4) == b'scor'
    except IOError:
        return False

if __name__ == '__main__':
    main()
