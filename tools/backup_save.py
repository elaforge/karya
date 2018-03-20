#!/usr/bin/env python3

import datetime, os, sys, subprocess, json


dir = '../data/save-backup'
verify_binary = 'build/opt/verify_performance'

def main():
    run(['bin/mk', verify_binary])
    patch = patch_json()
    tgz = '%s_%s_run:%s.tgz' % (
        patch['date'], patch['hash'], datetime.datetime.now().isoformat())
    tgz = os.path.join(dir, tgz)
    run(['tar', '-cLzf', tgz, 'save'])
    run(['ls', '-lh', tgz])

def patch_json():
    return json.loads(
        subprocess.run(
            [verify_binary, '--mode=PatchInfo'],
            stdout=subprocess.PIPE,
            check=True).stdout)

def run(cmd):
    print('##', ' '.join(cmd))
    subprocess.run(cmd, check=True)

if __name__ == '__main__':
    main()
