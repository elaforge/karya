#!/usr/bin/env python3
# Copyright 2020 Evan Laforge
# This program is distributed under the terms of the GNU General Public
# License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
"""Make OS X bundle directories."""

# There is a PkgInfo which is the old MacOS filetype/creator, I don't create
# it.  I think it can be "$type????".  It's not documented if it's used or how,
# but I've gotten away without it.

import argparse
import os
import subprocess
import textwrap


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--icon')
    parser.add_argument('--type', choices=type_extension.keys(), default='APPL')
    parser.add_argument('--make-wrapper', action='store_true')
    parser.add_argument('binary')
    args = parser.parse_args()

    app_dir = args.binary + '.' + type_extension[args.type]
    make_app_dir(app_dir, args.binary, args.icon, args.type)
    if args.make_wrapper:
        make_wrapper(args.binary)

type_extension = {
    'APPL': 'app',
    'BNDL': 'vst',
}

def make_wrapper(binary):
    # Make a shell script that runs the bundled executable
    # It uses $(basename $0) internally so it doesn't mind if it gets moved.
    name = os.path.basename(binary)
    with open(binary, 'w') as fp:
        fp.write(textwrap.dedent(f"""\
            #!/bin/sh
            dir=$(dirname "$0")
            bin=$(basename "$0")
            exec "$dir/$bin.app/Contents/MacOS/{name}" "$@"
        """))
    os.chmod(binary, 0o755)

def make_app_dir(app_dir, binary, icon, type):
    bindir = f'{app_dir}/Contents/MacOS'
    run(['rm', '-rf', app_dir])
    os.makedirs(bindir, exist_ok=True)
    os.rename(binary, os.path.join(bindir, os.path.basename(binary)))
    if icon:
        os.makedirs(f'{app_dir}/Contents/Resources', exist_ok=True)
        run(['cp', icon, f'{app_dir}/Contents/Resources/icon'])
        icon_info = '<key>CFBundleIconFile</key> <string>icon</string>'
    else:
        icon_info = ''
    with open(f'{app_dir}/Contents/Info.plist', 'w') as fp:
        name = os.path.basename(binary)
        fp.write(PLIST.format(
            name=name,
            id='elaforge.seq.' + name.replace(' ', '_'),
            type=type,
            icon_info=icon_info,
        ))

# The simplest Info.plist needed for an application.
PLIST = """\
<?xml version="1.0" encoding="UTF-8"?>
<plist version="0.9">
<dict>
    <key>CFBundleExecutable</key> <string>{name}</string>
    <key>CFBundleIdentifier</key> <string>{id}</string>
    <key>CFBundleInfoDictionaryVersion</key> <string>6.0</string>
    <key>CFBundleName</key> <string>{name}</string>
    <key>CFBundlePackageType</key> <string>{type}</string>
    <key>NSHighResolutionCapable</key><true/>
    {icon_info}
</dict>
</plist>
"""

# The one I used for VSTs had some extra fields, but I don't know if they
# matter:
# <!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
# ...
# <plist version="1.0">
# ...
# <key>CFBundleGetInfoString</key> <string>1</string>
# <key>CFBundleShortVersionString</key> <string>1</string>
# <key>CFBundleSignature</key> <string>????</string>
# <key>CFBundleVersion</key> <string>1</string>
# <key>LSMinimumSystemVersion</key> <string>10.8.0</string>


def run(cmd):
    subprocess.check_call(cmd)


if __name__ == '__main__':
    main()
