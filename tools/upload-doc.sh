#!/usr/bin/env zsh
# Upload the documentation to the website.

args=(
    --verbose -r --exclude=.DS_Store
    --copy-dirlinks --delete --delete-excluded
)
set -eux
rsync $args build/{doc,haddock} ofb.net:public_html/karya
