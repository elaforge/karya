#!/usr/bin/env zsh
# Upload the documentation to the website.

args=(
    --verbose -r --exclude=.DS_Store
    --copy-dirlinks --delete --delete-excluded
    build/{doc,haddock,hscolour}
    ofb.net:public_html/karya
)
set -eux
rsync $args
