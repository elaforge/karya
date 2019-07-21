#!/usr/bin/env zsh
# Upload the documentation to the website.

rsync --verbose -r --exclude=.DS_Store \
    --copy-dirlinks --delete --delete-excluded \
    build/{doc,haddock,hscolour} save \
    ofb.net:public_html/karya
