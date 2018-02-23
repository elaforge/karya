#!/bin/zsh
# Upload the documentation to upload to the website.
rsync --verbose -r --copy-dirlinks \
    --delete --delete-excluded --exclude=.DS_Store \
    build/{doc,haddock,hscolour} save \
    ofb.net:public_html/karya
