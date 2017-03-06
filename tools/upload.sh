#!/bin/zsh
# Upload the documentation to upload to the website.
rsync --verbose -r --copy-dirlinks --delete build/{doc,haddock,hscolour} save \
    ofb.net:public_html/karya
