#!/bin/zsh
# Bundle up the documentation to upload to the website.
dirs=(
    build/{doc,haddock,hscolour}
    build/data
    doc/img
)
tar -L -czvf website.tgz $dirs
