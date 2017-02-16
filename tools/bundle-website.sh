#!/bin/zsh
# Bundle up the documentation to upload to the website.
# data is huge, no need to re-upload if it hasn't changed.
tar -L --exclude build/doc/data -czvf website.tgz build/{doc,haddock,hscolour}
