This is a place for local additions, which are not in source control.

Normally you'd initialize the contents via tools/setup-empty, but once you
start making your own configuration you should probably make a User/$USER and
put the modules in there, and track via source control as normal.

Presumably you'd make that a symlink so you could have a non-overlapping repo,
unless git has some way of combining repos that I don't know about.  Of course
if they seem to be of general use, you could submit them back to the main repo.
