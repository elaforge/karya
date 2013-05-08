## GUI

The GUI only has one kind of window, so it's pretty straightforward, but there
are some wrinkles.

Text entry boxes expand temporarily if their text is too large to fit.  It's
not entirely satisfactory, but I couldn't come up with a better way to cram
text into small spaces.

Control-h and control-l skip backward and forward by a token, where a token is
a space separated word, a `symbol`, or a parenthesized expression.  TODO maybe
it should just be a word.  Holding shift extends the selection as usual.
Control-backspace deletes a token.  Otherwise, they use the shortcuts
documented on fltk's Fl_Input.

## Cmds

## Ruler

## Tracks

## Signal render

## REPL

[repl](repl.md.html)
