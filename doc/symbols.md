## Symbols

Scores are more likely than most text to make use of many symbols.  So it may
seem strange that the tracklang does not support unicode identifiers.  Instead
it supports a backtick notation: text inside of backticks \`like this\` is
looked up in a table that maps symbol names to symbol glyphs, which may include
one or more unicode glyphs but also placement and rotation.  The name to glyph
map is declared statically in the source, although it's spread around based on
who is using it: 'Derive.Call.Symbols', 'Derive.Scale.Symbols', and
'Derive.Instrument.Symbols'.  They are collected at startup time and loaded by
'App.LoadConfig'.

This design has several motivations.  Firstly, the major problem with unicode
is how to type it.  Even though I'm used to switching between several IMEs it's
still a hassle.  And it hurts interoperability since not everyone uses or even
can use the same IMEs.  Fltk doesn't support IMEs anyway.  Secondly, you have
to be able to say which font to get a glyph from.  Even unicode doesn't include
all the symbols that maybe used in a score, and without font information the
score no longer shows up correctly anywhere but the original author's computer.
Thirdly, many symbols are compositions of existing symbols, e.g. a number with
a dot above it.  So plain unicode glyphs aren't going to be enough anyway.

Cons of the design are that you don't get the convenience of a full-fledged IME
for selecting characters, and that adds some overhead to each new symbol you
want to introduce, since you have to manually add it to a table.
