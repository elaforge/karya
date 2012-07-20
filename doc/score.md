## Score

Karya can be seen as two things: a language used to express music, and an
editor for that language.  It's a bit like an IDE for a music language.

The score language, like any language, can be described in several layers.

The lowest level is the syntax.  The most basic structure is a `block`.
Each block can have 0 or more `views`, which are just windows on a certain
block.  A block has a title, which is a box you can put text in, and a set of
tracks.  Each track can be either an event track or a ruler.  Rulers just
display hierarchical marks, and by convention each block has a ruler in the
0th track spot, which is special in that it stays put and doesn't scroll off
the screen.  The ruler is analogous to the meter since the the selection
usually aligns to it.

Event tracks have a title, which is once again a place for text, and a bunch
of events.  An event, at the simplest level is a start time, a duration, and a
bit of text.  The events aren't allowed to overlap, but other than that are
free form.  One other detail is that there's a "skeleton", which links together
each track in a hierarchical way, so each track can have parents or children.
The skeleton is visualized as some lines with arrows above the tracks.

That's the basic syntax.  The Deriver is the part that's responsible for
turning it into a performance.

## Tracklang

The text that appears in the block title, track titles, and events is in a
simple expression oriented language.  It has the usual literals, such as
`'strings'`, and numbers, but also has music-related literals such as
`*scales`, `>instruments` or `+attributes`.  Numeric literals can take type
suffixes, so `.5d` is half of one diatonic step, and `3r` is 3 seconds of
RealTime.  An expression consists of literals separated by spaces, and the
first word is a "call", which is like a function call.  So `f 4 'hi'` is a
call to `f` with two arguments.  The first word is allowed to have any
non-space characters in it, so `5( 4 'hi'` is a call to `5(`.

Details on the syntax are in in 'Derive.TrackLang'.

The interpretation of the score is called [derivation](derivation.md.html) and
is complicated enough to warrant its own document.
