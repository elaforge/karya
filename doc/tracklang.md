### notes

The weakness of the score language is also its strength: it's designed to be
general purpose and flexible, but this means it can be more awkward than
notations specialized to one particular kind of music.

It's poor at expressing chords.  Staff notation lays them out visually which
makes them instantly recognizable as patterns.  But this compactness is at the
cost of specializing to a particular kind of scale, and having no place for
detailed control over pitch or dynamics.

Similarly, note heads and flags can very compactly represent multiplicative
rhythms, but turns into a mess of ties for additive rhythms, and can't express
small variations at all (small variations are expected to be supplied by the
performer).

Also, staff notation's free form graphical symbols are expressive but make it
difficult to lay out mechanically, and lack the generality of text.

Tracks introduce another level of complexity that is orthogonal to the
actual music.

Piano roll notation gets around this by effectively having one track per
pitch.

As does staff notation.


The score language is code to to be interpreted, not data to be manipulated.
This is good for flexibility, but bad for direct transformation.  There are
three levels addressing the problem of transformation:

1. The best way is to express transformations directly in the score as calls,
rather than manipulating the score directly.  The advantage is that calls
retain high level structure and can be parameterized.  E.g., express legato
with a legato call rather than stretching note durations in the score.  The
legato can then be adjusted globally, or can have different effects depending
on the instrument, or draw a slur in the lilypond backend.

2. The weakness of calls is that they can't well express ad-hoc
transformations.  E.g., if the amount of legato overlap varies for each note it
can be expressed as an argument to the legato call, but it defeats the purpose
of a graphical score language if it devolves to a list of numbers.  Integration
provides a middle ground by allowing you to apply a call transformation and
then further edit the output.

3. However, integration still can't deal with transformations that are diretly
concerned with the structure of the score.  For example, the presence of
different tracks and the fact that the same note can appear on different tracks
is an artifact of the notation, and is gone after derivation.
'Cmd.ModifyEvents' is intended to fill this niche.

