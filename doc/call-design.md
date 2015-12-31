## Call design

The design of the various musical ornaments requires some thought to make sure
they work together.  The rules are different than standard programming, and
I'm inventing them as I go along, so I write some down here to develop my own
ideas if nothing else.

Composition is very important to keep the score expressive while limiting the
number of calls.

### general principles

- Calls should generally be divided into high level and low level.  Low level
calls are concrete, e.g. interpolate between two values with a given curve.
High level calls are in musical terms, e.g. slur these three notes.  Low level
calls should be general purpose and orthogonal, but high level ones can be
specific to a particular kind of music or piece.  Another use for high level
calls is that they may be dynamically bound to a particular concrete
implementation.  E.g. the score uses a `tr` note call, but the caller binds it
with `>tr = special-trill`.

- Ask lower levels to do things, instead of doing them yourself.  And ask them
using a higher level method, if available.  So e.g. instead of shortening a
note with postproc, shorten it with `Derive.stretch`.  Or instead of that,
shorten it by setting 'Derive.Controls.sustain_abs', which the default note
deriver will pay attention to.  Another example is that `g`, the grace note
call, applies its generated notes to the `(` call, which should be bound to do
the appropriate thing, whether that be overlap notes, bend pitch, or emit a
lilypond slur.

- Sometimes a call has relatives with the same name but a different type.  For
instance, there is a note call `tr`, a pitch call `tr`, and a control call
`tr`.  They are at different levels of abstraction and have different pros and
cons.  For instance, the only note call `tr` can do a fingered trill, and emit
lilypond.  It also knows the bounds of the note, but can only be applied to a
whole note.  The pitch call can emit chromatic or diatonic intervals, and it
is given a specific start and end time, so it can be in the middle of a note,
or span multiple notes.  The control call is the lowest level, but works for
any control, including 'Derive.Controls.chromatic' and
'Derive.Controls.diatonic', but itself has no control over where it winds up.

- Similarly, if applicable, a generator should have a corresponding
transformer, and vice versa.  All transformers can be written as a
corresponding note transformer, and this is frequently useful to apply a
transformation to multiple notes without needing to refactor them into their
own call.  E.g. the call to add an attribute `+a` can be written either as
`+a |` on a single note, or as a note transformer scoping over several.  The
`ap` call (for "apply", but it's a crummy name) is a generic note transformer
that simply derives its sub-notes, so any transformer can be applied to
multiple notes via `transform | ap`.

- Conciseness is important.  But in general control names and environ names
should be complete words, not single letters or abbreviations.  This is because
if you want to write something down frequently, there should be a call for it,
and that call can have a short name.  Also, setting a control or env var is
lower level, a call can set a control, but not the other way around.
Standardizing on calls means you always write `d 5`, and don't have to remember
if it should be written `%d = 5` or `d = 5`.

    For example, an env var to get calls to delay their attacks is called
`delay`, and if you use it frequently you should have a transformer `d 5`
rather than writing `delay = 5` everywhere.

- Too many arguments becomes hard to use.  You can effectively write keyword
args using the environ defaulting mechanism, e.g.
`kam (4s) 1 8 .1` becomes `speed=8 | transition=.1 | kam (4s) 1`

- The call argument environ defaulting frequently uses both a prefixed and
non-prefixed form.  For example, the `grace` call has a `place` argument,
which can be defaulted by either `grace-place` or `place`.  The short form
will override the long form, so it's useful for a local assignment, while you'd
use the long form for a global assignment.  Also, several calls may share the
same unprefixed argument name, which is generally ok.  For example, many
calls have a `dur` argument, so if you set `dur=x` it will default whatever
is the logical duration for that call, while if you set `call-dur=x` it will
specifically address that call.  Calls which are variants of each other may
even use the same call name so that many of their arguments will coincide.

    Of course this means that you can't conveniently set a separate global
default for one of those, which might be a problem if the point of the separate
variations is to be able to have separate defaults, but so far I haven't had
that problem.

    To support defaulting, and to avoid having to memorize too many argument
names, calls should try to stick to a standard vocabulary for argument names,
e.g. `dur` for duration, `time` for a time interval, `pitch` for main pitch,
etc.


### conventions

- Calls with arguments to do fancy effects should do the simplest thing when
given no arguments.

- If there are variants that affect the beginning and end of a note, the one
that affects the beginning is lowercase, the one that affects the end is
capitalized.  Often they are not the "same" transformation, but a kind of dual.
E.g. `d` delays the start, and `D` shortens the end.  `drop` starts at a higher
pitch and drops to the note's pitch on the attack, while `Drop` drops the
note's given pitch to a lower pitch at the note's end.

- Suffix^ for high variant, suffix_ for low variant.  E.g. `tr^` and `tr_`.

- Pitch calls take their "base" pitch (what that is exactly depend on the call)
as their first argument.  This is assumed by 'Cmd.PitchTrack.modify_note'.
