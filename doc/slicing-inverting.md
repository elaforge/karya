## Inverting calls

Slicing and inversion are confusing and error-prone features that are
unfortunately very useful and therefore ubiquitous.

### Overview

TODO write overview

TODO maybe I should start with slicing, then introduce inversion as a notation
shortcut

Slicing is confusing because sliced events are evaluated with less context
than it looks like they have in the score.

Inversion is confusing because it means a track shows up twice in the stack.
In general, the call order is not the same as what is on the screen.  For
instance, two inverting calls will wind up inverting until the recursion limit
is hit (in fact, the limit exists for that reason).

### Details

There are several variants of slicing, all of which are implemented in
'Derive.Slice'.

The original motivation was a "delay" transformer that causes a note to be
delayed by a bit.  A transformer can either change the environment to
encourage the deriver to produce a certain output, or evaluate the deriver and
transform the events directly.

- Transforming the events directly is a possibility for "delay", but it defeats
the abstraction offered by the environment, so calls won't have an opportunity
to react to the environment, and the delay will be uniform rather than follow
the tempo curve (of course this may be appropriate for delay, but not for
all transformations).

- Another problem is that it delays all controls uniformly.  While a delay is
certainly appropriate for the pitch track, it is probably not for a gradual
decrescendo.  This is even worse for an "echo" call: you want each echo to be
later along the decrescendo curve, not retain the bit of curve overlapping the
original note.

- A third problem is that transforming all of the signals is inefficient,
though that could probably be fixed with a hack.

So while directly transforming the score events is appropriate in some cases,
we still need to be able to transform controls by manipulating the
environment.

However, in the normal order of evaluation, controls and pitches are evaluated
before the note call since they themselves are transformers, and only change
the environment of the note deriver.  So a delay on the note is too late, the
controls have already been evaluated.  In a functional notation pseudo-code,
if you put the delay on the note you get
`dyn [1] (pitch [4c] (>inst (delay (note))))`.  In score notation this looks
like:

```
    dyn ->      * ->    >inst
    1           4c      delay |
```

The delay is too late to affect the dynamics or pitch.  What we really want is
to put the delay outside: `dyn [1] (delay (pitch [4c] (>inst (note))))`:

```
    dyn ->      > ->    * ->    >inst
    1           delay   4c      ""
```

Unfortunately, tracks don't work that way.  Only control tracks can modify
other tracks, and control tracks generate a single signal that has scope over
the entire track below it.

However, tracks *could* work that way, even control tracks.  Given:

```
    dyn ->      >inst
    1           ""
    .5          ""
```

You could view this as `dyn [1, .5] (>inst (note <> note))` (if `<>` is the
merge operator) where both notes get the `[1, .5]` signal, but it also could
make sense to interpret it as
`dyn [1] (>inst (note)) <> dyn [.5] (>inst (note))`, i.e. the `dyn` track is
sliced up for each note.  Which one is appropriate depends on the musical
context, so if `dyn` is a decrescendo curve then the first is correct, but if
it is intended to be individual dynamics for each note then the second is
correct.  This yields to the correct behaviour for a delay: if it's a
decrescendo, the delayed note should move to the quieter part of the curve,
but if it's individual dynamics, a delayed note should keep its dynamic, and
not pick up the dynamic of its successor!  So the notation needs to
distinguish between the two.

So there are two complementary extensions:

- Slicing: this slices the block horizontally along the boundaries of the
notes in the note track.  Since I don't want to slice everytihng, only
the tracks *below* the note track are sliced.  Of course, up until now there
isn't any meaning assigned to the children of a note track, so another
extension is needed:

- Inversion: this is a where a note call re-evaluates itself underneath its
children.  Only tracks below the note track are sliced, but control tracks
have to go around note tracks for them to affect the note track's evaluation.
The null note call "" is an inverting call, so you can put control tracks
below them.  In fact, most all calls that generate events should be inverting.
Inverting calls are wrapped in 'Derive.Call.Sub.inverting'.

Visually, the transformation looks like this:

```
    dyn ->      >inst ->        *
    1           a               4c
    .5          b               4d
```

[1, .5] is intended to be a decrescendo, while 4c and 4d belong to notes `a`
and `b` respectively.  After inversion and slicing we effectively have two
blocks:

```
    dyn ->      * ->            >inst
    1           4c              a
    .5

    dyn ->      * ->            >inst
    1
    .5          4d              b
```

Of course this transformation isn't useful as-is, since we could have written
in the second form in the first place (ignoring the slicing into two blocks
part).  But if we only invert the generator part of the note expression and
wrap the transformer part around the sliced children, we can transform this:

```
    dyn ->      >inst ->        *
    1           delay 1 | a     4c
    .5          delay 2 | b     4d
```

Into this:

```
    dyn ->      delay 1 | ->    * ->            >inst
    1                           4c              a
    .5

    dyn ->      delay 2 | ->    * ->            >inst
    1
    .5                          4d              b
```

Here, `delay 1 |` isn't real tracklang syntax, but represents a transformer
wrapped around its children.  In functional notation:

```
    dyn [1, .5] ((delay 1 (pitch [4c] (>inst (a))))
              <> (delay 2 (pitch [4d] (>inst (b)))))
```

Which is exactly what is needed for delay to work as expected.


### Note transformers

Slicing without inversion is called a note transformer, since it's basically a
way for a note track to take other notes as arguments.  Since the note call
isn't inverting itself below the control track children, there has to be
another note track below it.  E.g.

```
    >inst ->    >
    tuplet      a
      |         b
                c
```

In this case, the `tuplet` call (really named `t`, defined in
'Derive.C.Prelude.Parent') slices events within its duration, so it gets
`a` and `b` as arguments and is free to do with them as it wishes.  This is
how ornaments that transform multiple other notes are implemented.

As usual, there are a few more wrinkles:

- If the calling event has 0 duration it only slices events beginning at the
same time, which is useful for note transformers that don't naturally have a
use for duration.  But that may be more than one transformed event, since the
calling note track can have multiple children.  This is how arpeggio is
implemented, which takes multiple notes that start together and spaces them
out.

- What about `c`, which is not covered by the `tuplet` call?  Strictly
speaking, it shouldn't be called at all, because the events on the `>` are
only passed as arguments to the events on `>inst`, and there is no calling
event for `c`.  But in practice this would make it a hassle to add a parent
track to a note track, say to add a single tuplet, because you'd need to move
all the other notes over to the parent, which would then have a mixture of
`tuplet` calls and "normal" note calls.  So there's a special hack where
notes on a child note track that are not covered under any event on a parent
note track, they're extracted out into their own event track and evaluated
separately.  This is called "orphan extraction" and is implemented by
'Derive.Slice.slice_orphans'.

- If one of the intervening tracks is a note track that also has a call, it
will in turn insert itself below the inverted call, and this will continue
until the recursion limit is hit.  So don't do that.

- Normally control track events are sliced from one event at *or before* the
start of the parent event to one event *after* its end.  This is because
many common control track calls interpolate from the previous call, so they
need a bit of surrounding context to generate samples.  However, this isn't
always context for some calls.  A call may require a previous value to produce
a value of its own.  For instance, an interpolate call can emit the
"interpolate to" value even if there is no previous value, but a
"repeat previous value" call has no such luxury if the previous call was
sliced off.  I used to have a hardcoded `require_previous` list for those, but
at some point I fixed that... though I don't really remember how.  Maybe I just
don't have such calls any more, or they use the `NOTE [previous-pitch]`
machinery.

Note that inverting and non-inverting slicing calls can be combined, for
instance:

```
    >inst ->    > ->    *
    tuplet      ""      4c
      |         ""      4d
```

`tuplet` will slice the two events under it and receive the two null note
calls as arguments.  If it chooses to evaluate them, they will then invert
themselves beneath the `*` pitch track.


## old version

This is an old version of the doc, maybe it makes more sense:

Transformers operate on derivers.  They can change the environment to
encourage the deriver to produce a certain output.  For instance, they can
modify the pitch to transpose the derived music, or modify the warp to delay
it.

Since the controls must be evaluated and in the environment for the notes to
pick them up, by the time the note track is being evaluated it's too late to
change them.  So a "delay" transform on a note may delay the note itself but
will not affect the controls.  Sometimes this is desired, e.g. if you delay a
note during a decrescendo you may not want the decrescendo to move with the
note, but sometimes it isn't, e.g. if you delay a note its pitch should
be delayed along with it.

So what is needed is for the transform to be placed "above" the note, e.g.
`(decrescendo (delay (pitch (note))))`.  This can be written in terms of
tracks, but it's awkward, because the delay, which is logically part of the
note track, must be split into two tracks, like so:

```
    dynamics ->     transform ->    pitch ->    note
    1               delay           4c          ""
                    |                           |
    i 0             V                           V
```

It's not clear what the duration of "delay" means, and the fact that it
applies to the "" note two tracks below it.  So there's a concept of track
inversion, which is an automatic transformation from this:

```
    note    pitch
    t | n   4c
    |
    V
```

To this:

```
    note    pitch   (note)
    t       4c      n
    |               |
    V               V
```

If there are tracks below the note track, they are sliced horizontally in the
range of the note being evaluated, and the generator part of the note is put
in a new "track" below them.

Inverting calls check for the presence of subtracks, and if found, do the
slicing described above and reinsert themselves below their subtracks, hence
inverting the call structure.
