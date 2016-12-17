This is a list of more fundamental problems with the whole track-oriented form
of notation.  Most of these I don't really have a solution for, but if I write
them down it gives me a centralized place to think about them.

### Code vs. data.

There is a constant tension between code and data.

For example, it turns out a lot of calls need to know what the next pitch
is.  If the score and pitches were data, it would be trivial to just walk
forward one step and see what it is.  Since they're both code, I have to
think of a way to evaluate the next note... which is not trivial because
a lot of context can affect the evaluation, such as parent calls or
transposition, or it itself can depend on a neighbor pitch.  But that
very flexibility is stuff that would make a score data type impossibly
unwieldy, thus losing the benefit of easy analysis.
Similarly, `norot` not only wants to know what the next pitch is, but if
the next note is also a norot.  If there was an Ornament data type that
included Norot, I could just look at it, but since it's a call it's not so
simple.  Of course I could look at the name, but names can be rebound, and
in fact that ability is essential.  But of course a score data type
wouldn't have a Norot type because I can't really make an exhaustive
enumeration of every single ornament for every single kind of music.

However, at higher levels of abstraction, there definitely is a kind of
grammar, and I can definitely make a useful data type.

I should be able to express score at multiple levels of abstraction, e.g. a
high level without time information, a medium level with general things
partially realized, and a low level with explicit time.  The current score is
basically "low level" only.  Even though it tries to have high level ornaments,
it's fundamentally limited by the time thing.

### Order of application.

`unison | realize-gangsa` vs `realize-gangsa | unison`?  In fact,
`realize-gangsa` is just a macro for `realize-noltol | cancel-pasang |
realize-ngoret`, bundled together to make it easier to get the order right.
If `unison` goes first, it splits a pasang note into polos and sangsih,
and then `cancel-pasang` has to know that a polos note can cancel both...
which is does, which is why it has that name.  If it goes in the other way,
I don't need a specialized `cancel-pasang`, but then I wind up having to
put `unison` at the top level, which is not musically suitable, because
I don't want everything to be unison all the time.  The same thing goes for
`realize-gangsa`, they have to be in that order, or the results will come out
wrong.

The reason all these postprocs exist in the first place is related to the
underlying problem, which is that many transformations have to happen in a
specific order, and that order is not necessarily the same order as its
convenient to put them in the score.  So there are a lot of mechanisms for
delaying evaluation, e.g. the `ngoret` call figures out timing and puts special
flags on the note, which `realize-ngoret` later uses to figure out pitch and
damping.  But the result is that there are more and more "realize" calls,
and they all have to go in the right place.

I can't think of any ways to actually solve the problem.  I supposed I could
analyze the input and infer the realize calls automatically, but it sounds
like working around complexity by adding more complexity.

### No time abstraction.

Also the way blocks have nested tempo is often not what I want.  It seems to
be more useful to just append times.  I would like to be able to add some
beats in the middle of a block and not have to resize everything above it, or
comment out some section and have everything move back to fill in the space.

This also applies to controls.  E.g. often uncab will happen either right after
a beat, or right before one, but because the dyn track require a concrete time,
I have to put that in manually.  Also because it's not associated with any one
note, I have to line it up manually.  What I would prefer is to use the dyn
track for overall dynamic changes, and then events like uncab I can mark and
each instrument can interpret as it sees fit.

### No track abstraction.

I can't express cross-track things, like angsels.  The actual expression of the
angsel needs detailed per-instrument information, but it cuts across many
instruments.  I can put the angsel in its own block, but then I wind up putting
the calling event on some arbitrary track.

### Complicated track structure

- makes copy paste annoying
- adding a new track still feels heavyweight

#### track order structure

Tracks can carry information which isn't relevent to the music.

For instance, the order doesn't have an effect on sound.  Actually, this is
just the toplevel order, since a parent call can use the order of its children,
e.g. arpeggio.  Staff notation has this same problem, but addresses it with a
conventional order.  I could do the same thing if I have a standard instrument
set by automatically sorting.

I've tried various extensions that make use of track order, such as inferring
voice number or hand, but I more or less decided that it's too implicit and
I'd rather be explicit and make the voice or hand declaration short, e.g. `v=2`
instead of `voice=2`.  It's true staff notation uses implicit order, but maybe
it can get away with it because it has a more restricted structure.  Also it's
just more familiar, an established notation can get away with more.

And maybe I'd like to keep track order for expressive purposes, e.g. to put
related parts next to each other.  It can make it hard to read though.

### parent notes rely on an implicit time connection

If I want to apply a transformation to a set of notes, I have to either
abstract them into a block, which requires a name, or use a parent call, which
requires a new track.  The latter isn't such a burden if I already have that
track, but then I have to make sure the parent event's extent exactly
encompasses the transformed notes.  I'd rather say "this transformation applies
to this set of (unnamed) notes", which is trivial in a normal language, but
instead I have to say "this transformation has this start and duration, which
happens to correspond to the extent of those other notes underneath."  It's
not only more work to create it, but then I have to keep it up to date and
extend or shorten it when the transformed notes change.

Some UI conventions can help, e.g. select both parent and child when I do a
delete or add time.  Other than that, I think the only way to avoid this is to
refactor the notes to a block, which is supported by Cmd.Factor, and more
directly analogous to a function call in a traditional language.  But a block
call is still out of line, so it's harder to read, and needs a name.  It's
like a language with no nested expressions, and you have to make a function
and name for everything.  Parent calls are the closest I could think of
to a traditional nested expression.  Once again, time being built into
everything gets in the way.

### The cache is hacky and can be inaccurate.

### Notes that should ring across block boundaries.

### UI lag

UI lagginess when editing notes is a continual problem.  It tends to get
clunky after editing for a while.  Likely there are memory leaks which cause
GC to get more expensive.  Also in the past I've accidentally blocked the UI
thread by unevaluated but expensive thunk.

Part of this endemic to any interactive program, but Haskell's laziness makes
it worse.  It's also hard to troubleshoot since it only tends to come up after
editing a score for 30m or so, which is both too long to reproduce and not the
time I want to get distracted by a bug hunt.

Aside from periodic profiling runs and interactive ekg, I could come up with
some more automatic way to reproduce and ideally catch regressions.  For
example, I could do a bunch of random editing automatically and check memory
usage and latency.

Cmd/MemoryLeak_profile.hs looks for both memory leaks and bad latency, so it's
a start.
