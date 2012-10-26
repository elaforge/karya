Derivation is the process that turns the UI level score of blocks, tracks, and
events into a stream of score events, which is input for the
[performer](performance.md.html).  While a UI event ('Ui.Event.Event')
corresponds directly to a bit of text in the score, a score event
('Derive.Score.Event') has a pitch signal, control signals, instrument, and
everything else that's needed by the performer.


## Score syntax

The lowest level is the score format, which is analogous to syntax in a normal
language.

The most basic structure is a "block".  Each block can have 0 or more "views",
which are just windows on a certain block (so technically this is a
[UI](ui.md.html) concept).  A block has a title, which is a box you can put
text in, and a set of tracks.  Each track can be either an event track or a
ruler.  Rulers just display hierarchical marks, and by convention each block
has a ruler in the 0th track spot, which is special in that it stays put and
doesn't scroll off the screen.  The ruler is analogous to the meter since the
the selection can aligns to it.

Derivation is mostly concerned with event tracks.  They have a title, which is
once again a place for text, and a bunch of events.

Event tracks have a title, which is once again a place for text, and a bunch of
events.  A UI event is a start time, a duration, and a bit of text (with some
other auxiliary attributes: 'Ui.Event.Event').  The events aren't allowed to
overlap, but other than that are free form.  One other detail is that there's a
"skeleton", which links together each track in a hierarchical way, so each
track can have parents or children.  The skeleton is visualized as some lines
with arrows above the tracks.

## Derivation

Derivation starts from a root block.  You can derive from any block, but for
convenience one block is marked as the root, and derivation normally starts
from there.  The eventual output of derivation is a stream of
'Derive.Score.Event's.

Tracks are arranged hierarchically, as expressed by the lines and arrows in
the "skeleton", above the track titles.  A parent track has scope over its
children, which means it sets the environment in which the children evaluate.

Tracks are divided into two kinds: note tracks, and control tracks.  Control
tracks are further divided into tempo, control, and pitch.  The type of a track
is indicated by its title.  Note tracks begin with `>`, pitch tracks begin with
`*`, tempo tracks are called `tempo`, and tracks starting with letters set
controls of that name.  Full details are in 'Derive.TrackInfo'.  Note tracks
generate events, and control tracks generate a control signal and put it into
the dynamic environment.  What all of that means will be defined later.

A block may optionally have a tempo track that scopes over the toplevel tracks.
Note tracks will set an instrument or inherit one, and each have an optional
pitch track, `dyn` dynamic control track, and possibly other control tracks.
The control tracks can either be above the note track and possibly apply to
multiple note tracks, or below a note track and apply only to that note track,
through a processes called [slicing and inversion](#slicing-and-inversion).

The events in a control track are generally numbers which set the control
signal at that point in time, or possibly calls which interpolate to create
lines or curves, the events of a pitch track will be names of scale degrees and
possible arguments, and the events of a note track will generally be empty to
generate a single note, or have an ornament name to do something fancier.  If a
note track event has the name of another block, that block will be substituted
in the place of the event---this is one way to construct scores hierarchically,
write repeats, factor out common phrases, etc.  If a sub-block has a tempo
track the tempos will compose, so a phrase with rubato built-in will also
accelerate if the calling block has an accelerating tempo.

Since control and pitch track calls are usually points in a curve, they tend
to be zero duration, while note track events tend to have a duration.  There
are exceptions, for instance a pedal or switch control might use the event
duration, while a percussion instrument may have no need for note duration.

## Dynamic environment

The dynamic environment concept is core to derivation.  The idea is that
instead of manipulating output of music generating functions (that's score
events or signals), the various score constructs may manipulate the
environment, though they may also directly manipulate the derived output if
they choose.  This is so that high level transformations can put the decision
of what exactly that transformation means to a lower level bit of score or
instrument that may then interpret it in its own way.

For instance, to reduce the volume of a phrase, instead of generating the notes
and then modifying their "dyn" signal (for "dynamics", pun not intended), you
can modify the dyn signal in the environment, and when the notes are eventually
generated they will inherit the dyn signal.  But any function in between (or
the eventual event generating function) can intercept that signal and treat it
specially, e.g. by resetting the dyn signal and reducing instrument doubling.

The dynamic environment ('Derive.Deriver.Monad.Dynamic') consists of a scope,
control signals, pitch signals, a warp signal, and a map of scalar values
called the "environment".  As its name suggests, it implements dynamic scope:
while values may not be mutated, they can be rebound within the dynamic scope
of a call.

Control signals are floating point values that change in time, normally ranging
from 0--1.  Pitch signals are similar, except the values are abstract objects
that can have other values applied to them, for instance chromatic or diatonic
transposition, and later evaluated to a normal control signal representing
frequency.  The warp signal is the same as a control signal, except it's used
to control [ScoreTime to RealTime mapping](#scoretime-and-realtime).  The
environ ('Derive.TrackLang.Environ') is different: it holds constant
[vals](#vals) ('Derive.TrackLang.Val' or 'Derive.BaseTypes.Val' thanks to
haddock bugs), but they they may be typed.  For instance, the key of a section
of music is stored as a string, or the current instrument or scale in scope is
stored as a instrument or scale respectively.  As documented in
[Calls](#calls), the environ is also used for argument defaulting.

Actually, control signals may also carry types also, for instance to document
whether a transposition signal is in chromatic or diatonic steps, or whether a
delay signal is in ScoreTime or RealTime, as in 'Derive.Score.TypedVal' and
'Derive.Score.Type' ('Derive.BaseTypes.TypedVal' and 'Derive.BaseTypes.Type',
haddock bug).

TODO: it would probably be possible to unify all the environ types into a
single map of typed signals.

The dynamic scope determines the calls in scope, and maps names to the
functions implementing the call.  Calls are documented in more detail
elsewhere, but the fact that they are in 'Derive.Deriver.Monad.Dynamic' means
that the set of calls in scope can also be modified within a dynamic scope.
There are separate namespaces for note tracks, control tracks, and pitch tracks
since the calls in each track return different types.  This is what allows an
instrument to bring into scope instrument-specific calls, or a scale to bring
into scope calls that emit its scale degrees.

When a 'Derive.Score.Event' is generated, it inherits the controls, pitch,
instrument, and [attributes](#attributes) in scope.  After that it's up to the
performer to interpret those values, which are likely also dependent on the
instrument.  Since an instrument has the power to modify the calls in scope
when it comes into scope, it may also modify the default note call (which is
the empty call "", also called the null call, documented below) to have some
special behaviour.

## ScoreTime and RealTime

Time as represented by physical location in the score is called ScoreTime.
It's in abstract units and may be shifted and stretched arbitrarily.  For
performance it has to be converted into RealTime, which is in seconds.  The
ScoreTime to RealTime conversion is yet another signal, called the warp
signal.

Event placement is handled not by directly manipulating score events after
they are derived, but by modifying the warp signal in scope, and allowing the
calls themselves calculate their RealTime as they see fit.  For instance, if a
call wants the score underneath it to evaluate so that one score unit equals
one second, it can set the warp to the identity signal.  If it wants to delay
its notes it can shift the warp signal by a certain amount, or change its
slope to stretch or compress them.  The event generating call can then map its
ScoreTimes through the warp signal, or adjust the times itself.  For instance,
a call implementing grace notes may override the warp to give them a constant
duration regardless of the tempo.

ScoreTime is implemented in 'Ui.ScoreTime', RealTime in 'Perform.RealTime'.

## TrackLang syntax

The text that appears in the block title, track titles, and events is in a
simple expression oriented language.  It has the usual literals such as
`'strings'`, and numbers, but also has music-related literals such as
`*scales`, `>instruments` or `+attributes`.  See [vals](#vals).

A call expression consists of literals separated by spaces, and the first word
is a "call", which is like a function call.  So `f 4 'hi'` is a call to `f`
with two arguments.  The first word is allowed to have any non-space characters
in it, so `5( 4 'hi'` is a call to `5(`.  Most names in the tracklang syntax
are very restrictive, but call names are especially flexible.  For instance,
it's natural for scales to use numbers as their scale degrees, or the `(` call
might represent a slur.  Call names also frequently contain
[symbols](symbols.md.html).

A full expression, as allowed in a title or event, is multiple expressions
separated by `|`.  As described in [calls](#calls), the last one is called the
"generator call" and the ones before it are called "transformer calls".

Details on the syntax are in in 'Derive.TrackLang' and 'Derive.ParseBs'.

There are a couple of hacks in the syntax to make scores look nicer:

- Null call - In fact, the call name is also allowed to be empty, so "" (the
empty string) is a valid call to the call bound to "".  Normally in a note
track this is bound to the default note-generating call so that most notes
don't need any text at all.  Transformers can still be applied to this, so for
example `d |` is an application of the `d` transformer to the null call.
Notice that you can't actually pass arguments to the null call, because as soon
as you add a character it looks like a call name itself.  Well, except for the
control track hack below.

- Comments start with `--`.  As a special hack, an event containing only `--`
is completely ignored rather than considered a null call.  This can be useful
to mark an event boundary but not actually produce a value.  For instance, some
calls may extend to the next event, and this can make them end earlier.

- An expression with an infix `=` such as `x = 42` will be parsed the same as
the prefix application `= x 42`.  The default behaviour of this call is a
transformer that sets the given value in the dynamic environ of its generator.

- Control track num expr: control track events are actually parsed slightly
differently.  Namely, an expression that starts with a number is parsed as a
null call with a number argument rather than a call.  E.g. `42` is parsed as an
application of the `42` val to "", rather than the application of a call called
`42`.  The result is that a bare number in a control track is interpreted as
passing that number to "", similar to how pitch tracks work.

### Vals

TODO blah blah 'Derive.TrackLang.Val' or 'Derive.BaseTypes.Val' thanks to
haddock bugs.

Identifier naming is more strict than most languages.  Only lowercase letters
a-z, digits 0-9, and the hyphen are allowed.  IDs also allow \`s so BlockIds
can embed symbols.  This enforces a lowercase-with-hyphens naming scheme.  The
exact definition is in 'Derive.ParseBs.p_identifier'.

The intent of the restrictive rules are that they relieve me of the burden of
remembering a naming scheme (e.g. '.' vs. '-' vs. '_') and leave room for
flexibility in other places, e.g. @-macros in the [REPL](repl.md.html).

TODO document literal suffixes, e.g. 1s 1t 1d 1c, control track suffixes

Numeric values can have a type suffix.  For instance, 3d means that the number
is intended to be interpreted as a transposition of 3 diatonic steps, and `2s`
is 2 seconds of RealTime.  Similarly, control names can have a type suffix,
such as `delay:s`.  This means that the values of the control are intended to be
interpreted as 3 RealTime seconds.

Of course, whether or not they are so interpreted depends on the call that
winds up receiving them.  Calls that expect an amount of transposition will
accept `d` and `c`, and untyped numbers will be interpreted as `c` (chromatic).

Many numbers and controls are untyped and will ignore any type on a value or
control.  The type mechanism is intended only as a way of overloading certain
calls.  For example, it would be awkward to include all combinations of trills
on diatonic or chromatic neighbors, and in score time or real time.  It's
easier to have one control that can be passed a diatonic or chromatic neighbor.

All type codes are enumerated in Derive.BaseTypes.Type.

The interaction between typed controls and arguments is also documented in
'Derive.CallSig'.

## Calls

Calls are the tracklang version of functions.  Almost every bit of text in the
score is a call expression.  Calls are classified by the type of value they
return: a NoteCall returns score events, a ControlCall returns a control
signal, a PitchCall returns pitch signal, and a ValCall returns a tracklang
Val.  Each of note, control, and pitch calls are only in scope in their
relevant tracks, but val calls are in scope in all tracks.

Notice that NoteCall is inconsistently named, it should probably be named
EventCall along with EventDeriver.  The root of the inconsistency is that at
the UI level event track means any track with events in it, while the
derivation level distinguishes between event tracks that emit signals and those
that emit score events.  Since calling a track that emits score events an
"event track" would be confusing since it's more specific than UI's "event
track", I call it a "note track" and the name of the call found on that track
follows suit.  Phew.

A call may have zero or more arguments, which are parsed as
'Derive.TrackLang.Val's.  Argument parsing and the defaulting scheme (which
uses the dynamic environ) is documented in 'Derive.CallSig'.

The syntax of a call expression is documented in
[tracklang syntax](#tracklang-syntax), but the gist is that it looks like this:
`t1 | t2 (v1 1) | g 'arg'`.  `t1` and `t2` are transform calls since they come
before a pipe and `g` is a generator since it occurs at the end.  `v1` is a val
call whose result is passed to `t2`.

Furthermore, calls are divided into generator and transform calls.  Generator
calls produce values of their appropriate type, while transform calls are
applied to other calls.  The call they are applied to is actually unevaluated
at the time of application, so the transformer can modify the environment in
which the transformee is evaluated, or evaluate it and transform its output,
or even not evaluate it at all.  For example, a echo transformer could
evaluate its generator multiple times with shifted warps and merge the
results, or it could evaluate the generator itself and directly modify the
produced score events.  The former is like a musical echo and will stretch
with tempo changes and allow the notes to change their derivation based on the
reduced dynamic, while the latter is like a physical echo and will produce
notes at the given interval regardless of the tempo.

All tracks except note tracks are analogous to transform calls even though
they are implemented differently, since they evaluate a signal and put it in
the dynamic environment of the tracks below them.

Transform calls and generator calls live in their own namespaces, though they
are bound to their names together (i.e. a call is a (Maybe TransformCall,
Maybe GeneratorCall) pair: 'Derive.Deriver.Monad.Call'.  The exception is
'Derive.Deriver.Monad.ValCall's, which don't have transformer and generator
versions.

In addition to the syntax hacks, call evaluation also has some:

- ValCall fallback: If a generator call is not found in the appropriate scope
for the track (note, control, or pitch), the evaluator looks for a val call
with that name.  If one is found, it is evaluated and the result is passed to
the null call.  This is used to implement pitches: a pitch like `4c` is
actually a val call that returns the appropriate `Derive.PitchSignal.Pitch`.
Therefore it can be passed as an argument to another call (e.g. `tr (4c)`),
but if it occurs alone in a pitch track its Pitch is passed to the null call,
which for pitch tracks just sets the pitch signal to that pitch.

Evaluation is implemented by 'Derive.Call.apply_toplevel'.

## track evaluation

Each kind of track is evaluated in its own way, but they are all very similar.

The main difference is in how the titles are interpreted.  The parsing is
documented in 'Derive.TrackInfo.parse_control', but here's an overview:

- Control tracks look like `control`, `add control` or `%`.  A plain word
replaces the named control in the environment with the signal from the track.
`add control` instead adds the signal to an existing one.  There are other
functions like `sub`, `mul`, `min` and `max`, documented in
'Derive.Deriver.Monad.default_control_op_map'.  `%` is an unnamed control
track and is used only by control block calls, as documented below.

    You can optionally append a pipeline expression after the control name,
and the expressions will be called as a transformer around the whole track.
So `add c | t1 | x = 42` will evaluate the track with `x` set to `42` and
transformed by `t1`, and then add the resulting signal to the `c` control
currently in scope.

- Pitch tracks look like `*`, `*scale` or `*scale #name`.  They set the scale
to `scale` and evaluate the track as a pitch track.  A plain `*` will reuse
the scale currently in scope.  Setting a scale also has the effect of bringing
the scale's calls into scope, as documented under Scales.  The generated pitch
signal will replace the default unnamed pitch signal in scope, unless you give
a `#name`.  There is no facility for combining pitch tracks as there is for
control tracks.  Relative pitch signals are instead implemented by a control
signal that represents chromatic or diatonic transposition.  What those are
depends on the scale, but the defaults are 'Derive.Score.c_chromatic',
'Derive.Score.c_diatonic' and 'Derive.Score.c_hz'.  As with control tracks,
you can append a transformer pipeline.  Pitch tracks are highly related to
[scales](#scales).

- Tempo tracks are just titled `tempo`.  The track is just a normal control
track, but the generated signal will be composed with the warp signal in
scope.  Normally a single tempo track will have scope over all the tracks in a
module, but it's also possible to have multiple tempo tracks.

- Note tracks look like `>` or `>inst` or `>inst arg1 arg2 ...`.  They are
passed as arguments to a `note-track` transformer whose default behaviour is to
set the current instrument and possibly attributes, the same as the
[default null note call](#call-docs).  Similar to scales, setting the
instrument will bring the instrument's calls into scope, as documented under
[Instruments](#instruments).  As with control tracks, you can append a
transformer pipeline.

When a track is evaluated, the text of each UI event is evaluated, and the
results from each call are merged.  Any evaluation errors will be logged and
abort the evaluation of that event.

In addition to the arguments, each call actually gets a whole bunch of other
information ('Derive.Deriver.Monad.CallInfo').  Notably, it gets the event
start and duration, along with previous and subsequent events, so calls know
where their neighbors are.  It also gets the last value of the previous call,
which is used by control calls, some of which want to interpolate from the
previous value.

### call docs

Documentation for individual calls is included in the calls themselves, and is
extracted and formatted by 'Cmd.CallDoc'.  'Cmd.Lang.LBlock' provides some
functions to emit documentation for the calls in scope at the cursor.

TODO I should emit documentation for all builtin calls and link it from here.

### Block calls

As mentioned in the overview, a note call with the same name as another block
will substitute the contents of that block, stretching and shifting it into
place.  If the sub-block uses relative pitches and controls the generated
notes will wind up depending on the calling environment.

Block calls are note calls and go on a note track, but there's also a variant
for control tracks.  The sub-block is expected to have a control track titled
`%` whose signal will be substituted into the signal of the calling track.

### Slicing and inversion

[Slicing and inversion](inverting_calls.md.html) is a score level
transformation that happens at when a track is derived.

### Note Transformers

### Integration

## Instruments

### Attributes

Instruments can also have attributes attached to them.  Attributes are just a
set of strings, that are intended to be interpreted by the performer based on
the instrument.  For instance, a "pizz" attribute may cause an instrument that
understands it to emit a keyswitch to play the affected notes as pizzicato.
Attributes can also be used by percussion, e.g. +sn for a snare, or +hh for
high-hat.  Drums are also likely to support combinations of attributes, such
as +hh+open.  Attributes can be any string, but a set of standard names is in
'Derive.Attrs'.

Attributes are a set, so you can add and remove individual attributes.  For
instance, `+pizz` is a literal representing the addition of the "pizz"
attribute, and putting it around a call will presumably cause everything that
can play pizz to play as pizz, unless it's cancelled out with a `-pizz`.  If
`+tremolo` is already in effect, the instrument can apply them both
simultaneously (unlikely in the case of +pizz+tremolo, but many sample
libraries do have combinations like +cresc+tremolo), or decide based on
priority which one to apply.  Details are in
'Perform.Midi.Instrument.KeyswitchMap'.

### Local.Instrument

[doc/local](local.md.html)

## Scales

talk about how abstract transposition works

caveat about how you can't write `%t-chromatic = 1 | 4c`

## Logging
