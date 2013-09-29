[back to overview](overview.md.html#karya)

## Cmds

Technically, a Cmd is anything in the 'Cmd.Cmd.CmdT' monad.  The Cmd monad is
basically just a state monad on 'Ui.State.State' and 'Cmd.Cmd.State', which
means a Cmd is a function that can modify the score or the app state.

Bound Cmds are just functions that take a 'Cmd.Msg.Msg' and decide whether or
not to do something based on that: 'Cmd.Cmd.Cmd'.  Most of them are bound in
'Cmd.GlobalKeymap' and track-specific ones are bound from 'Cmd.Track'.  The
keymap bound cmds are summarized in [the keymap doc](keymap.html), and the
track commands mostly depend on the EditMode.

However, the vast majority of Cmds are accessible only from the
[REPL](repl.md.html).

## Tracks

Tracks are indexed by 'Ui.Types.TrackNum', starting at 0 on the far left.  In
practice though, the 0th track is usually the ruler, so the "real" tracks start
at 1.

A track can be addressed either by a (BlockId, TrackNum) pair, or directly by a
TrackId or RulerId.  The functions in 'Ui.State' enforce that a given TrackId
can only appear in a block once, so you can convert between TrackId and
TrackNum.  The TrackNum is actually more general, in that it may address a
ruler or divider, so functions that expect an event track will generally take a
TrackId.

The actual meaning of the tracks is defined by
[derivation](derivation.md.html#derivation), the documentation
here is just the low level, without reference to what it means.  This is
analogous to syntax in a normal language.

There are three kinds of tracks: divider, ruler, and event.

### divider track

'Ui.Block.Divider's are just visual spacers.  All they have is a color and a
width.  You can add one by hand with 'Cmd.Repl.LBlock.divide' to visually
separate tracks, and they are used automatically to represent collapsed tracks.

### ruler track

The ruler by convention is in TrackNum 0, which on the far left, and is special
in that it stays put and doesn't scroll right to left like the other tracks.
It provides a visual reference for the rhythmic structure and is analogous to
the meter.  If you are so inclined, you could put an event track in TrackNum 0,
perhaps as a reference.  Or you could have multiple rulers, which is analogous
to multiple simultaneous meters.  However, cmds that deal with rulers generally
expect only one ruler per block, and that it be in TrackNum 0, so you might
have to make those smarter.

A 'Ui.Ruler.Ruler' has 'Ui.Ruler.Marklist's, which is just a list of
'Ui.Ruler.Mark's.

Technically a ruler can have marks in any kind of arbitrary pattern, but
typically you'd want them dividing up time according to a meter or tala or
whatever system you use to organize time.  'Cmd.Meter' has utilities to
describe Western-style meters, while 'Cmd.Tala' has Carnatic talas.  Normally
you'd use the functions in 'Cmd.Repl.LRuler' to modify the ruler or create new
rulers.

While ruler tracks have a ruler only, event tracks have rulers too, which show
up as transparent lines.  Normally all tracks in a block have the same ruler,
but if you want you can set up different meters for different tracks, in the
same way you can have multiple ruler tracks.

Since rulers are addressed by RulerId, they have an identity and are shared.
So you need to be aware if you are modifying an existing ruler, which will
change all blocks with that ruler, or if you are creating a local copy.  The
'Cmd.Repl.LRuler.modify' and 'Cmd.Repl.LRuler.local' functions apply a ruler
modification destructively or to a local copy, respectively.  Remember to
'Cmd.Repl.LRuler.gc' to delete orphaned rulers.

### event track

Event tracks have 'Ui.Event.Event's.  They are divided into several types,
differentiated by their titles.  Details are in the [derivation
doc](derivation.md.html#track-evaluation).  Since this is the track that
actually holds score data, and which you spend most of your time editing,
"track" usually means an event track, not a ruler.  Usually I say "note track"
or "pitch track" or "control track", all of which are event tracks at the low
level, but are distinguished by their titles and treated differently by cmds
and derivation.

Of course, to be visible, a track has to live in a block.  The same track,
identified by its TrackId, can be in multiple blocks at once, but it can only
appear once in each block.  This restriction is for convenience, since
otherwise just a (BlockId, TrackId) pair wouldn't be enough to identify a
track.  Note that when you remove a track from a block, the track still
exists, and is accessible via its TrackId.  'Cmd.Repl.LTrack.gc' will delete
all tracks that aren't referenced by any block.

The data inherent to a track is in 'Ui.Track.Track', and data which may vary
per-block is in 'Ui.Block.Track'.  The most interesting is a set of
'Ui.Block.TrackFlag's, which affect display, derivation, or performance.

One special case is merged tracks.  Since note calls frequently have no text
and just mark a start time and duration, and control calls frequently have
text but no duration, you can save space by merging the neighbor pitch or
control track into the note track.  Merging is a purely display feature, where
the text from the events in one track will be displayed right-aligned in
another track.  By convention (i.e. what the default keybindings do), the
merged track is the adjacent pitch track, which is then collapsed.

#### note track

[Note tracks](derivation.md.html#note-track) actually produce score events.
The cmds are implemented and documented in 'Cmd.NoteTrack'.

Because most instruments are pitched, note track cmds listen for pitches
(either from the MIDI keyboard or [kbd entry](#note-entry)) and enter pitches
on a neighboring pitch track, or create one if it doesn't exist.  So they
mostly act like pitch tracks.  But ValEdit on note tracks also supports
'Cmd.Cmd.state_chord' to enter chords easily, and
'Cmd.Cmd.state_record_velocity'.

#### pitch track

[Pitch tracks](derivation.md.html#pitch-track) produce a pitch signal, and the
cmds are in 'Cmd.PitchTrack'.  They're mostly concerned with turning pitch
input into the named pitch for the scale in scope.

#### control track

[Control tracks](derivation.md.html#control-track) generate
'Perform.Signal.Control's, which are numbers that vary in time.  The cmds are
in 'Cmd.ControlTrack'.  By convention they are normalized between 0 and 1,
which makes them multiply together nicely, which is convenient, because that in
fact is what they do by default.  But they are not restricted, and for example
a control indicating the amount of delay, or the depth of a trill would have no
reason to stay within that range.

But since they mostly are normalized, the default call to set a control value
is a bit weird, though it may be familiar if you've used a tracker.  ValEdit
will accept hex "higits" and replace any existing number.  The value is
superscripted with an `x` to indicate that it's in hex, and is divided by 0xff
to normalize it.  This makes it fast and convenient to type normalized
numbers, makes them all the same physical width, and doesn't rely on a tiny
decimal point to determine their magnitude.  But you can still use [raw
input](#raw-input) enter decimal numbers if you like them, or need numbers
outside the 0--1 range.

#### tempo track

[Tempo tracks](derivation.md.html#tempo-track), also implemented in
'Cmd.ControlTrack', are in most respects normal control tracks, except of
course they are treated differently by the deriver.  One wrinkle is that
they're not normalized between 0--1, so they don't do the hex input thing.

### Signal render

Tracks can display a visual representation of a signal, namely the signal
which that particular track generates.  You have your choice of
'Ui.Track.RenderConfig's, and it works on pitch tracks too.  Note tracks are
handled separately, as documented in RenderConfig.

## selections

Selections are differently-colored transparent rectangles that are drawn in
event or ruler tracks, starting at some time and ending at some other time.
At the lowest level, that's all they are, but of course cmds establish some
conventional meanings for them.  The most frequently used is the insert
selection, which corresponds to the "edit point selection" found in most
programs, but there are others, configured and documented in
'App.Config.insert_selnum'.

Selections can have zero duration, at which point they're called "point
selections" and in the case of the insert selection, some cmds may behave
slightly differently, e.g. act upon an overlapping or previous event rather
than the events strictly contained within.  It's all rather ad-hoc and
complicated, but is hopefully what you expect, as long as you're
sufficiently like me.

## TimeStep

Besides visual reference, rulers also provide snapping points for the insert
selection.

A 'Cmd.TimeStep.TimeStep' describes a time interval, some of which are in terms
of ruler mark ranks.  Utilities in 'Cmd.Meter' can create and modify rulers
with appropriately spaced marks, which cause the various timesteps to
correspond to whole notes, quarter notes, etc.  The short form of a TimeStep as
emitted by 'Cmd.TimeStep.show_time_step' uses mnemonics like `w`, `h` and `q`,
and in Western meters these generally correspond to whole, half, and quarter
notes, but not necessarily exactly, as documented in 'Cmd.Meter'.

## EditMode

'Cmd.Cmd.EditMode' is a global mode setting, which is somewhat analogous to
vi's modes.  Basically, each mode enables different editing oriented cmds.

The current edit mode is displayed in the edit box, above the ruler.  The
mapping is in 'Cmd.Internal.sync_edit_box', but basically a color means you're
in some edit mode, and gray means you're in command mode.

The precise effect of each edit mode depends on the track:

In ValEdit mode, number keys on a control track will enter numbers.  A pitch
track will listen for notes, either from a MIDI keyboard or from kbd input, and
will create the appropriate pitch.  A note track will create a note along with
an entry in the pitch track, creating a pitch track if the note track doesn't
already have one.

MethodEdit is for control and pitch tracks.  It's just a way to edit the call
of the control or pitch (which is itself a call, but never mind that).  For
example, you would turn on ValEdit, type a number like `1`, then switch to
MethodEdit (tab, by default) and type `i` to wind up with `i 1`.  What that
means, of course, is documented in [control calls](calls.html).

### raw input

RawEdit just appends whatever text you type directly to the event, so it's the
only edit mode which acts the same on all tracks.  It's mostly made obsolete by
edit input, so it might go away someday.

Edit input is technically not a edit mode at all, but it's used to enter or
alter text.  For example, the "append text" cmd (bound to 'a' by default),
brings up a text input to edit event text directly.

Events created on control and pitch tracks will normally be zero duration.
Events on a note track will use the duration of the current TimeStep, but if
you select a range of time and then create an event, the event will be exactly
that duration.

### note entry

Note entry mode ('Cmd.NoteEntry') is actually orthogonal to EditMode, in that
it can be enabled and disabled independently, but is conceptually also a mode.
It turns the ASCII keyboard into a music keyboard by remapping the keys to
emit pitched notes.  It's often more convenient to use a MIDI keyboard, though,
because then you get to keep the computer keyboard for cmds.

But since the ASCII keyboard is actually analogous to a 10-key per octave
keyboard with black keys in between each white key, it can be more convenient
for scales with non-piano layouts.  This also allows it to behave slightly
differently, for example relative scales always start at `z` and `q` (the
"middle c" of the ASCII keyboard), even for the piano style scales, because
the ASCII keyboard allows black keys to be anywhere.

An input note, whether via MIDI or ASCII keyboard, is represented by a
'Perform.Pitch.Input', which has more information about how they're
represented.

## Integration

The key to having a high level score is to factor out repeated aspects.
There turn out to be a lot of different ways things are repeated in music, and
most of the complexity in Karya is concerned with different ways to factor out
repetition.

Calls are one approach.  A single ornament or idiomatic phrase can be factored
into a call, and parameterized with arguments and signals.  Similarly, a
section and a whole score is factored into a block call, and can still be
varied by means of different signals, tempo, etc.  If factoring fails, there's
always copy and paste, and since music often has repeats with small
idiosyncratic differences, factoring fails often.

Integration is a middle ground.  It takes the output of derivation and
integrates it back into the score.  It also keeps track of the source, and
when that changes will reintegrate back into the destination.  There is a
simplistic merge algorithm described in 'Cmd.Integrate.Merge'.  Each event
keeps track of its source, and can detect position, duration, and text changes,
but it's probably not terribly difficult to confuse it.

Since track structure is lost during derivation, integration has to recreate
it.  It's described by 'Cmd.Integrate.Convert.TrackKey', but basically it
tries to keep the source tracks and their order, while splitting overlapping
events and differing instruments.  It also splits on the 'Derive.Environ.voice'
variable, and calls that expect their output will be integrated can set this
to explicitly divide their output events into separate tracks.

As a visual reminder, events that were integrated from a source have their text
in italics, and it will be bold if the event hasn't been changed.

### block integration

Block integration is triggered by adding a `<<` as a note transformer.
It only works if you add it to a block title, because
'Ui.Block.block_integrated' keeps track of block integration by BlockId.

This will automatically create a new block with its contents as seen by the
transformer.  You can create another block manually with
'Cmd.Repl.LIntegrate.block'.

As a visual reminder, the status bar of an integrated block will have a red
background.

### track integration

Track integration allows one track to act as the source for other tracks on the
same block.  It's basically the same story as block integration, except you add
the `<` transformer to a note track title, and it will create a set of
integrated tracks on the same block.  Unlike block integrate, the `<`
transformer will not pass the events through, so the integrated track itself
won't sound when you play it.

Similar to block integration, adding the track integrate transformer
automatically creates new tracks if there were none, and
'Cmd.Repl.LIntegrate.track' will create additional tracks.

The integrated tracks are linked back to their source with red arrows.

## Scales

'Derive.Scale.Scale's have a Cmd component, since how to input and modify
scale degrees depends on the scale.  Each scale has a bunch of ad-hoc
functions to support cmds that work generically across all scales, like
symbolic transposition or mapping an input key to a scale degree.

There is quite a bit of complication in scales relating to enharmonics,
spelling, keys, and the like, much of which is documented in the support
modules like 'Derive.Scale.ChromaticScales' and 'Derive.Scale.Theory'.  If you
want to manipulate pitches in a higher-level way than just an opaque symbol,
you'll need to use the various functions used to construct the scale to parse
the degrees and get at their internal structure.

The details of how scale degrees turn into frequencies is handled [at the
derivation level](derivation.md.html#scales).

## Instruments

Instruments have some presence at the [derivation
level](derivation.md.html#instruments), and most of their complication is at
the [performance level](instruments.md.html).  But they can also bring Cmds
into scope, via 'Cmd.Cmd.inst_cmds'.  These cmds are only in scope on note
tracks, and there are utilities for their definition in
'Cmd.Instrument.CUtil'.  Generally they do things like special input keys for
drum maps.

Unfortunately, cmds are not yet introspectable for documentation like deriver
calls are, so while the 'Instrument.Browser' can reveal their presence,
you'll have to look at the instrument's source to know what they are.
