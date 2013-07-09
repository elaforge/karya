## GUI

<img align=right width=180 src="../../doc/img/ss-vla.png">

The GUI only has one kind of window, and that is a view on a block, which is
the top level of score organization.  All blocks have the same physical
structure, which boils down to text in various places.  How that is interpreted
and becomes music is the business of the deriver.  So the UI elements described
here have links to the relevant bits of the
[derivation doc.](derivation.md.html)

One important thing to know about the UI is that it is, like the rest of the
program, organized in many layers.  The lowest layer is the C++ API, which
just opens windows, changes colors, etc. but has no idea what any of that
means.  This is represented mostly by the API exposed in 'Ui.Block.BlockC',
which is a direct wrapper around the C++ API.  The medium level is mostly
dumb, in that you alter haskell data structures and the UI then changes to
reflect the changes, but you are still free to alter them in just about any
way you see fit.  This level is what you see if you directly manipulate
'Ui.State.State', which you could do if you wanted, or slightly higher level
if you use the functions in 'Ui.State', which is what you should probably do.
The high level is defined by what Cmds are available, and it is at this level
that UI changes have "meaning", for example where a "K" in the edit box means
that kbd entry is enabled.  This is all to say that most of what is described
here are simply the conventions established by the default set of Cmds, and if
you write your own Cmds or override the defaults you can change things
arbitrarily.  You could put other letters in the edit mode box if you wrote a
Cmd to do that.

Each block is named by a [BlockId](#ids), and the name part of
the ID is in the window title, followed by the [ViewId](#ids).

Below that, there is a text input, which is the block title.  This actually
hides itself when it's empty, and you can get it to come back by double
clicking on the skeleton display.

The [skeleton display](derivation.md.html#derivation) has arrows from track to
track.

To the left of a the skeleton is a blank gray box that displays the [edit
mode](#editmode), and below that is the [ruler](#ruler).

To the right of the ruler are vertical [tracks](#tracks), which is the bulk of
the window.  The tracks have [events](#events), each of which has a red line at
its trigger point, a duration, and a bit of text.  Some events (for instance,
most events on control or pitch tracks) have a 0 duration, so they're just a
red trigger line and a bit of text.  Tracks and events are color coded based on
their type.  This doesn't have any semantic meaning, and is analogous to syntax
highlighting.  The tracks may also have any number of
[selections](#selections).

The bottom of the window has a status display, which displays some highly
abbreviated bits of app state.  The various status fields are from the "status
view" section of 'App.Config'.

There's another tiny little gray box in the lower left corner.  This shows play
status: gray when ready to play, light blue when the score needs to be
rederived, dark blue when derivation is in progress, and green when playing.

That's all!

### text input

Text input boxes expand temporarily if their text is too large to fit.  It's
not very satisfactory, but I couldn't come up with a better way to cram
text into small spaces.  Track titles still usually wind up being too short to
display their contents.

Control-h and control-l skip backward and forward by a token, where a token is
a space separated word, a `symbol`, or a parenthesized expression.  TODO maybe
it should just be a word.  Holding shift extends the selection as usual.
Control-backspace deletes a token.  Otherwise, they use the shortcuts
documented on fltk's Fl_Input.

## IDs

IDs are names for the major score elements.  ViewIds name 'Ui.Block.View's,
BlockIds name 'Ui.Block.Block's, RulerIds name 'Ui.Ruler.Ruler's, and TrackIds
name 'Ui.Track.Track's.  When you want to do something with one of those, say
create a view for a block, modify a ruler, or whatever, you wind up using the
ID to name it.  Ultimately IDs all turn into lookups on the maps in
'Ui.State.State'.

Each ID has a namespace and a name component, e.g.  `(bid "namespace/name")` is
a BlockId.  Each score has a default namespace
'Ui.StateConfig.config_namespace'.  New IDs get the default namespace, and when
writing a block call you normally just write the name, leaving the namespace
implicit.  The result is that you should be able to merge two scores together
and avoid ID clashes.

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

### EditMode

The edit modes are somewhat analogous to vi's modes.  Basically, they enable
certain editing oriented cmds.

The current edit mode is displayed in the edit box, above the ruler.  The
mapping is in 'Cmd.Internal.sync_edit_box', but basically red means you're in
edit mode, and gray means you're in command mode.

The effect of each edit mode depends on the track, so its documented below with
each track type.

## Tracks

Tracks are indexed by 'Ui.Types.TrackNum', starting at 0 on the far left.  So a
track can be addressed either by a (BlockId, TrackNum) pair, or directly by a
TrackId or RulerId.

The actual meaning of the tracks is defined by
[derivation](derivation.md.html#derivation), the documentation
here is just the low level, without reference to what it means.  This is
analogous to syntax in a normal language.

The lowest level is the score format, which is analogous to syntax in a normal
language.

There are three kinds of tracks: divider, ruler, and event.

### divider track

'Ui.Block.Divider's are just visual spacers.  All they have is a color and a
width.  You can place them manually to provide visual separation between
tracks, and they are used automatically to represent collapsed tracks.

### ruler track

The ruler by convention is in TrackNum 0, which on the far left, and is special
in that it stays put and doesn't scroll right to left like the other tracks.
It provides a visual reference for the rhythmic structure and is analogous to
the meter.

A 'Ui.Ruler.Ruler' has 'Ui.Ruler.Marklist's, which is just a list of
'Ui.Ruler.Mark's.

Technically a ruler can have marks in any kind of arbitrary pattern, but
typically you'd want them dividing up time according to a meter or tala.
'Cmd.Meter' has utilities to describe Western-style meters, while 'Cmd.Tala'
has Carnatic talas, normally you'd use the functions in 'Cmd.Repl.LRuler' to
modify the ruler or create new rulers.

While ruler tracks have only a rule, event tracks have rulers too, which show
up as transparent lines.  Normally all tracks in a block have the same ruler,
but if you want you can set up different meters for different tracks.  You can
also add multiple rulers, but most of the utilities assume all tracks have the
same ruler, and there's one ruler track at tracknum 0.

### event track

Event tracks have 'Ui.Event.Event's.  They are divided into several types,
differentiated by their titles.  Details in the [derivation
doc](derivation.md.html#track-evaluation).

Of course, to be visible, a track has to live in a block.  The same track,
identified by its TrackId, can be in multiple blocks at once, but it can only
appear once in each block.  This restriction is for convenience, since
otherwise just a (BlockId, TrackId) pair wouldn't be enough to identify a
track.

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
(either from the MIDI keyboard or kbd entry) and enter pitches on a
neighboring pitch track, or create one if it doesn't exist.  So they mostly
act like pitch tracks.  But ValEdit on note tracks also supports
'Cmd.Cmd.state_chord' to enter chords easily, and
'Cmd.Cmd.state_record_velocity'.

#### pitch track

[Pitch tracks](derivation.md.html#pitch-track) produce a pitch signal, and the
cmds are in 'Cmd.PitchTrack'.  They're mostly concerned with turning pitch
input into the named pitch for the scale in scope.

#### control track

[Control tracks](derivation.md.html#control-track) generate controls, which
are numbers that vary in time.  The cmds are in 'Cmd.ControlTrack'.  By
convention they are normalized between 0 and 1, which makes them multiply
together nicely, which is convenient, because that in fact is what they do by
default.  But they are not restricted, and for example a control indicating
the amount of delay, or the depth of a trill would have no reason to stay
within that range.

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

Note entry mode ('Cmd.NoteEntry') turns the computer keyboard into a music
keyboard by remapping the keys to emit pitched notes.  It's often more
convenient to use a MIDI keyboard, though, because then you get to keep the
computer keyboard for cmds.

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
