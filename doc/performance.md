## Performance

Performance is the stage after derivation, that turns 'Derive.Score.Event's
into the final output format.  There are two major backends so far, MIDI and
lilypond.

General instrument details are in [instrument.md](instrument.md.html).

## MIDI

At the derive level, a 'Derive.BaseTypes.Instrument' is simply a text string,
but the first step in performance is to look up all those instruments in the
[instrument db](#instrument-db), and in the per-score configuration, which is
'Ui.UiConfig.config_allocations', which maps instruments to
'Perform.Midi.Patch.Config's.

The main feature of the Config is the allocation, which says which Addrs, (MIDI
device, channel) pairs, the instrument is allocated to.  If there are multiple
Addrs, the instrument will be [multiplexed](#multiplexing) across them.

The backend-indepndent per-allocation configuration is in
'Instrument.Common.Config'.

### controls

As an example, one of the basic things that most all instruments set is
'Perform.Midi.Patch.patch_control_map'.  This is just a mapping from
symbolic control names to MIDI CC numbers.  As a special rule, you can always
name controls like `cc15` or `cc23` and they will be mapped directly to MIDI CC
numbers, but if the instrument has a control map, you can write names like
`filter` or `bow-speed`.

The `dyn` control ('Derive.Controls.dynamic') has special support.  If the
instrument has the `Pressure` 'Perform.Midi.Patch.Flag' set, it is mapped to
the `breath` control, otherwise it's mapped to `vel`.

### multiplexing

If an instrument is allocated to more than one channel, it will be multiplexed
across them to try to keep pitch bend and controls from interfering with each
other.  Any overlapping notes that are compatible will be merged into the same
channel.  Compatible means they have the same control signals, compatible pitch
signals (in parallel 12TET intervals), the same instrument, and the same
keyswitch, and is implemented by 'Perform.Midi.Perform.can_share_chan'.

If a note can't be shared, it will get its own channel.  If all channels are in
use, it will simply reuse the one that was used longest ago.

The result is that if you allocate multiple channels, you should be able to
give notes independent tunings, controls, or keyswitches.  This is especially
important for scales which are not 12TET, since every note will be retuned
(unless 'Perform.Midi.Patch.config_scale' is set to the same scale).  But
if you have more overlapping notes than channels, they will start to share and
you'll hear artifacts.  This can happen easily if you have an instrument with a
long decay.

### attributes

Another basic configuration is 'Perform.Midi.Patch.AttributeMap'.  The
deriver keeps an attribute set in 'Derive.EnvKey.attributes'.  These are used
to select a particular articulation or playing style for the instrument, which
will then map to a keyswitch or a key range, or what have you.  Attributes are
additive, so while you may add `+pont` to a whole section to switch all
instruments that understand to sul pont, a specific note may add `+cresc` to
swell.  It will wind up with `+pont+cresc`, which, if you have a sufficiently
expensive sample library, will select a specific sample.

There is a standard vocabulary of attributes in 'Derive.Attrs', but generally
they're instrument-specific adjectives.

There are lots of other kinds of configuration, see the haddock for details.

## lilypond

Per-score lilypond configuration is in 'Ui.UiConfig.config_lilypond'.
Unlike MIDI perforance, lilypond "performance" is always initiated manually,
via the functions in 'Cmd.Repl.LLily'.

Lilypond output is significantly different from MIDI output, and needs a fair
amount of tweaking to look good.  This is because Karya's score notation is
more flexible and powerful than staff notation, in addition to being lower
level (and is also much simpler in terms of layout).  Ultimately this is why I
designed my own sequencer in the first place, instead of just using a staff
notation program, so it's hardly a surprise if a perfect translation is not
possible.

However, being more general and lower level, Karya's notation doesn't represent
aspects of tonal European music as well as staff notation does, especially
vertical harmony and binary rhythms.  Also, musicians generally read staff
notation.

You probably won't be able to get very far without understanding lilypond's own
score format.

Notes are written as non-overlapping events on tracks at the Karya level, but
they are grouped by instrument (and then hand and voice, as documented below)
at the lilypond level.  So the track structure is not reflected in the staff
structure, though of course you may want to lay them out similarly.

### calls

One  problem is that the events emitted by derive are sometimes too low level.
For example, the `+staccato` attribute may shorten a note's duration, or apply
a keyswitch, depending on the instrument.  But `+staccato` in staff notation
does neither, and applies a staccato marking to the notehead.  A tuplet
[note parent](derivation.md.html#note-parent-tracks) might normally emit a
triplet by setting start times and durations, but in staff notation emits a
larger duration and puts tuplet brackets around them.  To deal with this,
lilypond derivation replaces a number of standard calls with ones that instead
either attach an attribute that the lilypond performer understands (e.g.
`+staccato`), or attaches a bit of lilypond code to insert into the lilypond
score (e.g. tuplets).

In addition, some concepts exist only in staff notation, such as "mf" or
hairpin dynamics, or special markings like cross-staff notes or cautionary
accidentals.

While some calls are replaced outright with lilypond-emitting versions, others
change their behaviour when they notice they are being derived in lilypond
mode.  Which is which is just a question of code modularity.  Other calls exist
only to emit lilypond.  The library of lilypond-specific calls is in
'Derive.Call.Lily', and are in the 'Derive.Call.Module.ly' module.  You can
search for them [call doc](calls.html) with `m:ly`.

<img align=right width=180 src="img/example-tracks.png">

Sometimes the differences between lilypond output and MIDI output are too great
to be reconciled by the calls.  For instance, you may want to write notes as
simply being slurred together, but on playback you actually want to emit a
combination of keyswitches to get the details of the phrasing right.  Or a
four-note violin chord should be written as one chord, but played broken into
two dyads.  In this case, you can substitute an expression with the `if-ly`
call, or substitute an entire track with the `ly-track` and `not-ly-track`
calls.  In the example to the right, there are a total of four note parent
tracks applying to two note tracks.  Two of the tracks apply to both lilypond
and normal derivation, one applies to only when in lilypond mode, and the other
applies only in normal mode.  (TODO: screenshot should use up to date names,
a dyn track, and be shorter)

In addition, there are calls whose sole purpose is to emit staff-level lilypond
commands, rather than being attached to a specific note.  For example, clef
changes, dynamic changes, instructions to performers, or in fact any arbitrary
bit of lilypond code you might want to insert.  To avoid cluttering other
tracks, you can put them on their own track as zero-duration events.  Normally
you'd then put `ly-track` on it so normal derivation can skip it entirely.

### voice, staff

One of the concepts that exists in staff notation but not Karya notation is
voice and staff.  These are handled with environ values.  If there are notes
with the 'Derive.EnvKey.hand' value set, they will be split into the upper and
lower staves of a grand staff.  And if there are notes with
'Derive.EnvKey.voice' set, the notes on a single staff will be split into
multiple voices.  However, the voices are only split for the time where the
voice number is actually set.  And if you set voice for some notes, you have to
set it for all of them, and for the same duration.  Bad things will happen if
you have voiced notes overlapping with non-voiced notes on the same staff.
Hopefully there will be warnings in the log, and you should notice messed up
notation.

### pitch

Staff notation only supports the `*twelve` scale.  If you want it to support
some other scale you'll have to create an adapter.  This is just a mapping
from scale degree names to pitch names that lilypond understands, so if you
can get lilypond to understand it then you can get Karya to emit it.

### time signature

Time signature is another concept that only exists in staff notation.  The
closest analog is the ruler, but the ruler is more general and lower level,
so I don't try to infer time signature from it.  Instead, you set meter
explicitly with the `meter` call.  Still, for your sanity, you should make sure
the ruler lines up to the time signature in use.

The `meter` call is special in that it's interpreted as a global instruction,
no matter which instrument (if any instrument) it happens to fall under.
While technically different simultaneous meters are possible, they're tricky to
implement.

The more important part of meter is that it affects rhythm spelling, which
another concept that doesn't exist in Karya.  Therefore, it's the
responsibility of the lilypond performer to decide on how to spell rhythms.
This also means that irregular meters must be explicit, e.g. you have to write
3+2/4 or 3+3/8 instead of 5/4 or 6/8.  Rhythm spelling is complicated and
partially a matter of aesthetics, so it may spell a rhythm differently than you
want.  E.g.  if the meter is 2+2+2/8, it will spell a duple pattern with ties
instead of dotted notes.  Currently, there's no way to override the rhythm
spelling, so you'd have to either change the meter to 3+3/8 for that measure,
edit the lilypond code by hand, or just live with it.  Or make like Stravinsky
and write it as a duplet.

Currently, meter support is hardcoded, so 'Perform.Lilypond.Meter' only
supports a limited set of them.  However, it's easy to add new ones, and I
could probably guess one automatically from the meter, so this limitation could
be removed someday.

The same issue, though simpler, arises with beaming.  In this case, lilypond
itself will guess a good beaming for you, which you can override by directly
emitting the relevant bits of lilypond code.  Interestingly, lilypond also
handles beaming with a hardcoded list of specially tweaked time signatures.
