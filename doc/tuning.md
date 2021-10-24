## tuning

Tuning means any tuning which is not 12TET.  For everything other than MIDI,
there is no problem, just say what pitch you want.  So the easiest way to
do tuning is to use the OSC or `im` backend.

Unfortunately lots of stuff uses MIDI, so there are lots of hacks to try
to get around that.

NoteNumber is a `Pitch.NoteNumber`, which is in 12TET, but since it's a
real number, it can express any pitch.  Key is a `Midi.Key` which is
the integral MIDI note number.

## bake tuning into patch

This means the MIDI patch takes its integral Keys, but internally maps them to
your tuning, whatever that is.  The disadvantage is that you have to choose one
tuning, have to configure the patch to use it (where each synth has a different
ad-hoc way to do that), and karya has to "undo" the tuning when going
NoteNumber -> Key, so the patch will redo it.  If you mess up any of those
steps, you get wonky pitches.

Since Patch.Scale is flattened into an Key -> NoteNumber mapping, it can only
accomodate static scales, where the scale is a mapping from a small number of
symbols (127 maximum!) to exact pitch frequencies, which never change.  So
for scales that change tuning or whose pitches can't be enumerated into <128
discrete values, these hacks won't work, see "send pitch with note."

### Configure patch to use the tuning.

This is synth dependent, so there are various hacks:

- `LTuning.write_ksp` writes terrible KSP language for Kontakt.  Copy paste
into its "script" slot.  For this (and many others), you'll need a
`Patch.Scale`, which is a flattening of a Scale into a simple
`Key -> NoteNumber` vector.  There are tools in `LTuning` to get those, e.g.
`LTuning.selection`.

- `LTuning.nrpn`, this using NRPN magic to send the tuning table.  No one
understands it unless you can configure them to do so, and there's a KSP
script for Kontakt.

- `LTuning.realtime`, this uses the "midi tuning standard" (MTS) to send the
tuning table.  This is how it was "supposed" to be done, but very few synths
support it.  Pianoteq does, but pianoteq also supports MIDI channels, so it
can also use multiplexed MIDI channels, below.

The latter two can send the configuration over MIDI, so they are also stored in
the karya patch configuration.  `LInst.initialize_all` will send over all the
relevant patch configuration.  It could happen automatically when you load a
score, but currently it's manual.

### Configure karya to undo the tuning.

If you don't do this, karya will send the Midi.Key assuming 12TET, and send
PitchBend to tune each note.  You don't want the pitch bend because you've
already configured the synth to do that internally.

So, MIDI patches at the karya level have an optional associated `Patch.Scale`
to undo the tuning.  If it's the same tuning as the scale it's played under,
it will map the scale's pitches to Keys and no more pitch bend.  The functions
in `LTuning` will also set the patch scale along with the initialization, so as
long as you use those and keep your scale consistent, things should work out.

If pitches sound wonky, you can look for PitchBend in "MIDI Monitor", or
use `pp LPerf.sel_midi`, or `pp LPerf.sel_events` to see what the intended
pitch is.  `LInst.list` shows if instruments have a Patch.Scale, and
`LInst.get_scale` shows it in detail.

## send pitch with note

This is the proper way, but MIDI makes it awkward because it has to send as
two separate messages (NoteOn + PitchBend) and because the PitchBend is global
to the channel.

### use a monophonic instrument

The easiest way around this is to use a monophonic instrument.  If you don't
mind that note decays are also bent (which is appropriate for a single string,
say), then this should just work, provided you configured the pitch bend range
properly.  I usually set it to the maximum the synth will allow, and try to
make every patch in the synth use the same range.  That way I set the default
in the karya synth def.  However, each instrument allocation can also be
separately configured with `LInst.set_pb_range`.

### use multiplexed midi channels

The "traditional" way (this was the original plan and the first thing I
implemented) is to allocate multiple MIDI channels to the patch, and have karya
multiplex among them to try to avoid conflicting pitch bends.  At the time I
had used mostly hardware synths, so I was surprised to discover that software
synths mostly don't support multiple MIDI channels, in that they can listen to
all channels but they ignore the channel and pitch bend is global anyway.

For a hardware synth, it's probably sufficient to go into "multitimbral" mode
and allocate the same patch to as many channels as you need.  For a software
synth, some will support this (like pianoteq and kontakt), but many don't and
instead have to be allocated as a separate VST for each one.  This is very
inefficient because they don't share any resources.  Even the ones like kontakt
that can share resources but still make you allocate a separate patch per
channel can be quite awkward because now if you want to tweak a patch you
have to get all those independent patches to have the same tweak.  Best to
change one, delete the rest, and duplicate the changed one.

The MPE "MIDI polyphonic expression" standard is this channel multiplexing
approach, so synhs that claim to support MPE should "just work" like channels
were supposed to work.  MPE adds an some extra bits like configure channel
range with NRPN, but treating channels properly is the main thing.

MIDI 2, if anyone ever implements it, should also solve the problem, and
possibly bring MIDI somewhat up to date with '00s technology like OSC.  Why
not use OSC?  I don't know, but the MMA can't charge money for the OSC spec.
