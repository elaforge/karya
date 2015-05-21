[back to overview](overview.md.html#karya)

## definition

Instruments are defined in 'Local.Instrument', which will collect other the
various instrument definitions together into an 'Instrument.Db.Db'.  This is
then used by the 'Instrument.Browser' to search and display instruments, or by
the sequencer to look up instruments by name.

There are utilities to assist creating instruments in
'Cmd.Instrument.MidiInst'.  Instrument definitions can be very involved
because instruments are complicated, but it's mostly optional.  The minimum you
can get away with is a synth name and a pitchbend range.  E.g. a minimal load
function is

```
    load _dir = return $ MidiInst.make $ MidiInst.softsynth "synth-name"
        "Synth description." (-12, 12) []
```

After this you can create an instrument `synth-name/any-patch-name` and it
will have a pitch-bend range of (-12, 12), but no other configuration.  The
rest of the configuration is opt-in, so the more things you set in there, the
more fancy things the instrument can do.

As documented in 'Local.Instrument', some instruments may require expensive
things like parsing sysex dumps.  'Instrumnet.MakeDb', which is built as
`build/opt/make_db`, will run them and write caches.  'Instrument.Sysex' has
support for parsing (and generating) sysexes.

Actual instrument data is divided into two parts, the
'Perform.Midi.Instrument.Synth' and the 'Perform.Midi.Instrument.Instrument'.
The name is written `synth/inst`, and using the `>` prefix for an instrument
literal in [tracklang](derive.md.html#tracklang-syntax), you wind up with
`>vl1/sax` or `>kontakt/mridangam-g`.  Both the synth and instrument names
should conform to 'Ui.Id.valid'.

Synth and instrument names should not be abbreviated.  Of course you will want
to use a short name in the score, but you're expected to define a score
specific alias when configuring via [LInst](#linst-config).

Instruments have parts that affect the Cmd, Derive, and Perform systems.

## Cmd

At the Cmd level, each instrument can bring Cmds into scope when the insert
selection is focused on a track.  Typically this is used for drums, which will
want to bind specific strokes to keys instead of using the default
pitch-oriented [kbd entry](cmd.md.html#note-entry).

Utilities for generating per-instrument cmds are in 'Cmd.Instrument.CUtil'.

### LInst config

The 'Cmd.Repl.LInst' module has utilities to configure per-score instrument
configuration, which lives in 'Ui.StateConfig.config_midi', but is ultimately
'Perform.Midi.Instrument.Config'.

The most useful functions are 'Cmd.Repl.LInst.list', to show an abbreviated
version of the current configuration, 'Cmd.Repl.LInst.add' to allocate a new
instrument, and 'Cmd.Repl.LInst.remove' to remove one.  Other functions will
set or clear various fields in the Config.

## Derive

At the derive level, an instrument is mostly just a string
'Derive.Instrument'.  However, a few other bits of instrument config apply, as
defined by 'Derive.Deriver.Monad.Instrument'.  Specifically, a note track with
a title `>xyz` will wind up calling 'Derive.Deriver.Lib.with_instrument' on
`>xyz`.  A note track with an empty instrument name, like `>`, marks the track
as a note track but won't alter anything.

Analogous to CUtil, 'Derive.Instrument.DUtil' has utilities for defining
instrument-specific calls.

## Perform

In theory, all the Perform level instrument configuration is specific to a
backend, and a different backend may expose an entirely different set of
fields.  However, currently the only backend with non-trivial instrument needs
is MIDI (lilypond config is in 'Ui.StateConfig.config_lilypond', but it just
cares about the name to put it on the staff).

Performance specific details are in [performance.md](performance.md.html).
