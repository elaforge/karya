[back to overview](overview.md.html#karya)

## definition

Usually "patch" will mean a backend-specific (likely MIDI) definition, while
"instrument" means a specific allocation of a patch in a particular score.
I'm not totally consistent with that terminology, though I try.

Patches are defined in 'Local.Instrument', which will collect other the
various instrument definitions together into an 'Instrument.Inst.Db'.  This is
then used by the 'Instrument.Browser' to search and display patches, or by the
sequencer to look them up by name.

There are utilities to assist creating patches in 'Cmd.Instrument.MidiInst'.
Patch definitions can be very involved, but it's mostly optional.  The minimum
you can get away with is a synth name and a pitchbend range.  E.g. a minimal
synth definition is

```
    synth :: MidiInst.Synth
    synth =
        MidiInst.synth "synth-name" "Synth description." $
            MidiInst.synth_controls controls patches
        where
        controls = []
        patches = [MidiInst.default_patch (-12, 12) []]
```

After this you can create an instrument bound to the
'Instrument.InstTypes.Qualified' name `synth-name/` and it will have a
pitch-bend range of (-12, 12), but no other configuration.  The rest of the
configuration is opt-in, so the more things you set in there, the more fancy
things the instrument can do.  Both the synth and patch names should conform to
'Ui.Id.valid'.  That's not a technical requirement, but the restriction is good
for consistency and keeps my options open.

As documented in 'Local.Instrument', some patches may require expensive things
like parsing sysex dumps.  'Instrument.MakeDb', which is built as
`build/opt/make_db`, will run them and write caches.  'Instrument.Sysex' has
support for parsing (and generating) sysexes.

Instruments have parts that affect the Cmd, Derive, and Perform systems.

## Cmd

Each instrument can bring Cmds into scope when the insert selection is focused
on a track.  Typically this is used for drums, which will want to bind specific
strokes to keys instead of using the default pitch-oriented [note
entry](cmd.md.html#note-entry).

Utilities for generating per-instrument cmds are in 'Cmd.Instrument.CUtil'.

### LInst config

The 'Cmd.Repl.LInst' module has utilities to configure per-score instrument
configuration, which lives in 'Ui.StateConfig.config_allocations'.

The most useful functions are 'Cmd.Repl.LInst.list', to show an abbreviated
version of the current configuration, 'Cmd.Repl.LInst.add' to allocate a new
instrument, and 'Cmd.Repl.LInst.remove' to remove one.  Other functions will
set or clear various configuration fields.

## Derive

At the derive level, an instrument is mostly just a string
'Derive.ScoreTypes.Instrument'.  However, a few other bits of instrument config
apply, as defined by 'Derive.Deriver.Monad.Instrument'.  Specifically, a note
track with a title `>xyz` will wind up calling
'Derive.Deriver.Lib.with_instrument' on `>xyz`.  A note track with an empty
instrument name, like `>`, marks the track as a note track but won't alter
anything.

Analogous to CUtil, 'Derive.Instrument.DUtil' has utilities for defining
instrument-specific calls.

## Perform

In theory, all the Perform level instrument configuration is specific to a
backend, and a different backend may expose an entirely different set of
fields.  However, currently the only backend with non-trivial instrument needs
is MIDI (lilypond config is in 'Ui.StateConfig.config_lilypond', but it just
cares about the name to put it on the staff).

Performance specific details are in [performance.md](performance.md.html).
