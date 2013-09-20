[back to overview](overview.md.html#karya)

## Local Configuration

Local configuration is what would be a config file in other sequencers.  But
since Karya's configuration is also in haskell, and the configuration is
much more extensive than other sequencers, it's not that different from editing
the code for the application itself.

The code is all in the Local directory.  Technically it should probably not be
in the source repo, or at least not the same source repo, since it's specific
to one person's config.  But at the moment I'm the only user, and it's good to
have an example around.

There are three entry points.

## Local.Config

This exports `load_static_config`, which gets a
'App.StaticConfig.StaticConfig'.  This is meant to be local global
configuration, and you can set up the MIDI ReadDevices and WriteDevices,
load the instrument db, add local keybindings, and the like.

'App.StaticConfig.Midi' is especially important, because it configures names
for MIDI devices, and especially which devices should be opened for read on
startup.

## Local.Repl

The modules in here are put into scope for the [REPL](repl.md.html), so you can
add local REPL utilities.  That said, I just edit Cmd.Repl.LWhatever.

## instrument db

Local.Instrument exports the load functions for the instrument db.  The `load`
function is called from `load_static_config` when the app starts up, and
`make_dbs` is used by 'Instrument.MakeDb' to build caches for the more
expensive instruments.  What this means is part of the local instrument
configuration, and therefore entirely dependent on the instrument definitions
themselves.

Each synth is defined in its own module in `Local/Instrument/`, and exports a
`load` function that returns 'App.MidiInst.SynthDesc's.  Some of them may do
fancy things like parsing directories full of sysex dumps, which is what is
meant by an "expensive" instrument.  The `make_db` program just calls the
`make_db` function of each synth, which is free to do whatever parsing it
needs, and then serialize and dump the result, presumably in `inst_db/db/`.  At
that point, their `load` function just reads from the cache.  Otherwise, `load`
just returns a data structure defined directly in the module.

Instruments can be very complicated, because not only do they have a whole pile
of fields as documented in 'Perform.Midi.Instrument.Patch', but they can also
define Cmds that are in scope when the cursor is on a track with that
instrument, and define [NoteCalls](derivation.md.html#calls) which are brought
into scope along with the instrument.  These often work together, for example,
a drum instrument might bind a bunch of keystrokes to emit various calls
representing different strokes, and also bring those calls into scope.

So there are various libraries to assist defining instruments, the main one
being 'App.MidiInst'.  Utilities for defining instrument-specific calls are in
`Derive/Instrument/`, e.g. 'Derive.Instrument.DUtil', while utilities for
defining instrument-specific cmds are in `Cmd/Instrument/`, e.g.
'Cmd.Instrument.CUtil'.

'Instrument.Browser', compiled to `browser`, is used to search the instrument
db.

## theory

Ideally I'd like to make it as easy as possible to insert new code into a
score.  Historically I meant for the score language to be very simple and
limited, and entirely rely on writing haskell functions.  However, it gradually
got more complicated and more powerful, and I still don't have an easy way to
insert new calls at runtime.  So far, it hasn't been a problem, because all the
calls I've been adding have been general purpose enough to warrant going into
the default "prelude" library.  At some point, though, I might want to
implement a dynamic loading scheme where per-score config is loaded from
Local.Score.ScoreName.
