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

## Local.Instrument

Documented in [instrument.md](instrument.md.html).

## theory

Ideally I'd like to make it as easy as possible to insert new code into a
score.  Historically I meant for the score language to be very simple and
limited, and entirely rely on writing haskell functions.  However, it gradually
got more complicated and more powerful, and I still don't have an easy way to
insert new calls at runtime.  So far, it hasn't been a problem, because all the
calls I've been adding have been general purpose enough to warrant going into
the default 'Derive.Call.Module.prelude' module, though, I might want to
implement a dynamic loading scheme where per-score config is loaded from
Local.Score.ScoreName.  In practice, [ky files](ui.md.html#ky-file) provide a
fairly powerful way to write per-score configuration.
