## How to get sound to come out

- Intall and compile, as documented in [INSTALL.md.html](INSTALL.md.html).

- You did run `tools/setup-empty`, right?

- If you will use MIDI: Set up some internal MIDI devices.  On OS X this means
going to "Audio MIDI Setup", open up "IAC Driver" and add IAC ports.  I add 4
and rename them to 1 2 3 4.  You can test MIDI connection with `test_midi`:
`bin/mk build/opt/test_midi`.  Just run it and it will monitor from the inputs,
so play your MIDI keyboard if you have one, and make sure events show up.  On
OS X, I recommend "Midi Monitor" for debugging MIDI connections.  On linux,
JACK provides the MIDI IPC, so you should be able to use something like
`qjackctl` to connect.

    If you named the IAC ports 1 2 3 4, they should show up as "IAC Driver 1"
etc. (`test_midi` will show those on stdout).  In that case, the default config
in `User/Empty/Config.hs` will map them to local names `loop1`, `loop2`, etc.

- If you won't use MIDI, the `im` backend may be usable.  You'll have to
install its dependencies from `INSTALL.md`.  You won't have to do any of the
MIDI configuration below, and there are details in the `im` section below.

- Run `bin/opt`.  This will recompile and start `build/opt/logview` and
`build/opt/seq`.  Open another terminal window and type `bin/repl` to compile
and run the REPL.

- You might need to do some local config, as described below.  If you do,
you'll have to quit and `bin/opt` to recompile.

- The default block is whatever is loaded from `save/default`, but it probably
has a tempo track (called `tempo`), note track (starting with a `>`), and pitch
track (starting with a `*`).  The pitch track might be collapsed, which means
it shows up as a blue line.  You can expand it by clicking the note track and
typing cmd-m, and then expanding the window either manually by dragging or
whatever your window manager likes, or automatically with cmd-shift-r.

- The note track is named `>inst` and there should already be an allocation
for the instrument called "inst".  You can see it by typing `LInst.list` in the
REPL.  You can add another on MIDI channel 1 (or 2 if you count from 1) with
`LInst.add "inst2" "generic/" "loop1" [1]`.

    `loop1` should be the name of one of the internal MIDI devices you created.
`generic` is provided by 'User.Empty.Instrument'.  `inst` is the name of the
new instrument, which is why the note track title is `>inst`.  It will be
allocated MIDI channel 0 on loop1, so your DAW or MIDI device should be set up
to receive it.  Your DAW likely starts counting from 1, so add 1 to the
channels.  You can see an abbreviated instrument config with `LInst.list`, or
the complete version with `pp LInst.allocations`.  You can remove an instrument
allocation with `LInst.remove "inst"`.  Look at the source in 'Cmd.Repl.LInst'
to see all the things you can do.  All of the modules in `Cmd/Repl/L*.hs` are
intended to be used from the REPL.

- Click in the note track to set the selection, and turn on edit mode with
`cmd-[` or escape, and kbd entry with `cmd-]`.  You can do both at once with
`shift-cmd-[`.  Note that `cmd` is the command key on OS X, but is control on
Linux.  The edit box should turn red and get a `K`.  If you have a MIDI
keyboard you don't have to turn on kbd entry.  Play a few notes, either on the
MIDI keyboard or the computer keyboard.  If things are configured correctly,
you should get MIDI thru on loop1, channel 0, and notes should appear in the
score.

    If the MIDI keyboard doesn't make notes show up, make sure it shows up in
the startup stdout under "read devs" with a `*` next to it.  If it shows up but
without a `*`, it was detected but not opened for reading, because it's not in
`get_midi_config`.  See local config below.

- Press enter to play, or cmd-enter to play from the start of the block, and
space to stop.

- Read [keymap.html](keymap.html) to see the keybindings, and
[calls.html](calls.html) to see what built-in notation is available.  Cmds
don't have docs for now, so look at 'Cmd.GlobalKeymap' to find the cmds and
look at the haddock or source for whatever function.

### local config

- Hopefully local configuration is self-explanatory if you know Haskell.  If
you ran `tools/setup-empty`, then `Local.Config` will be a re-export of
'User.Empty.Config'.  So at first you'll probably be editing things in
`User/Empty`, but of course then git will want you to check that stuff in.  If
you modify them directly in `Local/`, git won't track that.  Of course, if you
get complicated configuration you'll want to track that stuff, so you can make
your own repo, or put it in the main repo in `User/` as I have done.  I'm
not really sure how per-user configuration should be handled.

- For local MIDI setup, you should modify `Config.get_midi_config`.  This
has aliases for MIDI device names, and a list of MIDI inputs to open for
reading.  I use short aliases like `loop1` to `loopn` to avoid depending
directly on the name given by OS.  Otherwise, the OS will decide to rename it
for fun and all your music will break.  See 'User.Elaforge.Config' for an
example.

- Configure a synthesizer.  'User.Empty.Instrument' has a generic MIDI synth
you can address as `generic/` as in the `LInst.add` above, you can start with
that.

    Later on, you should configure your specific synthesizers.  There are lots
of fancy features, but the most basic configuration is synth name and pitch
bend range.  Copy an existing soft synth, e.g.  'User.Elaforge.Instrument.Fm8',
and add the new module's `synth` value to 'Local.Instrument.midi_synths'.  The
simplest configuration is a single patch named
'Cmd.Instrument.MidiInst.default_patch' which is just "", so the qualified name
(used below when you allocate an instrument) will be `synth-name/`.

- If you don't use qwerty, modify 'Local.KeyLayout'.  This is relevant because
some keys are bound by their logical letter, while others are bound by their
physical position on the keyboard (e.g. kbd entry, where you can play the
keyboard like an organ).

### if you don't have MIDI setup already

One option is to use im, below.  Another is to try some free ones.  I don't
know much about the free synth world, but `fluidsynth` is small, simple,
and can play `.sf2` files which seem to be easily available.  It also makes a
good way to test if you even have working MIDI.

- Install `fluidsynth` and a General MIDI soundfont from
https://github.com/FluidSynth/fluidsynth/wiki/SoundFont

- Run it like
`fluidsynth -a coreaudio -m coremidi -o midi.portname=fluid something.sf2`
or on linux: `-a jack -m jack`.

- Now you can run `build/opt/browser`, search for `synth=gm`, and if
you double click on an instrument, it should switch the instrument on
the currently selected track to that one.  Or
`LInst.add "drum" "gm/percussion" "fluid" [10]` (it must be channel 10)
and some keys should be bound to drums.

### im

If you are using `im` together with MIDI, or if you have a DAW and want to
use it, then you'll have to install the `play_cache` VST so they DAW can
stream `im` rendered audio.  `tools/install-vst` will build and install it
on OS X.  Technically it builds on linux, but I've never used it there and
don't know how to install VSTs on linux.  Help me out?

You'll then have to use `LInst.add_play_cache` to configure the MIDI channel
to talk to the `play_cache` VST on your DAW.

If you want to avoid the MIDI headache entirely, you can set
`StaticConfig.im_play_direct` via `Local.Config`, and it'll attempt to play
audio itself (via sox at the moment, so have that installed), no `play_cache`
or DAW needed.

The problem with `im` is many fewer instruments than the MIDI world,
specifically there is `sampler-im` and `faust-im`.  The upside is that
`im` instruments don't need any configuration, simply
`LInst.add_im "somename" "faust/some-patch"`.  See `build/opt/browser` to
see the options.  However, samples are really big, so they're not included,
and the faust selection is very sparse.  Write your own!  Contribute them!
