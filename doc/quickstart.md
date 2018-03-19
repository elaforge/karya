## How to get sound to come out

- Intall and compile, as documented in [INSTALL.md.html](INSTALL.md.html).

- You did run `tools/setup-empty`, right?

- Set up some internal MIDI devices.  On OS X this means going to "Audio MIDI
Setup" and adding some IAC ports.  If you name them `loop1` to `loop4` then the
default config should work for you.  You can test MIDI connection with
test_midi: `bin/mk build/opt/test_midi`.  Just run it and it will monitor from
the inputs, so play your MIDI keyboard if you have one, and make sure events
show up.

- Run `bin/opt`.  This will recompile and start `build/opt/logview` and
`build/opt/seq`.  Open another terminal window and type `bin/repl` to compile
and run the REPL.

- You might need to do some local config, as described below.  If you do,
you'll have to quit and `bin/opt` to recompile.

- The default block is whatever is loaded from `save/default` (unless you
changed that in 'Local.Config'), but it probably has a tempo track (called
`tempo`), note track (starting with a `>`), and pitch track (starting with a
`*`).  The pitch track might be collapsed, which means it shows up as a blue
line.  You can expand it by clicking the note track and typing cmd-m, and then
expanding the window either manually by dragging or whatever your window
manager likes, or automatically with cmd-shift-r.

- The note track is named `>inst` and there should already be an allocation
for the instrument called "inst".  You can see it by typing `LInst.list` in the
REPL.  You can add another on MIDI channel 1 (or 2 if you count from 1) with
`LInst.add "inst2" "generic/" "loop1" [1]`.

    `loop1` should be the name of one of the internal MIDI devices you created,
or its alias if you configured one in 'Local.Config'.  `generic` is provided by
'User.Empty.Instrument'.  `inst` is the name of the new instrument, which is
why the note track title is `>inst`.  It will be allocated MIDI channel 0 on
loop1, so your DAW or MIDI device should be set up to receive it.  Your DAW
likely starts counting from 1, so add 1 to the channels.  You can see an
abbreviated instrument config with `LInst.list`, or the complete version with
`pp LInst.allocations`.  You can remove an instrument allocation with
`LInst.remove "inst"`.  Look at the source in 'Cmd.Repl.LInst' to see all the
things you can do.  All of the modules in `Cmd/Repl/L*.hs` are intended to be
used from the REPL.

- Click in the note track to set the selection, and turn on edit mode with `^[`
or escape, and kbd entry with `^]`.  You can do both at once with `shift^[`.
Note that in this context, `^` means control on Linux, and command on OS X.
The edit box should turn red and get a `K`.  If you have a MIDI keyboard you
don't have to turn on kbd entry.  Play a few notes, either on the MIDI keyboard
or the computer keyboard.  If things are configured correctly, you should get
MIDI thru on loop1, channel 0, and notes should appear in the score.

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

- Hopefully local configuration is self-explanatory if you know Haskell.
If you ran `tools/setup-empty`, then `Local.Config` will be a re-export of
'User.Empty.Config'.  So at first you'll probably be editing things in
User/Empty.

- If you want MIDI input to work, you should modify `Config.get_midi_config` to
be non-empty.  This has optional aliases for MIDI device names, and a list of
MIDI inputs to open for reading.  I use short aliases like `loop1` to `loopn`
to avoid depending directly on the name given by OS.  Otherwise, the OS will
decide to rename it for fun and all your music will break.  See
'User.Elaforge.Config' for an example.

- Configure a synthesizer.  There are lots of fancy features, but if you ignore
them all the configuration is basically just synth name and pitch bend range.
Copy an existing soft synth, e.g. 'User.Elaforge.Instrument.Fm8', and add the
new module's `synth` value to 'Local.Instrument.midi_synths'.  The simplest
configuration is a single patch named 'Cmd.Instrument.MidiInst.default_patch'
which is just "", so the qualified name (used below when you allocate an
instrument) will be `synth-name/`.

- If you don't use qwerty, modify 'Local.KeyLayout'.  This is relevant because
some keys are bound by their logical letter, while others are bound by their
physical position on the keyboard (e.g. kbd entry, where you can play the
keyboard like an organ).
