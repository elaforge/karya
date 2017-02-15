## How to get sound to come out

- Intall and compile, as documented in [INSTALL.md.html](INSTALL.md.html).

- Set up some internal MIDI devices.  On OS X this means going to "Audio MIDI
Setup" and adding some IAC ports.  You can test this with test_midi:
`bin/mk build/opt/test_midi`.  Just run it and it will monitor

- Add your machine's name to 'Local.Config.get_midi_config', and add make a
config for it.  This is just optional aliases for MIDI device names, and a list
of MIDI inputs to open for reading.  If you don't care about any of that,
you can just return `StaticConfig.empty_midi`.  I use short aliases like
`loop1` to `loopn` to avoid depending directly on the OS level name.
Otherwise, the OS will decide to rename it for fun and all your music will
break.

- If you don't use my somewhat customized dvorak layout, modify
'Local.KeyLayout'.  For instance, change it to `layout = KeyLayouts.qwerty`.
This is relevant because some keys are bound by their logical letter, while
others are bound by their physical position on the keyboard (e.g. kbd entry,
where you can play the keyboard like an organ).

- Configure a synthesizer.  There are lots of fancy features, but if you ignore
them all the configuration is basically just synth name and pitch bend range.
Copy an existing soft synth, e.g. 'Local.Instrument.Fm8', and add the new
module's `synth` value to 'Local.Instrument.midi_synths'.  The simplest
configuration is a single patch named 'Cmd.Instrument.MidiInst.default_patch'
which is just "", so the qualified name (used below when you allocate an
instrument) will be `synth-name/`.

- `mkdir save; cp data/default.save save/default`.  `save` is where saved
scores go, and `default` is an example mostly-empty score to start with.

- Run `bin/opt`.  This will recompile and start `build/opt/logview` and
`build/opt/seq`.  Open another terminal window and type `bin/repl` to compile
and run the REPL.

- The default block is whatever is loaded from `save/default` (unless you
changed that in 'Local.Config'), but it probably has a tempo track (called
`tempo`), note track (starting with a `>`), and pitch track (starting with a
`*`).  The pitch track might be collapsed, which means it shows up as a blue
line.  You can expand it by clicking the note track and typing cmd-m, and then
expanding the window either manually or automatically with cmd-shift-r.

- Type in the REPL: `LInst.add "inst" "synth-name/" "loop1" [0]`

    `loop1` should be the name of one of the internal MIDI devices you created,
or its alias if you configured one in 'Local.Config'.  `synth-name` is whatever
you named the new synth in its config.  `inst` is the name of the new
instrument, which is why we're going to set the note track title to `>inst`.
It will be allocated MIDI channel 0 on loop1, so your DAW or MIDI device should
be set up to receive it.  If your DAW starts MIDI channels at 1 instead of 0
then of course it will consider that channel 1.  You can see an abbreviated
instrument config with `LInst.list`, or the complete version with `pp
LInst.allocations`.  `LInst.remove "inst"` to remove an instrument allocation.
Look at the source to 'Cmd.Repl.LInst' to see all the things you can do.  All
of the modules in `Cmd/Repl/L*.hs` are intended to be used from the REPL.

- Rename the `>` note track to `>inst`.

- Click in the note track to set the selection, and turn on edit mode with `^[`
or escape, and kbd entry with `^]`.  The edit box should turn red and get a
`K`.  If you have a MIDI keyboard you don't have to turn on kbd entry.  Play a
few notes, either on the MIDI keyboard or the computer keyboard.  If things are
configured correctly, you should get MIDI thru on loop1, channel 0, and notes
should appear in the score.

If the MIDI keyboard doesn't make notes show up, make sure it shows up in the
startup stdout under "read devs" with a `*` next to it.  If it shows up but
without a `*`, it was detected but not opened for reading, because it's not in
`get_midi_config`.

- Press enter to play, or cmd-enter to play from the start of the block, and
space to stop.

- Read [keymap.html](keymap.html) to see the keybindings, and
[calls.html](calls.html) to see what built-in notation is available.  Cmds
don't have docs for now, so look at 'Cmd.GlobalKeymap' to find the cmds and
look at the haddock or source for whatever function.
