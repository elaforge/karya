## HOW TO GET SOUND TO COME OUT

- Intall and compile, as documented in [INSTALL.md.html](INSTALL.md.html).

- Set up some internal MIDI devices.  On OS X this means going to "Audio MIDI
Setup" and adding some IAC ports.

- Add your machine's name to 'Local.Config.get_midi_config', and add make a
config for it.  This is just optional aliases for MIDI device names, and a list
of MIDI inputs to pay attention to.

- Configure a synthesizer.  There are lots of fancy features, but if you ignore
them all the configuration is basically just synth name and pitch bend range.
Copy an existing soft synth, e.g. 'Local.Instrument.Fm8', and add the new
module's `load` function to 'Local.Instrument.synths'.  If you use
'App.MidiInst.softsynth' then you don't have to have any explicit patch names.

- Recompile because you edited stuff: `bin/mk binaries`

- Recompile and start `build/opt/logview` and `build/opt/seq` with the
`bin/opt` shell script.  `bin/run build/opt/repl` to compile and run the REPL
in another terminal.

- The default block has a tempo track, note track, and pitch track.

- Type in the REPL: `LInst.add "inst" "synth-name/patch-name" "loop1" [0]`

    `loop1` should be the name of one of the internal MIDI devices you created,
or its alias if you configured one in 'Local.Config'.  `synth-name` is whatever
you named the new synth in its config, and `patch-name` is whatever you want.
`inst` is the name of the new instrument, which is why the note track
title is `>inst`.  It will be allocated MIDI channel 0 on loop1, so your DAW or
MIDI device should be set up to receive it.  You can see the instrument config
with `LInst.configs`.

- Rename the note track from `>` to `>inst`.

- Click in the note track to set the selection, and turn on edit mode with `^[`
and kbd entry with `^]`.  The edit box should turn red and get a `K`.  If you
have a MIDI keyboard you don't have to turn on kbd entry.  Play a few notes,
either on the MIDI keyboard or the computer keyboard.  If things are configured
correctly, you should get MIDI thru on loop1, channel 0, and notes should
appear in the score.

- Press enter to play, or cmd-enter to play from the start of the block, and
space to stop.

- Read [keymap.html](keymap.html) to see the keybindings, and
[calls.html](calls.html) to see what built-in notation is available.
