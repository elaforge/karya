## HOW TO GET SOUND TO COME OUT

- Intall and compile, as documented in INSTALL.

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

- Recompile because you edited stuff: `repl=t mk binaries`

- Start `build/opt/logview` in the background, and the app `build/opt/seq` from
one terminal, and the REPL `build/opt/repl` from another.

- The default block has a tempo track, note track, and pitch track.  Note tracks
start with `>`, rename it `>synth-name/patch-name`, where `synth-name` is
whatever you named the new synth, and `patch-name` is whatever you want.

- Type in the REPL: `LInst.alloc "synth-name/patch-name" "loop1" [0]`

    `loop1` should be the name of one of the internal MIDI devices you created,
or its alias if you configured one in 'Local.Config'.  This allocates MIDI
channel 0 on loop1 to that instrument.  You can see allocation with `pp
LInst.configs`.

- Click in the note track to set the selection, and turn on edit mode with `^[`
and kbd entry with `^]`.  The edit box should turn red and get a `K`.  If you
have a MIDI keyboard you don't have to turn on kbd entry.  Play a few notes,
either on the MIDI keyboard or the computer keyboard.  If things are configured
correctly, you should get MIDI thru on loop1, channel 0, and notes should
appear in the score.

- Press enter to play, or cmd-enter to play from the start of the block, and
space to stop.
