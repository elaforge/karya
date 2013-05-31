Cmds turn user input into actions.  This is mostly modifying internal
application state, but may also have IO effects, e.g. reading and writing
save files, or writing MIDI.

The basic unit of user input is a 'Cmd.Msg.Msg'.  The toplevel cmd loop is in
'Cmd.Responder'.  The top level set of Cmds is hardcoded in Cmd.Responder, and
consists of these groups:

- Internal housekeeping such as keeping track of which keys are held down,
mostly implemented in 'Cmd.Internal'.

- Global keymap, implemented in 'Cmd.GlobalKeymap'.

- Track-specific Cmds, which vary based on the track type, implemented in
'Cmd.Track'.

- REPL cmds, directly invoked by the user via the REPL.  This could be any cmd
anywhere, but there is also a library of functions meant to be invoked from
the REPL in the `Cmd/Repl` directory.  The REPL is evaluated in the context of
'Cmd.Repl.Environ', which basically imports everything qualified, and
'Cmd.Repl.Global' unqualified.
