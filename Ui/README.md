## OVERVIEW

This is the interface between the C++ implementation of the UI (which is in
`fltk/`) and the rest of the app.

### BlockC, UiMsg

The lowest level is in the various *C modules, which contain code to serialize
the haskell versions of the sequencer state to the c++ versions, and imported
functions that directly call the C++ code.  Since the haskell FFI can't talk
directly to C++, they actually call an API defined in `c_interface.h`, which
basically just re-exports the relevant C++ methods, and does things like
serialize C arrays to C++ vectors.

The actual UI event loop is hooked into the fltk event loop.  The haskell part
is in 'Ui.Ui'.  It runs on the app's main thread, receiving actions on a
channel.

If 'Ui.BlockC' is for haskell -> C++ communication, 'Ui.UiMsg' is for C++ ->
haskell communication.  The UI emits UiMsgs in response to mouse and keyboard
actions.  UiMsg deserializes those and puts them on a channel for the rest of
the app.

### Block, Ruler, Track, Event, Sync, Diff

Most of the UI data like widget sizes and tracks displayed is small and is just
copied to the C++ layer, which basically maintains a mirror image of the
haskell state, but note data is passed to C++ as a callback.

The UI level data structures are declared in 'Ui.Block', 'Ui.Ruler',
'Ui.Track' and 'Ui.Event'.  These are all tied together by 'Ui.State', which
represents the complete state of a score.

On every change to the State (which is immutable), the displayed portions have
to be synced with the C++ state.  Recreating all the windows from scratch on
every change would be slow and look ugly, so the 'Ui.Diff' module generates a
set of 'Ui.Update's needed to change one State to another, and the 'Ui.Sync'
module then applies those by calling BlockC functions.


## Code conventions

Haskell types should be converted to C types with 'Ui.Util'.  This can do
range checking if necessary.

Functions in the Fltk monad make calls to FLTK and must be called in the UI
thread so they are serialized in the main thread (this is a requirement for
some OSs).  Functions with IO types don't call FLTK and can be run from any
thread concurrently.
