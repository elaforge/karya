{- | Process a textual language to perform UI state changes.

It would be nice to get line editing.  I can do this with either readline,
using a shell directly, or using vi directly.

readline: a custom readline using app that talks on a socket.

shell: a "send" command and then a set of shell aliases / small scripts to make
it easier.  This means I can also script things with shells or other languages
with a little more work.  Shell is still a sucky language.

vi / acme: If the language supports more than just command lists, I'll want
multi-line editing.

What I really want to do, though, is have the language be haskell.  For that
I need to get hs-plugins working.


Use cases:

    send a simple cmd, "create block"
I should be able to just type the command + args with minimum fuss.

    create a one-off compound command, "create 3 copies of this block"
Same as above, but it runs in the Cmd monad, and saves as single undo.

    create a compound command and save it, doesn't get run
I should just edit a plain module, and tell the UI to include it in its
language namespace.  This way I use any editor, and commands use the module
namespace, and are easily sharable and distributable by normal means.

    add a custom keybinding
Modify the keymap, globally, per block, or per track.

    show blocks without views

    attach a derivation to a track, or set of tracks, or block

    reorder tracks, without messing up the derivation rules


For saved cmds to work, all cmds need to run in a namespace that includes them.
The UI keeps a list of directories, and when a one shot command is compiled, it
imports all modules from those directories.  It can also take a signal to
recompile and rebind the keymaps.


    Block / derivation management

Because each step in derivation may be exposed as a Block, there needs to be
some kind of framework for organizing them.  Also, the order in which they
happen is relevant.

tempo track -> multiply times of everything in the block
note track -> substitute sub-blocks, realize note structures (tuplets), 
    realize scale

contorller1
controller2
    -> combine with controller2, apply static transformation

----
derived notes
controller1 + controller2
    -> NoteList that gets rendered to Midi / score




    Ways to display a derivation

A neighboring track in the same block.

Another block view.

"Hidden" behind the current block / track, press a key to "descend" one level.

Not displayed at all, but played of course.


-}
module Cmd.Language where

import qualified Language.Haskell.Interpreter.GHC as GHC

import qualified Util.Log as Log

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg


cmd_language :: GHC.InterpreterSession -> Cmd.CmdIO
cmd_language session msg = do
    text <- case msg of
        Msg.Socket s -> return s
        _ -> Cmd.abort
    Log.notice $ "got lang: " ++ show text
    return Cmd.Done

{- commands:
list views, blocks, tracks, rulers
create ""
add / remove tracks to a block

load / save state

set editing state (kbd entry octave, ...)
set midi_instrument_config
-}



