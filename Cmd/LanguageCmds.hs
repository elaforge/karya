{- | Helper functions to be imported into LanguageEnviron.  LanguageEnviron
must be interpreted since it's the "top level" module, so I put the library
of commands in here.

    --- old notes ---
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


    For saved cmds to work, all cmds need to run in a namespace that includes
    them.  The UI keeps a list of directories, and when a one shot command is
    compiled, it imports all modules from those directories.  It can also take
    a signal to recompile and rebind the keymaps.


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
module Cmd.LanguageCmds where
import qualified Control.Monad.Identity as Identity

import Ui.Types

import qualified Util.Log as Log

import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Ui.TestSetup as TestSetup

import qualified Cmd.Cmd as Cmd

{- commands:
    list views, blocks, tracks, rulers
    create ""
    add / remove tracks to a block

    load / save state

    set editing state (kbd entry octave, ...)
    set midi_instrument_config
-}

-- * show status

_cmd_state :: (Cmd.State -> a) -> Cmd.CmdT Identity.Identity a
_cmd_state = flip fmap Cmd.get_state

show_save_file = _cmd_state (show . Cmd.state_default_save_file)
set_save_file s =
    Cmd.modify_state $ \st -> st { Cmd.state_default_save_file = s }

show_midi_config = _cmd_state (show . Cmd.state_midi_instrument_config)
