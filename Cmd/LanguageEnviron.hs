{- | Top-level module for the interpreter in Language.  Everything in this
module is visible to the REPL, so it imports a lot of potentially useful
modules.

It has to be interpreted, so it should just put useful things into scope but
not actually define anything itself.  Those definitions go in LanguageCmds.
-}
module Cmd.LanguageEnviron where
import qualified Control.Monad.Identity as Identity

import Ui.Types

import qualified Util.Log as Log
import qualified Util.PPrint as PPrint

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.MakeRuler as MakeRuler

import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.InstrumentDb as InstrumentDb
import Cmd.LanguageCmds


-- | Like 'Cmd.run', but pretty-print the return value.  If the value is
-- already a string, just return it unchanged.
--
-- This is automatically added to language text by Language.mangle_text so it
-- can pretend to be running in the "real" CmdT.
run :: Show a => Cmd.CmdT Identity.Identity a -> State.State -> Cmd.State
    -> Cmd.CmdVal String
run cmd ui_state cmd_state = Identity.runIdentity $
    Cmd.run "" ui_state cmd_state (fmap PPrint.str_pshow cmd)
