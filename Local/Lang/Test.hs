-- | Put your own local commands here.
module Local.Lang.Test where
import qualified Util.Log as Log

import Ui
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

import qualified Derive.Score as Score
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import Cmd.Lang.Global


test_cmd :: Cmd.CmdL ()
test_cmd = Log.notice "test command"
