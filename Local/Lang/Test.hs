module Local.Lang.Test where
import Ui.Types

import qualified Util.Log as Log

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

import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.InstrumentDb as InstrumentDb
import Cmd.LanguageCmds


test_cmd :: Cmd.CmdL ()
test_cmd = Log.notice "test command"

midi_msgs block_id = do
    block <- State.get_block block_id
    (err_events, _) <- Play.derive block
    events <- case err_events of
        Left err -> State.throw $ "derive error: " ++ show err
        Right events -> return events

    let (midi_events, _) = Convert.convert events
    return midi_events
