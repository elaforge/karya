module Cmd.Serialize_test where
import qualified Data.Binary as Binary
import qualified Data.Map as Map

import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Serialize as Serialize


test_serialize = do
    let (_, ustate) =
            UiTest.run_mkview [("track", [(0, 1, "e0"), (1, 1, "e1")])]
    save_state <- Serialize.save_state ustate
    let run f = (f save_state, recode (f save_state))

    uncurry equal $ run (State.state_project . Serialize.save_ui_state)
    uncurry equal $ run (State.state_views . Serialize.save_ui_state)
    uncurry equal $ run (State.state_blocks . Serialize.save_ui_state)
    uncurry equal $ run (State.state_tracks . Serialize.save_ui_state)
    uncurry equal $ run (State.state_rulers . Serialize.save_ui_state)
    uncurry equal $ run (State.state_midi_config . Serialize.save_ui_state)


recode :: Binary.Binary a => a -> a
recode = Binary.decode . Binary.encode
