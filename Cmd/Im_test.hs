-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Im_test where
import qualified Data.Map as Map

import Util.Test
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ResponderTest as ResponderTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.NN as NN
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


test_respond = do
    let states = (bimap add_allocation set_db) $ ResponderTest.mkstates $
            UiTest.note_spec
                ( "im"
                , [(0, 1, "4c"), (1, 1, "4d")]
                , [("dyn", [(0, ".5")]), ("unsupported", [(0, ".25")])]
                )
        add_allocation = Ui.config#Ui.allocations #= allocs
        allocs = UiConfig.Allocations $ Map.fromList
            [ ("im", UiConfig.allocation
                (InstTypes.Qualified "im-synth" "") UiConfig.Im)
            ]
        set_db state = state
            { Cmd.state_config = (Cmd.state_config state)
                { Cmd.config_instrument_db = db }
            }
        (db, warns) = Inst.db [DeriveTest.default_im_synth]
    equal warns []
    results <- respond states (return ())
    ResponderTest.print_results results
    let config = Cmd.config_im $ Cmd.state_config DeriveTest.default_cmd_state
        Just synth_config = Map.lookup "im-synth" (Config.synths config)
    notes <- Note.unserialize $ Config.notesFilename (Config.imDir config)
        "#untitled" UiTest.default_block_id synth_config
    right_equal (map Note.start <$> notes) [0, 1]
    right_equal (map (Map.toAscList . Note.controls) <$> notes)
        [ [ (Control.dynamic, Signal.constant  0.5)
          , (Control.pitch, Signal.constant (realToFrac NN.c4))
          ]
        , [ (Control.dynamic, Signal.constant 0.5)
          , (Control.pitch, Signal.constant (realToFrac NN.d4))
          ]
        ]

respond :: ResponderTest.States -> Cmd.CmdT IO a -> IO [ResponderTest.Result]
respond = ResponderTest.respond_all [] 2
