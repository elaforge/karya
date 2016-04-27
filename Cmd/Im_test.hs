-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Im_test where
import qualified Data.Map as Map

import Util.Test
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ResponderTest as ResponderTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Local.Instrument
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


test_respond = do
    let states = (add_allocation *** set_db) $ ResponderTest.mkstates $
            UiTest.note_spec
                ("im", [(0, 1, "4c"), (1, 1, "4d")], [("dyn", [(0, ".5")])])
        add_allocation = State.config#State.allocations #= allocs
        allocs = StateConfig.Allocations $ Map.fromList
            [ (Score.Instrument "im", StateConfig.allocation
                (InstTypes.Qualified "sampler" "inst") StateConfig.Im)
            ]
        set_db state = state
            { Cmd.state_config = (Cmd.state_config state)
                { Cmd.config_instrument_db = db }
            }
        (db, warns) = Inst.db Local.Instrument.im_synths
    equal warns []
    results <- until_complete states (return ())
    ResponderTest.print_results results
    notes <- Note.unserialize DeriveTest.default_im_notes
    right_equal (map Note.start <$> notes) [0, 1]
    right_equal (map (Map.toAscList . fmap Signal.toList . Note.controls)
            <$> notes)
        [ [ (Control.envelope, [(0, 0.5)])
          , (Control.pitch, [(0, realToFrac NN.c4)])
          ]
        , [ (Control.envelope, [(0, 0.5)])
          , (Control.pitch, [(1, realToFrac NN.d4)])
          ]
        ]

until_complete :: ResponderTest.States -> Cmd.CmdT IO a
    -> IO [ResponderTest.Result]
until_complete = ResponderTest.respond_until ResponderTest.is_derive_complete
