-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Im_test where
import qualified Data.Map as Map

import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ResponderTest as ResponderTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.NN as NN
import qualified Instrument.Inst as Inst
import qualified Local.Instrument
import qualified Synth.Sampler.Control as Control
import qualified Synth.Sampler.Note as Note
import qualified Synth.Sampler.Signal as Signal

import Global


test_respond = do
    let states = (add_alias *** set_db) $ ResponderTest.mkstates $
            UiTest.note_spec
                ("im", [(0, 1, "4c"), (1, 1, "4d")], [("dyn", [(0, ".5")])])
        add_alias = State.config#State.aliases
            #= UiTest.make_aliases [("im", "sampler/inst")]
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
