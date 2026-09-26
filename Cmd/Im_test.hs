-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Im_test where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Cmd.ResponderTest as ResponderTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Instrument.Inst as Inst
import qualified Perform.NN as NN
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_respond :: Test
test_respond = do
    let states = (bimap addAllocation setDb) $ ResponderTest.mkstates $
            UiTest.noteSpec
                ( "im"
                , [(0, 1, "4c"), (1, 1, "4d")]
                , [("dyn", [(0, ".5")]), ("unsupported", [(0, ".25")])]
                )
        addAllocation = UiTest.setInstruments ">im im-synth/ [ms] im"
        setDb state = state
            { Cmd.stateConfig = (Cmd.stateConfig state)
                { Cmd.configInstrumentDb = db }
            }
        (db, warns) = Inst.db [DeriveTest.defaultImSynth]
    equal warns []
    results <- respond states (pure ())
    ResponderTest.printResults results
    let config = Cmd.configIm $ Cmd.stateConfig DeriveTest.defaultCmdState
        Just synthConfig = Map.lookup "im-synth" (Config.synths config)
    notes <- Note.unserialize $ Config.notesFilename (Config.imDir config)
        "#untitled" UiTest.defaultBlockId synthConfig
    rightEqual (map Note.start <$> notes) [0, 1]
    rightEqual (map (Map.toAscList . Note.controls) <$> notes)
        [ [ (Control.dynamic, Signal.constant  0.5)
          , (Control.pitch, Signal.constant (realToFrac NN.c4))
          ]
        , [ (Control.dynamic, Signal.constant 0.5)
          , (Control.pitch, Signal.constant (realToFrac NN.d4))
          ]
        ]

respond :: ResponderTest.States -> Cmd.CmdT IO a -> IO [ResponderTest.Result]
respond = ResponderTest.respondAll [] 2
