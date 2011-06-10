module Derive.Scale.Ratio_test where
import Util.Test

import qualified Derive.Score as Score
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.PitchSignal as PitchSignal


test_ratio = do
    let extract = DeriveTest.extract
            (PitchSignal.unsignal_degree . Score.event_pitch)
    let run ratio base = extract $ DeriveTest.derive_tracks
            [ (">i1", [(0, 1, "")])
            , ("*twelve #ratio-source", [(0, 0, base)])
            , ("*ratio", [(0, 0, ratio)])
            ]
    equal (run "1/1" "4c") ([[(0, 60)]], [])
    equal (run "2/1" "4c") ([[(0, 72)]], [])
    equal (run "-2/1" "4c") ([[(0, 48)]], [])
    equal (extract $ DeriveTest.derive_tracks
            [ (">i1", [(0, 1, "")])
            , ("*ratio", [(0, 0, "1/1")])
            , ("*twelve #ratio-source", [(0, 0, "4c")])
            ])
        ([[]], ["Error: ratio scale requires a Control \"ratio-source\" "
            ++ "pitch signal"])
