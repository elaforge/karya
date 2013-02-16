module Derive.Scale.Ratio_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_ratio = do
    let extract = DeriveTest.extract DeriveTest.e_nns
    let run ratio base = extract $ DeriveTest.derive_tracks
            [ (">i1", [(0, 1, "")])
            , ("*twelve #ratio-source", [(0, 0, base)])
            , ("*ratio", [(0, 0, ratio)])
            ]
    -- Bah, 'hz_to_nn . nn_to_hz' introduces imprecision.
    equal (run "1/1" "4c") ([[(0, 60.00000000000001)]], [])

    equal (run "2/1" "4c") ([[(0, 72.00000000000001)]], [])
    equal (run "-2/1" "4c") ([[(0, 48)]], [])
    equal (extract $ DeriveTest.derive_tracks
            [ (">i1", [(0, 1, "")])
            , ("*ratio", [(0, 0, "1/1")])
            , ("*twelve #ratio-source", [(0, 0, "4c")])
            ])
        ([[]], ["Error: required: ratio scale requires #ratio-source"])
