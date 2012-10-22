module Derive.Scale.BaliScales_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_ombak = do
    let run trans pitch = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks
                [("> " ++ trans, [(0, 1, "")]), ("*wayang", [(0, 0, pitch)])]
    equal (run "" "1") ([Just 67.57], [])
    equal (run "" "tuning = 'umbang' | 1") ([Just 67.57], [])
    equal (run "" "tuning = 'isep' | 1") ([Just 67.26], [])

    equalf 0.01 (run "| %ombak = 5" "1") ([Just 67.306], [])
    equalf 0.01 (run "| %ombak = 10" "1") ([Just 67.198], [])
