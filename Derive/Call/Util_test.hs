-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Call.Util_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Environ as Environ


test_random = do
    let f seed = DeriveTest.extract extract $ DeriveTest.derive_blocks
            [ ("top", [(">", [(0, 1, seed ++ "b"), (1, 1, seed ++ "b")])])
            , ("b", [(">", [(0, 1, "")]), ("c", [(0, 0, "set (range)")])])
            ]
        extract = DeriveTest.e_control "c"

    -- Different calls to the same block are differently random.
    let ([[(_, v1)], [(_, v2)]], logs) = f ""
    equal logs []
    check $ v1 /= v2

    -- Unless overridden.  Note that the seed is set after the difference in
    -- position, so these calls should be the same.
    let ([[(_, v1)], [(_, v2)]], logs) = f "seed = 1 | "
    equal logs []
    equal v1 v2

test_randoms_in = do
    let run seed = expect_right "run" . DeriveTest.eval State.empty
            . Derive.with_val Environ.seed (seed :: Int)
        randoms seed low high = take 4 $
            run seed (Util.randoms_in low high)
    let double :: Int -> Double -> Double -> [Double]
        double = randoms
        int :: Int -> Int -> Int -> [Int]
        int = randoms
    -- Just make sure I get numbers that look like they're in the right range.
    equal (map round $ double 0 0 100) [16, 99, 4, 60]
    equal (map round $ double 0 (-100) 100) [-68, 98, -92, 19]
    equal (int 0 0 100) [94, 51, 33, 62]
    equal (int 1 (-100) 100) [-72, 62, -70, 46]
