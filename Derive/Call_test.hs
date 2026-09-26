-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call_test where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score

import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_random :: Test
test_random = do
    let f seed = DeriveTest.extract extract $ DeriveTest.deriveBlocks
            [ ("top", [(">", [(0, 1, seed <> "b"), (1, 1, seed <> "b")])])
            , ("b", [(">", [(0, 1, "")]), ("c", [(0, 0, "set (range)")])])
            ]
        extract = DeriveTest.eControl "c"

    -- Different calls to the same block are differently random.
    let ([[(_, v1)], [(_, v2)]], logs) = f ""
    equal logs []
    notEqual v1 v2

    -- Unless overridden.  Note that the seed is set after the difference in
    -- position, so these calls should be the same.
    let ([[(_, v1)], [(_, v2)]], logs) = f "seed = 1 | "
    equal logs []
    equal v1 v2

test_random_freeze_seed :: Test
test_random_freeze_seed = do
    -- Query the seed, then hardcode it, then the call is no longer random.
    let f call = extract $ DeriveTest.deriveBlocks
            [ ("top", [(">", [call])])
            , ("b", [(">", [(0, 1, "")]), ("c", [(0, 0, "set (range)")])])
            ]
        extract r =
            ( fromMaybe (error "no seed") $
                maybe (error "no trackdyn") envSeed $
                eEnv (UiTest.bid "b") 1 r
            , first head $ DeriveTest.extract
                (\e -> (DeriveTest.eControl "c" e, Score.eventStack e)) r
            )

    -- Moving the event changes the value.
    let (seed0, (([(0, val0)], _st0), [])) = f (0, 1, "b")
    let (seed1, (([(1, val1)], _st1), [])) = f (1, 1, "b")
    notEqual val0 val1
    notEqual seed0 seed1

    -- To freeze a random value, get the seed with log-seed, and hardcode it:
    -- pprint $ snd $ snd (f (0, 1, "log-seed | b"))
    -- This works because Call is not included in the seed, otherwise the
    -- very preserce of log-seed would change it.
    let (_, (([(1, val1')], _st1), [])) = f (1, 1, "seed=232 | b")
    equal val0 val1'

    -- If I lock the seed, it doesn't change when moved.
    -- This works because the Call "equal" comes after Range 0 1 or Range 1 2,
    -- so it overrides whatever unique hash was in there.
    let (_, (([(0, val0)], _st0), [])) = f (0, 1, "seed=42 | b")
    let (_, (([(1, val1)], _st1), [])) = f (1, 1, "seed=42 | b")
    equal val0 val1

envSeed :: DeriveT.Environ -> Maybe Int
envSeed = Env.maybeVal EnvKey.seed

eEnv :: BlockId -> TrackNum -> Derive.Result -> Maybe DeriveT.Environ
eEnv blockId tracknum =
    fmap Derive.stateEnviron
    . Map.lookup (blockId, UiTest.mkTidBlock blockId tracknum)
    . Derive.rTrackDynamic

test_randoms_in :: Test
test_randoms_in = do
    let run seed = expectRight . DeriveTest.eval Ui.empty
            . Derive.withVal EnvKey.seed (seed :: Int)
        randoms seed low high = take 4 $
            run seed (Call.randomsIn low high)
    let double :: Int -> Double -> Double -> [Double]
        double = randoms
        int :: Int -> Int -> Int -> [Int]
        int = randoms
    -- Just make sure I get numbers that look like they're in the right range.
    equal (map round $ double 0 0 100) [16, 99, 4, 60]
    equal (map round $ double 0 (-100) 100) [-68, 98, -92, 19]
    equal (int 0 0 100) [94, 51, 33, 62]
    equal (int 1 (-100) 100) [-72, 62, -70, 46]

test_pick_weighted :: Test
test_pick_weighted = do
    let f weights = Call.pickWeighted
            (NonEmpty.fromList (zip weights ("abcdef" :: [Char])))
    equal (map (f [1, 3]) [0, 0.25, 0.5, 0.75]) "abbb"
    equal (map (f [3, 1]) [0, 0.25, 0.5, 0.75]) "aaab"
