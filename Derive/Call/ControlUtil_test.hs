-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.ControlUtil_test where
import Util.Test
import qualified Derive.Args as Args
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Signal as Signal


test_breakpoints :: Test
test_breakpoints = do
    let make start end = ControlUtil.breakpoints 1 ControlUtil.Linear
            . ControlUtil.distribute start end
    let f start end = Signal.toPairs . make start end
    equal (f 4 8  []) []
    equal (f 4 8  [1]) [(4, 1)]
    equal (f 4 8  [0, 1, 0]) [(4, 0), (6, 1), (8, 0)]

test_modify :: Test
test_modify = do
    let run title = DeriveTest.extract DeriveTest.eDyn
            . DeriveTest.deriveTracksSetup withCall title
        withCall = CallTest.withControlGenerator "g" cGen
    let tracks dyn =
            [ (">", [(0, 8, "")])
            , ("dyn set", [(0, 0, dyn)])
            , ("c", [(4, 2, "g")])
            ]
    -- *0.5 for the range only.
    equal (run "" (tracks ".5"))
        ([[(0, 0.5), (4, 0.5), (4, 0.25), (6, 0.25), (6, 0.5)]], [])
    -- +0.5 for the range only.
    equal (run "default-merge add dyn" (tracks ".25"))
        ([[(0, 0.25), (4, 0.25), (4, 0.75), (6, 0.75), (6, 0.25)]], [])

    equal (run "default-merge set dyn" (tracks ".25"))
        ([[(0, 0.25), (4, 0.25), (4, 0.5), (6, 0.5), (6, 0.25)]], [])
    where
    cGen :: Derive.Generator Derive.Control
    cGen = CallTest.generator1 $ \args -> do
        (start, end) <- Args.realRange args
        let signal = Signal.fromSample start 0.5
        ControlUtil.modifyWith Derive.DefaultMerge Controls.dynamic end signal
        pure mempty

test_slope_to_limit :: Test
test_slope_to_limit = do
    let f low high from slope = Signal.toPairs $
            ControlUtil.slopeToLimit low high from slope 4 8
    equal (f Nothing Nothing 0 1) [(4, 0), (8, 4)]
    equal (f (Just 0) (Just 2) 0 1) [(4, 0), (6, 2)]
    equal (f (Just 0) (Just 8) 0 1) [(4, 0), (8, 4)]
    equal (f (Just 0) (Just 2) 2 1) []
    -- TODO not sure this makes sense
    equal (f (Just 0) (Just 2) 4 1) [(4, 2)]

    equal (f Nothing Nothing 2 (-1)) [(4, 2), (8, -2)]
    equal (f (Just 0) (Just 2) 2 (-1)) [(4, 2), (6, 0)]
    equal (f (Just (-8)) (Just 2) 2 (-1)) [(4, 2), (8, -2)]
    equal (f (Just 0) (Just 2) 0 (-1)) []
    -- TODO not sure this makes sense
    equal (f (Just 0) (Just 2) (-1) (-1)) [(4, 0)]

test_smooth_absolute :: Test
test_smooth_absolute = do
    let f time = Signal.toPairs
            . ControlUtil.smoothAbsolute ControlUtil.Linear 1 time
        s020 = [(0, 0), (2, 2), (4, 0)]
    equal (f 2 [(0, 0), (2, 2)]) [(0, 0), (2, 0), (4, 2)]
    equal (f 2 s020) [(0, 0), (2, 0), (4, 2), (6, 0)]
    equal (f (-1) s020) [(0, 0), (1, 0), (2, 2), (3, 2), (4, 0)]
    equal (f (-0.75) s020) [(0, 0), (1.25, 0), (2, 2), (3.25, 2), (4, 0)]
    equal (f 0.75 s020) [(0, 0), (2, 0), (2.75, 2), (4, 2), (4.75, 0)]
    equal (f (-2) s020) [(0, 0), (2, 2), (4, 0)]
    -- Not enough room.
    equal (f 4 s020) [(0, 0), (2, 0), (4, 2), (8, 0)]
    equal (f (-4) s020) [(0, 0), (2, 2), (4, 0)]
    -- Zero duration.
    equal (f 0 s020) [(0, 0), (2, 0), (2, 2), (4, 2), (4, 0)]

test_smooth_relative :: Test
test_smooth_relative = do
    let f time = Signal.toPairs
            . ControlUtil.smoothRelative ControlUtil.Linear 1 (const time)
    equal (f 0 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (4, 0), (4, 4), (8, 4), (8, 0)]
    equal (f 0.5 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (2, 0), (4, 4), (6, 4), (8, 0)]
    equal (f 1 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (4, 4), (8, 0)]

test_split_samples :: Test
test_split_samples = do
    let abs = ControlUtil.splitSamplesAbsolute
    equal (abs 1 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (4, 0), (5, 4), (8, 4), (9, 0)]
    equal (abs (-1) [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (3, 0), (4, 4), (7, 4), (8, 0)]

    let rel time = ControlUtil.splitSamplesRelative (const time)
    equal (rel 0.25 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (3, 0), (4, 4), (7, 4), (8, 0)]
    equal (rel 1 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (4, 4), (8, 0)]
    equal (rel 10 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (4, 4), (8, 0)]
    equal (rel 0 [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (4, 0), (4, 4), (8, 4), (8, 0)]
    equal (rel (-10) [(0, 0), (4, 4), (8, 0)])
        [(0, 0), (4, 0), (4, 4), (8, 4), (8, 0)]
