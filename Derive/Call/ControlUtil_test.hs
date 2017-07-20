-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.ControlUtil_test where
import qualified Data.Map as Map

import Util.Test
import qualified Derive.Args as Args
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.ControlUtil as ControlUtil
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Signal as Signal


test_breakpoints = do
    let make start end = ControlUtil.breakpoints 1 id
            . ControlUtil.distribute start end
    let f start end = Signal.unsignal . make start end
    equal (f 4 8  []) []
    equal (f 4 8  [1]) [(4, 1)]
    equal (f 4 8  [0, 1, 0]) [(4, 0), (5, 0.5), (6, 1), (7, 0.5), (8, 0)]

test_modify = do
    let run merge = DeriveTest.extract DeriveTest.e_dyn_rounded
            . DeriveTest.derive_tracks_setup (with_call merge) ""
        with_call merge = CallTest.with_control_generator "g" (c_gen merge)
    let tracks dyn =
            [ (">", [(0, 8, "")])
            , ("dyn", [(0, 0, dyn)])
            , ("c", [(4, 2, "g")])
            ]
    equal (run Derive.DefaultMerge (tracks ".5"))
        ([[(0, 0.5), (4, 0.25), (6, 0.5)]], [])
    let Just merge_max = Map.lookup "max" Derive.mergers
    equal (run (Derive.Merge merge_max) (tracks ".25"))
        ([[(0, 0.25), (4, 0.5), (6, 0.25)]], [])
    where
    c_gen :: Derive.Merge Signal.Control -> Derive.Generator Derive.Control
    c_gen merge = CallTest.generator1 $ \args -> do
        (start, end) <- Args.real_range args
        let signal = Signal.signal [(start, 0.5)]
        ControlUtil.modify_with merge Controls.dynamic end signal
        return mempty

test_limited_slope = do
    let f low high from slope = Signal.unsignal $
            ControlUtil.limited_slope 1 low high from slope 4 8
    equal (f Nothing Nothing 0 1) [(4, 0), (5, 1), (6, 2), (7, 3), (8, 4)]
    equal (f (Just 0) (Just 2) 0 1) [(4, 0), (5, 1), (6, 2)]
    equal (f (Just 0) (Just 8) 0 1) [(4, 0), (5, 1), (6, 2), (7, 3), (8, 4)]
    equal (f (Just 0) (Just 2) 2 1) [(4, 2)]
    equal (f (Just 0) (Just 2) 4 1) [(4, 2)]

    equal (f Nothing Nothing 2 (-1)) [(4, 2), (5, 1), (6, 0), (7, -1), (8, -2)]
    equal (f (Just 0) (Just 2) 2 (-1)) [(4, 2), (5, 1), (6, 0)]
    equal (f (Just (-8)) (Just 2) 2 (-1))
        [(4, 2), (5, 1), (6, 0), (7, -1), (8, -2)]
    equal (f (Just 0) (Just 2) 0 (-1)) [(4, 0)]
    equal (f (Just 0) (Just 2) (-1) (-1)) [(4, 0)]
