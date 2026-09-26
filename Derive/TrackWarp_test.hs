-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TrackWarp_test where
import qualified Data.Foldable as Foldable
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Warp as Warp

import Global
import Types


test_collect_track_warps :: Test
test_collect_track_warps = do
    let f tracks = extract . TrackWarp.collectTrackWarps tracks
        extract = sort . map extract1
            where
            extract1 (TrackWarp.TrackWarp s e block tracks warp) =
                ( (s, e)
                , block
                , Foldable.toList tracks
                , Warp._stretch <$> Warp.isLinear warp
                )
            sort = Lists.sortOn (\(range, block, _, _) -> (block, range))
    let bid = UiTest.bid
        tids name = map (UiTest.mkTidName name)
        tempo n = ("tempo", [(0, 0, showt n)])

    let (tracks, wmap, logs) =
            runWarps [("b1", UiTest.noteTrack [(0, 1, "4c")])]
    equal logs []
    equal (f tracks wmap) [((0, 1), bid "b1", tids "b1" [1, 2], Just 1)]

    let (tracks, wmap, logs) = runWarps
            [("b1", tempo 2 : UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])]
    equal logs []
    equal (f tracks wmap) [((0, 1), bid "b1", tids "b1" [1, 2, 3], Just 0.5)]

    -- Sub-block.
    let (tracks, wmap, logs) = runWarps
            [ ("top", [(">", [(0, 2, "sub"), (2, 2, "sub")])])
            , ("sub=ruler", UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
            ]
    equal logs []
    equal (f tracks wmap)
        [ ((0, 2), bid "sub", tids "sub" [1, 2], Just 1)
        , ((2, 4), bid "sub", tids "sub" [1, 2], Just 1)
        , ((0, 4), bid "top", tids "top" [1], Just 1)
        ]

    -- Sub-block with different tempos.
    let (tracks, wmap, logs) = runWarps
            [ ("top", [tempo 2, (">", [(0, 2, "sub"), (2, 2, "sub")])])
            , ("sub=ruler",
                tempo 0.5 : UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
            ]
    equal logs []
    equal (f tracks wmap)
        [ ((0, 1), bid "sub", tids "sub" [1, 2, 3], Just 0.5)
        , ((1, 2), bid "sub", tids "sub" [1, 2, 3], Just 0.5)
        , ((0, 2), bid "top", tids "top" [1, 2], Just 0.5)
        ]

    -- Two toplevel tempos.
    let (tracks, wmap, logs) = runWarps $ (:[]) $ (,) "b1" $
            tempo 1 : UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")]
            ++ tempo 2 : UiTest.noteTrack [(0, 1, "4e"), (1, 1, "4f")]
    equal logs []
    equal (f tracks wmap)
        [ ((0, 1), bid "b1", tids "b1" [4, 5, 6], Just 0.5)
        , ((0, 2), bid "b1", tids "b1" [1, 2, 3], Just 1)
        ]


runWarps :: [UiTest.BlockSpec]
    -> ([(BlockId, [Tree.Tree TrackId])], TrackWarp.WarpMap, [Text])
runWarps = extract . DeriveTest.deriveBlocks
    where
    extract r = (tracksOf r, wmap, logsOf r)
        where
        logsOf = snd . DeriveTest.extract id
        wmap = Derive.collectWarpMap $ Derive.stateCollect $ Derive.rState r
    tracksOf r = UiTest.eval state TrackWarp.getTrackTrees
        where state = Derive.stateUi $ Derive.stateConstant $ Derive.rState r
