-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.TrackWarp_test where
import qualified Data.Foldable as Foldable
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Derive.TrackWarp as TrackWarp

import Global
import Types


test_collect_track_warps = do
    let f tracks = extract . TrackWarp.collect_track_warps tracks
        extract = sort . map extract1
            where
            extract1 (TrackWarp.TrackWarp s e block tracks warp) =
                ((s, e), block, Foldable.toList tracks,
                    ScoreTypes.warp_stretch warp)
            sort = Seq.sort_on (\(range, block, _, _) -> (block, range))
    let bid = UiTest.bid
        tids name = map (UiTest.mk_tid_name name)
        tempo n = ("tempo", [(0, 0, showt n)])

    let (tracks, wmap, logs) =
            run_warps [("b1", UiTest.note_track [(0, 1, "4c")])]
    equal logs []
    equal (f tracks wmap) [((0, 1), bid "b1", tids "b1" [1, 2], 1)]

    let (tracks, wmap, logs) = run_warps
            [("b1", tempo 2 : UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])]
    equal logs []
    equal (f tracks wmap) [((0, 1), bid "b1", tids "b1" [1, 2, 3], 0.5)]

    -- Sub-block.
    let (tracks, wmap, logs) = run_warps
            [ ("top", [(">", [(0, 2, "sub"), (2, 2, "sub")])])
            , ("sub=ruler", UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
            ]
    equal logs []
    equal (f tracks wmap)
        [ ((0, 2), bid "sub", tids "sub" [1, 2], 1)
        , ((2, 4), bid "sub", tids "sub" [1, 2], 1)
        , ((0, 4), bid "top", tids "top" [1], 1)
        ]

    -- Sub-block with different tempos.
    let (tracks, wmap, logs) = run_warps
            [ ("top", [tempo 2, (">", [(0, 2, "sub"), (2, 2, "sub")])])
            , ("sub=ruler",
                tempo 0.5 : UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
            ]
    equal logs []
    equal (f tracks wmap)
        [ ((0, 1), bid "sub", tids "sub" [1, 2, 3], 0.5)
        , ((1, 2), bid "sub", tids "sub" [1, 2, 3], 0.5)
        , ((0, 2), bid "top", tids "top" [1, 2], 0.5)
        ]

    -- Two toplevel tempos.
    let (tracks, wmap, logs) = run_warps $ (:[]) $ (,) "b1" $
            tempo 1 : UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")]
            ++ tempo 2 : UiTest.note_track [(0, 1, "4e"), (1, 1, "4f")]
    equal logs []
    equal (f tracks wmap)
        [ ((0, 1), bid "b1", tids "b1" [4, 5, 6], 0.5)
        , ((0, 2), bid "b1", tids "b1" [1, 2, 3], 1)
        ]


run_warps :: [UiTest.BlockSpec]
    -> ([(BlockId, [Tree.Tree TrackId])], TrackWarp.WarpMap, [Text])
run_warps = extract . DeriveTest.derive_blocks
    where
    extract r = (tracks_of r, wmap, logs_of r)
        where
        logs_of = snd . DeriveTest.extract id
        wmap = Derive.collect_warp_map $ Derive.state_collect $ Derive.r_state r
    tracks_of r = UiTest.eval state TrackWarp.get_track_trees
        where state = Derive.state_ui $ Derive.state_constant $ Derive.r_state r
