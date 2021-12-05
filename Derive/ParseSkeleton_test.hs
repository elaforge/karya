-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseSkeleton_test where
import qualified Util.Graphs_test as Graphs_test
import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Ui.Skeleton as Skeleton

import           Util.Test


test_parse :: Test
test_parse = do
    let mktracks = map (uncurry ParseSkeleton.Track) . zip [0..]
    let mkskel = Skeleton.make
    let f = ParseSkeleton.default_parser . mktracks
    let note_bottom = ParseSkeleton.note_bottom_parser . mktracks

    -- They're both controls, with no instrument track.
    skel_equal (f ["", ""]) (mkskel [(0, 1)])
    skel_equal (f [">i1"]) (mkskel [])
    skel_equal (f [">i1", "c1", "c2"]) (mkskel [(0, 1), (1, 2)])

    skel_equal (f ["c1", ">i1", "c2"]) (mkskel [(0, 1), (1, 2)])
    skel_equal (f ["c0", ">i1", "c1", ">i2", "tempo", ">i3"])
        (mkskel [(0, 1), (1, 2), (0, 3), (4, 5)])
    skel_equal (f [">i1", "c1", ">i2", "c2"])
        (mkskel [(0, 1), (2, 3)])

    -- note-bottom parser
    skel_equal (note_bottom ["tempo", "*p", "c1", ">i1", "c2", ">i2"])
        (mkskel [(0, 1), (1, 2), (2, 3), (0, 4), (4, 5)])
    skel_equal (note_bottom ["c1", ">i1", "c2"])
        (mkskel [(0, 1), (0, 2)])
    where
    skel_equal (Skeleton.Skeleton g1) (Skeleton.Skeleton g2) =
        Graphs_test.graph_equal g1 g2
