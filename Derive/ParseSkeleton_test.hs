-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseSkeleton_test where
import qualified Util.Graph_test as Graph_test
import qualified Util.Seq as Seq
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import qualified Derive.ParseSkeleton as ParseSkeleton
import Global
import Types


-- * parse

test_parse = do
    let mktracks titles =
            [mk_track_info name n | (n, name) <- Seq.enumerate titles]
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
        Graph_test.graph_equal g1 g2

mk_track_info :: Text -> TrackNum -> Ui.TrackInfo
mk_track_info title tracknum = Ui.TrackInfo
    { track_title = title
    , track_id = tid
    , track_tracknum = tracknum
    , track_block = UiTest.btrack tid
    }
    where tid = UiTest.tid "fake"
