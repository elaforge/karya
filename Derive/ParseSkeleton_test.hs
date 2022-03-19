-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseSkeleton_test where
import qualified Util.Graphs as Graphs
import qualified Derive.ParseSkeleton as ParseSkeleton
import qualified Ui.Skeleton as Skeleton

import           Global
import           Util.Test


test_parse :: Test
test_parse = do
    let f = ParseSkeleton.default_parser . mktracks
    equal_fmt sfmt (f ["", ""]) (mkskel [(0, 1)])
    -- They're both controls, with no instrument track.
    equal_fmt sfmt (f ["", ""]) (mkskel [(0, 1)])
    equal_fmt sfmt (f [">i1"]) (mkskel [])
    equal_fmt sfmt (f [">i1", "c1", "c2"]) (mkskel [(0, 1), (1, 2)])

    equal_fmt sfmt (f ["c1", ">i1", "c2"]) (mkskel [(0, 1), (1, 2)])
    equal_fmt sfmt (f ["c0", ">i1", "c1", ">i2", "tempo", ">i3"])
        (mkskel [(0, 1), (1, 2), (0, 3), (4, 5)])
    equal_fmt sfmt (f [">i1", "c1", ">i2", "c2"])
        (mkskel [(0, 1), (2, 3)])

    equal_fmt sfmt (f [">i1", "c1", ">i2", ">i3"])
        (mkskel [(0, 1)])
    equal_fmt sfmt (f [">i1", "c1 -->", ">i2", ">i3"])
        (mkskel [(1, 2), (1, 3)])

test_parse_note_bottom :: Test
test_parse_note_bottom = do
    let mktracks = map (uncurry ParseSkeleton.Track) . zip [0..]
    let f = ParseSkeleton.note_bottom_parser . mktracks
    -- note-bottom parser
    equal_fmt sfmt (f ["tempo", "*p", "c1", ">i1", "c2", ">i2"])
        (mkskel [(0, 1), (1, 2), (2, 3), (0, 4), (4, 5)])
    equal_fmt sfmt (f ["c1", ">i1", "c2"])
        (mkskel [(0, 1), (0, 2)])

mktracks :: [Text] -> [ParseSkeleton.Track]
mktracks = map (uncurry ParseSkeleton.Track) . zip [0..]

mkskel :: [(Int, Int)] -> Skeleton.Skeleton
mkskel = Skeleton.make

sfmt :: Skeleton.Skeleton -> Text
sfmt (Skeleton.Skeleton g) = txt $ Graphs.draw g
