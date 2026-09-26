-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Create_test where
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Rect as Rect
import Util.Test
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create

import Global
import Types


test_track_ruler :: Test
test_track_ruler = do
    let f tracknum = Create.trackEvents UiTest.defaultBlockId
            Ui.noRuler tracknum 20 Track.empty
        run trackSpecs cmd = CmdTest.traceLogs $ CmdTest.eTracks $
            CmdTest.runTracks trackSpecs cmd
    equal (run [] (f 1)) $ Right [("", [])]
    equal (run [] (f 10)) $ Right [("", [])]
    equal (run [("1", [])] (f 10)) $ Right [("1", []), ("", [])]
    equal (run [("1", [])] (f 1)) $ Right [("", []), ("1", [])]

-- test_splice_above_all = do
--     let run = run_skel_point Create.splice_above_all
--     -- Splice the new track above 2.
--     equal (run 2 [(1, 2)] 2) (Right ([('1', 'x'), ('x', '2')], []))
--
--     -- No track above, becomes parent to all toplevel tracks.
--     equal (run 2 [] 2) (Right ([('x', '1'), ('x', '2')], []))
--     equal (run 3 [(1, 2)] 1)
--         (Right ([('x', '1'), ('x', '3'), ('1', '2')], []))
--     -- Track above, becomes parent to siblings.
--     equal (run 5 [(1, 2), (2, 3), (2, 4)] 3)
--         (Right ([('1', '2'), ('2', 'x'), ('x', '3'), ('x', '4')], []))
--     -- Make sure it goes to the right of the parent.
--     equal (run 5 [(1, 3), (3, 4)] 3)
--         (Right ([('1', 'x'), ('x', '3'), ('3', '4')], []))

test_splice_above_ancestors :: Test
test_splice_above_ancestors = do
    let run = runSkel Create.spliceAboveAncestors
    equal (run 2 [(1, 2)] (2, 2)) $ Right ([('x', '1'), ('1', '2')], [])
    equal (run 4 [(1, 2), (3, 4)] (4, 4)) $
        Right ([('1', '2'), ('x', '3'), ('3', '4')], [])
    equal (run 4 [(1, 2), (3, 4)] (1, 4)) $
        Right ([('x', '1'), ('x', '3'), ('1', '2'), ('3', '4')], [])

test_splice_below :: Test
test_splice_below = do
    let run = runSkelPoint Create.spliceBelow
    equal (run 2 [] 2) (Right ([('2', 'x')], []))
    equal (run 4 [(2, 1), (2, 4)] 2)
        (Right ([('2', 'x'), ('x', '1'), ('x', '4')], []))

test_insert_branch :: Test
test_insert_branch = do
    let run titles skel selTrack = CmdTest.extractUiState extract $
            runCmd titles skel (CmdTest.setPointSel selTrack 0
                >> Create.insertBranch)
        extract state = (map fst $ UiTest.extractTracks state,
            UiTest.extractSkeleton state)
    equal (run ["tempo", ">inst", "dyn"] [(1, 2), (2, 3)] 2) $ Right
        ( (["tempo", ">inst", "dyn", ">inst", "dyn"],
            [(1, 2), (1, 4), (2, 3), (4, 5)])
        , []
        )
    equal (run [">inst", "dyn"] [(1, 2)] 1) $ Right
        ( ([">inst", "dyn", ">inst", "dyn"],
            [(1, 2), (3, 4)])
        , []
        )

test_make_tracks :: Test
test_make_tracks = do
    let f tracknum = Create.makeTracks tracknum . makeTree
        makeTree :: [Tree.Tree Text] -> TrackTree.TrackTree
        makeTree = map $ fmap $ \title -> Ui.TrackInfo title
            (UiTest.mkTid 1) 1 (UiTest.btrack (UiTest.mkTid 1))
    equal (f 1
            [ Tree.Node "1" [Tree.Node "11" [], Tree.Node "12" []]
            , Tree.Node "2" []
            ])
        ([(1, "1"), (2, "11"), (3, "12"), (4, "2")], [(1, 2), (1, 3)])

runCmd :: [Text] -> [(TrackNum, TrackNum)] -> Cmd.CmdId a -> CmdTest.Result a
runCmd titles skel cmd = CmdTest.run state CmdTest.defaultCmdState cmd
    where
    state = UiTest.exec Ui.empty $ do
        UiTest.mkblocksSkel [((UiTest.defaultBlockName,
            [(title, []) | title <- titles]), skel)]
        UiTest.mkview UiTest.defaultBlockId

runSkelPoint :: Cmd.CmdId a -> Int -> [Skeleton.Edge] -> TrackNum
    -> Either Text ([(Char, Char)], [Text])
runSkelPoint m ntracks skel track = runSkel m ntracks skel (track, track)

-- | Put the selection on the given track, create a block with the given
-- skeleton, and run the cmd.  Return the skeleton as a list of edges,
-- replacing the tracknums with the track titles.  Tracks with no title (likely
-- new tracks) get an 'x'.
runSkel :: Cmd.CmdId a -> Int -> [Skeleton.Edge] -> (TrackNum, TrackNum)
    -> Either Text ([(Char, Char)], [Text])
    -- ^ (edges as titles, logs)
runSkel m ntracks skel (startTrack, endTrack) =
    extract $ CmdTest.runTracks tracks $ do
        Ui.setSkeleton UiTest.defaultBlockId (Skeleton.make skel)
        CmdTest.setSel startTrack 0 endTrack 0
        m
    where
    tracks = [(showt (n+1), []) | n <- [0..ntracks-1]]
    extract = CmdTest.extractUiState extractSkel
    extractSkel ustate = UiTest.eval ustate $ do
        skel <- Skeleton.flatten <$> Ui.getSkeleton UiTest.defaultBlockId
        mapM (\(t1, t2) -> (,) <$> replace t1 <*> replace t2) skel
    replace n = do
        tid <- Ui.getEventTrackAt UiTest.defaultBlockId n
        title <- Track.trackTitle <$> Ui.getTrack tid
        pure $ Text.head $
            if Text.null (Text.strip title) then "x" else title

test_find_rect :: Test
test_find_rect = do
    let f w = Create.findRect (Just (Rect.xywh 0 0 10 10)) (w, 0) . map rect
        rect (x1, x2) = Rect.xywh x1 0 (x2-x1) 10
    equal (f 2 []) (0, 0)
    equal (f 2 [(0, 2)]) (2, 0)
    -- 2--4 is a better fit.
    equal (f 2 [(0, 2), (4, 6)]) (2, 0)
    -- Now 6--8 is.
    equal (f 2 [(0, 2), (3, 6)]) (6, 0)
    -- But if it's stopped up, we'll take 2--3.
    equal (f 2 [(0, 2), (3, 10)]) (2, 0)
