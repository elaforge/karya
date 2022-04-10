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
    let f tracknum = Create.track_events UiTest.default_block_id
            Ui.no_ruler tracknum 20 Track.empty
        run track_specs cmd = CmdTest.trace_logs $ CmdTest.e_tracks $
            CmdTest.run_tracks track_specs cmd
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
    let run = run_skel Create.splice_above_ancestors
    equal (run 2 [(1, 2)] (2, 2)) $ Right ([('x', '1'), ('1', '2')], [])
    equal (run 4 [(1, 2), (3, 4)] (4, 4)) $
        Right ([('1', '2'), ('x', '3'), ('3', '4')], [])
    equal (run 4 [(1, 2), (3, 4)] (1, 4)) $
        Right ([('x', '1'), ('x', '3'), ('1', '2'), ('3', '4')], [])

test_splice_below :: Test
test_splice_below = do
    let run = run_skel_point Create.splice_below
    equal (run 2 [] 2) (Right ([('2', 'x')], []))
    equal (run 4 [(2, 1), (2, 4)] 2)
        (Right ([('2', 'x'), ('x', '1'), ('x', '4')], []))

test_insert_branch :: Test
test_insert_branch = do
    let run titles skel sel_track = CmdTest.extract_ui_state extract $
            run_cmd titles skel (CmdTest.set_point_sel sel_track 0
                >> Create.insert_branch)
        extract state = (map fst $ UiTest.extract_tracks state,
            UiTest.extract_skeleton state)
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
    let f tracknum = Create.make_tracks tracknum . make_tree
        make_tree :: [Tree.Tree Text] -> TrackTree.TrackTree
        make_tree = map $ fmap $ \title -> Ui.TrackInfo title
            (UiTest.mk_tid 1) 1 (UiTest.btrack (UiTest.mk_tid 1))
    equal (f 1
            [ Tree.Node "1" [Tree.Node "11" [], Tree.Node "12" []]
            , Tree.Node "2" []
            ])
        ([(1, "1"), (2, "11"), (3, "12"), (4, "2")], [(1, 2), (1, 3)])

run_cmd :: [Text] -> [(TrackNum, TrackNum)] -> Cmd.CmdId a -> CmdTest.Result a
run_cmd titles skel cmd = CmdTest.run state CmdTest.default_cmd_state cmd
    where
    state = UiTest.exec Ui.empty $ do
        UiTest.mkblocks_skel [((UiTest.default_block_name,
            [(title, []) | title <- titles]), skel)]
        UiTest.mkview UiTest.default_block_id

run_skel_point :: Cmd.CmdId a -> Int -> [Skeleton.Edge] -> TrackNum
    -> Either Text ([(Char, Char)], [Text])
run_skel_point m ntracks skel track = run_skel m ntracks skel (track, track)

-- | Put the selection on the given track, create a block with the given
-- skeleton, and run the cmd.  Return the skeleton as a list of edges,
-- replacing the tracknums with the track titles.  Tracks with no title (likely
-- new tracks) get an 'x'.
run_skel :: Cmd.CmdId a -> Int -> [Skeleton.Edge] -> (TrackNum, TrackNum)
    -> Either Text ([(Char, Char)], [Text])
    -- ^ (edges as titles, logs)
run_skel m ntracks skel (start_track, end_track) =
    extract $ CmdTest.run_tracks tracks $ do
        Ui.set_skeleton UiTest.default_block_id (Skeleton.make skel)
        CmdTest.set_sel start_track 0 end_track 0
        m
    where
    tracks = [(showt (n+1), []) | n <- [0..ntracks-1]]
    extract = CmdTest.extract_ui_state extract_skel
    extract_skel ustate = UiTest.eval ustate $ do
        skel <- Skeleton.flatten <$> Ui.get_skeleton UiTest.default_block_id
        mapM (\(t1, t2) -> (,) <$> replace t1 <*> replace t2) skel
    replace n = do
        tid <- Ui.get_event_track_at UiTest.default_block_id n
        title <- Track.track_title <$> Ui.get_track tid
        return $ Text.head $
            if Text.null (Text.strip title) then "x" else title

test_find_rect :: Test
test_find_rect = do
    let f w = Create.find_rect (Just (Rect.xywh 0 0 10 10)) (w, 0) . map rect
        rect (x1, x2) = Rect.xywh x1 0 (x2-x1) 10
    equal (f 2 []) (0, 0)
    equal (f 2 [(0, 2)]) (2, 0)
    -- 2--4 is a better fit.
    equal (f 2 [(0, 2), (4, 6)]) (2, 0)
    -- Now 6--8 is.
    equal (f 2 [(0, 2), (3, 6)]) (6, 0)
    -- But if it's stopped up, we'll take 2--3.
    equal (f 2 [(0, 2), (3, 10)]) (2, 0)
