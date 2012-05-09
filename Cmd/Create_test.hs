module Cmd.Create_test where
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create

import Types


test_track_ruler = do
    let f tracknum = Create.track_events UiTest.default_block_id
            State.no_ruler tracknum 20 Track.empty
        run track_specs cmd = CmdTest.trace_logs $ CmdTest.e_tracks $
            CmdTest.run_tracks track_specs cmd
    equal (run [] (f 0)) $ Right [("", [])]
    equal (run [] (f 10)) $ Right [("", [])]
    equal (run [("1", [])] (f 10)) $ Right [("1", []), ("", [])]
    equal (run [("1", [])] (f 0)) $ Right [("", []), ("1", [])]

-- test_splice_above_all = do
--     let run = run_skel_point Create.splice_above_all
--     -- Splice the new track above 2.
--     equal (run 2 [(1, 2)] 2) (Right ([('1', 'x'), ('x', '2')], []))
--
--     -- No track above, becomes parent to all toplevel tracks.
--     equal (run 2 [] 2) (Right ([('x', '1'), ('x', '2')], []))
--     equal (run 3 [(1, 2)] 1) (Right ([('x', '1'), ('x', '3'), ('1', '2')], []))
--
--     -- Track above, becomes parent to siblings.
--     equal (run 5 [(1, 2), (2, 3), (2, 4)] 3)
--         (Right ([('1', '2'), ('2', 'x'), ('x', '3'), ('x', '4')], []))
--     -- Make sure it goes to the right of the parent.
--     equal (run 5 [(1, 3), (3, 4)] 3)
--         (Right ([('1', 'x'), ('x', '3'), ('3', '4')], []))

test_splice_above_ancestors = do
    let run = run_skel Create.splice_above_ancestors
    equal (run 2 [(1, 2)] (2, 2)) $ Right ([('x', '1'), ('1', '2')], [])
    equal (run 4 [(1, 2), (3, 4)] (4, 4)) $
        Right ([('1', '2'), ('x', '3'), ('3', '4')], [])
    equal (run 4 [(1, 2), (3, 4)] (1, 4)) $
        Right ([('x', '1'), ('x', '3'), ('1', '2'), ('3', '4')], [])

test_splice_below = do
    let run = run_skel_point Create.splice_below
    equal (run 2 [] 2) (Right ([('2', 'x')], []))
    equal (run 4 [(2, 1), (2, 4)] 2)
        (Right ([('2', 'x'), ('x', '1'), ('x', '4')], []))

run_skel_point m ntracks skel track = run_skel m ntracks skel (track, track)

-- | Put the selection on the given track, create a block with the given
-- skeleton, and run the cmd.  Return the skeleton as a list of edges,
-- replacing the tracknums with the track titles.  The new track is named @x@.
run_skel :: Cmd.CmdId a -> Int -> [Skeleton.Edge] -> (TrackNum, TrackNum)
    -> Either String ([(Char, Char)], [String])
run_skel m ntracks skel (start_track, end_track) =
    extract $ CmdTest.run_tracks tracks $ do
        State.set_skeleton UiTest.default_block_id (Skeleton.make skel)
        CmdTest.set_sel start_track 0 end_track 0
        m
    where
    tracks = [(show (n+1), []) | n <- [0..ntracks-1]]
    extract = CmdTest.extract_state $ \ustate _ -> extract_skel ustate
    extract_skel ustate = UiTest.eval ustate $ do
        skel <- List.sort . Skeleton.flatten <$>
            State.get_skeleton UiTest.default_block_id
        mapM (\(t1, t2) -> (,) <$> replace t1 <*> replace t2) skel
    replace n = do
        tid <- State.get_event_track_at "" UiTest.default_block_id n
        title <- Track.track_title <$> State.get_track tid
        return $ head $ if null (Seq.strip title) then "x" else title
