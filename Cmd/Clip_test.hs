module Cmd.Clip_test where
import qualified Control.Monad.Identity as Identity

import Util.Test
import qualified Util.Log as Log

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.TestSetup as TestSetup

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Clip as Clip
import qualified Cmd.Simple as Simple

import qualified App.Config as Config

import Util.PPrint
import qualified Data.Maybe as Maybe


run = TestSetup.run State.empty

(empty_track_ids, empty_state) = run $ TestSetup.mkstate_view
    TestSetup.default_block_name
    [("t1", []), ("t2", [])]

(events_track_ids, events_state) = run $ TestSetup.mkstate_view
    TestSetup.default_block_name
    [ ("t1", track1_events)
    , ("t2", track2_events)
    ]

track1_events = [(0, 2, "e11"), (4, 2, "e12"), (8, 2, "e13")]
track2_events = [(1, 2, "e21"), (5, 2, "e22")]

(clip_track_ids, clip_state) = run $ TestSetup.mkstate_id
    clip_id
    [ ("t1", [(0, 2, "c1"), (4, 2, "c2")]) ]

clip_ns = Config.clip_namespace
clip_id = Block.BlockId (Id.id clip_ns Config.clip_block_name)

with_clip state cstate = State.error_either "with_clip" $
    State.exec state (Clip.state_to_namespace cstate clip_ns)

-- | Run a cmd and return everything you could possibly be interested in.
-- Will be Nothing if the cmd aborted.
run_cmd :: State.State -> Cmd.State -> Cmd.CmdT Identity.Identity a
    -> Maybe (Either State.StateError
        (a, Cmd.State, State.State, [Log.Msg]))
run_cmd ui_state cmd_state cmd = case Cmd.run_id ui_state cmd_state cmd of
    (cmd_state2, _midi_msgs, logs, result) -> case result of
        Left err -> Just (Left err)
        Right (val, ui_state2, _updates) -> case val of
            Nothing -> Nothing
            Just v -> Just (Right (v, cmd_state2, ui_state2, logs))

run_clip ui_state cmd = run_cmd ui_state default_cmd_state cmd

extract_block block_id (val, _cmd_state, ui_state2, logs) = (val, block, logs)
    where Right block = State.eval ui_state2 (Simple.dump_block block_id)

result_val (val, _, _) = val
state_val (_, state, _) = state

default_cmd_state = Cmd.empty_state
    { Cmd.state_clip_namespace = clip_ns
    , Cmd.state_focused_view = Just TestSetup.default_view_id
    }

run_io :: State.State -> Cmd.CmdT Identity.Identity a
          -> IO (a, Cmd.State, State.State, [Log.Msg])
run_io state m = do
    res <- case run_clip state m of
        Just (Right res) -> return res
        Just (Left err) -> error $ "state error: " ++ show err
        Nothing -> error "abort"
    return res

extract_events (_, _, tracks) = map (\(_, _, a) -> a) tracks
mksel a b c d = Just (Block.Selection a b c d)

run_sel state sel cmd = run_io state $ do
    State.set_selection TestSetup.default_view_id Config.insert_selnum sel
    cmd

set_sel sel = State.set_selection TestSetup.default_view_id Config.insert_selnum sel

-- * copy

run_copy state sel cmd = fmap
    (extract_events . state_val . extract_block clip_id)
    (run_sel state sel cmd)

test_cmd_copy_selection = do
    let state = events_state
    let run = run_copy state

    io_equal (run (mksel 1 4 1 8) Clip.cmd_copy_selection)
        [[(0, 2, "e12")]]
    -- I get the same event, but also the empty space before it.
    -- Events are not clipped.
    io_equal (run (mksel 1 1 1 5) Clip.cmd_copy_selection)
        [[(3, 2, "e12")]]

    io_equal (run (mksel 1 1 2 5) Clip.cmd_copy_selection)
        [ [(3, 2, "e12")]
        , [(0, 2, "e21")]
        ]

test_namespace_ops = do
    let run state cmd = do fmap (\(_, _, state, _) -> state) (run_io state cmd)

    state <- run events_state $ do
        set_sel $ mksel 1 1 2 4
        Clip.cmd_copy_selection
    -- TODO it's too hard at the moment to make sure they have been set, so
    -- I'll just make sure the state is at least valid
    pprint (State.structure state)
    pprint (snd (State.verify state))
    check_msg $ valid_state state

    state <- run state $ Clip.clear_clip
    pprint (State.structure state)
    check_msg $ valid_state state

valid_state state = (null msgs, show msgs)
    where (_, msgs) = State.verify state

-- * paste

run_paste state sel cmd = fmap
    (extract_events . state_val . extract_block TestSetup.default_block_id)
    (run_sel state sel cmd)

test_cmd_paste_overwrite = do
    state <- with_clip events_state clip_state
    let run = run_paste state

    -- From sel onwards replaced by clipboard.
    res <- run (mksel 1 1 1 1) Clip.cmd_paste_overwrite
    equal res
        [ [(0, 1, "e11"), (1, 2, "c1"), (5, 2, "c2"), (8, 2, "e13")]
        , track2_events
        ]

    -- Second track isn't overwritten because clip has no second track.
    -- So this is the same as above.
    res <- run (mksel 1 1 2 1) Clip.cmd_paste_overwrite
    equal res
        [ [(0, 1, "e11"), (1, 2, "c1"), (5, 2, "c2"), (8, 2, "e13")]
        , track2_events
        ]

    -- Only replace the second event, since clipboard is clipped to sel.
    res <- run (mksel 1 1 1 4) Clip.cmd_paste_overwrite
    equal res
        [ [(0, 1, "e11"), (1, 2, "c1"), (4, 2, "e12"), (8, 2, "e13")]
        , track2_events
        ]

    -- Pasted events are clipped to the selection.
    res <- run (mksel 1 2 1 7) Clip.cmd_paste_overwrite
    equal res
        [ [(0, 2, "e11"), (2, 2, "c1"), (6, 1, "c2"), (8, 2, "e13")]
        , track2_events
        ]

test_cmd_paste_merge = do
    state <- with_clip events_state clip_state
    let run = run_paste state

    res <- run (mksel 1 1 1 1) Clip.cmd_paste_merge
    equal res
        [ [(0, 1, "e11"), (1, 2, "c1"), (4, 1, "e12"), (5, 2, "c2"),
            (8, 2, "e13")]
        , track2_events
        ]
    -- Not much more to test here since it's all the same as
    -- cmd_paste_overwrite.

    -- They all overlap.
    res <- run (mksel 1 1 1 1) Clip.cmd_paste_soft_merge
    equal res [track1_events, track2_events]

    -- This time they make it in.
    res <- run (mksel 1 2 1 2) Clip.cmd_paste_soft_merge
    equal res
        [ [(0, 2, "e11"), (2, 2, "c1"), (4, 2, "e12"), (6, 2, "c2"),
            (8, 2, "e13")]
        , track2_events
        ]

test_cmd_paste_insert = do
    state <- with_clip events_state clip_state
    let run = run_paste state

    -- Point selection pushes by inserted length.
    res <- run (mksel 1 1 1 1) Clip.cmd_paste_insert
    equal res
        [ [(0, 1, "e11"), (1, 2, "c1"), (5, 2, "c2"),
            (10, 2, "e12"), (14, 2, "e13")]
        , track2_events
        ]

    -- selection pushes by selection length
    res <- run (mksel 1 1 1 3) Clip.cmd_paste_insert
    equal res
        [ [(0, 1, "e11"), (1, 2, "c1"), (6, 2, "e12"), (10, 2, "e13")]
        , track2_events
        ]
