module Ui.ApplyUpdate_test where
import Control.Monad
import qualified Control.Monad.Identity as Identity

import Util.Test
import qualified Ui.ApplyUpdate as ApplyUpdate
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import Ui.UiTest (mkid)
import qualified Ui.Update as Update


test_apply = do
    -- This would be a good spot for quickcheck.
    -- Unfortunately I think an Arbitrary instance for State.State is hard.
    consistent id State.empty $ State.create_block (mkid "b")
        (UiTest.mkblock "title" [])
    consistent id State.empty (State.set_namespace "hoho")
    let ([tid1, tid2], st) = UiTest.run_mkview [(">", []), ("*", [])]
    consistent State.state_blocks st $
        State.remove_track UiTest.default_block_id 2
    consistent State.state_tracks st $
        State.insert_event tid1 0 (Event.event "hi" 1)
    consistent State.state_tracks st $ do
        State.insert_event tid1 0 (Event.event "hi" 1)
        State.insert_event tid2 1 (Event.event "there" 1)
    consistent State.state_views st $
        State.destroy_view UiTest.default_view_id

test_apply_failure = do
    let (_, st) = UiTest.run_mkview [(">", []), ("*", [])]
    let f update = ApplyUpdate.apply update st
    left_like (f $ Update.ViewUpdate (UiTest.vid "x") Update.DestroyView)
        "doesn't exist"
    left_like (f $ Update.ViewUpdate (UiTest.default_view_id)
            (Update.CreateView UiTest.default_view))
        "already exists"
    left_like (f $ Update.BlockUpdate UiTest.default_block_id
            (Update.RemoveTrack 42))
        "can't remove index 42"
    let div = Block.DId (Block.Divider Color.blue)
    left_like (f $ Update.BlockUpdate UiTest.default_block_id
            (Update.BlockTrack 42 (Block.track div 10)))
        "can't replace index 42"


consistent :: (Show a, Eq a) => (State.State -> a)
    -> State.State -> State.StateId b -> IO ()
consistent extract state1 modify = case run_state state1 modify of
    Left err -> failure $ "modify state failed: " ++ show err
    Right (state2, cmd_updates) -> case Diff.diff cmd_updates state1 state2 of
        Left err -> failure $ "diff failed: " ++ show err
        Right (cmd_updates, _) -> case apply_all state1 cmd_updates of
            Left err -> failure $ "apply failed: " ++ err
            Right state2b -> do
                pmlist "updates" cmd_updates
                equal (extract state2) (extract state2b)

run_state :: State.State -> State.StateId a
    -> Either State.StateError (State.State, [Update.CmdUpdate])
run_state state m = case Identity.runIdentity (State.run state m) of
    Left err -> Left err
    Right (_, state, updates) -> Right (state, updates)

apply_all :: State.State -> [Update.CmdUpdate] -> Either String State.State
apply_all = foldM (flip ApplyUpdate.apply)

apply_equal :: Update.CmdUpdate -> State.State -> IO ()
apply_equal update state1 = case ApplyUpdate.apply update state1 of
    Left err -> failure err
    Right state2 -> equal state1 state2
