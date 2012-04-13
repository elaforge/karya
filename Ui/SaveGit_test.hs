module Ui.SaveGit_test where
import qualified System.Directory as Directory

import qualified Util.File as File
import Util.Test
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.SaveGit as SaveGit
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update


test_save = do
    repo <- new_repo
    let state = snd $ UiTest.run_mkview
            [ ("1", [(0, 1, "1a"), (1, 1, "1b")])
            , ("2", [(0, 1, "2a")])
            ]
    SaveGit.save repo state
    Right state2 <- SaveGit.load repo Nothing
    equal state state2
    let state3 = UiTest.exec state2 $ do
            State.destroy_view UiTest.default_view_id
            State.insert_event (UiTest.mk_tid 1) 2 (Event.event "hi" 2)
    SaveGit.save repo state3

test_checkpoint = do
    repo <- new_repo
    let state1 = snd $ UiTest.run_mkview
            [ ("1", [(0, 1, "1a"), (1, 1, "1b")])
            , ("2", [(0, 1, "2a")])
            ]
    let (state2, updates2) = diff state1 $
            State.insert_event (UiTest.mk_tid 1) 2 (Event.event "hi" 2)
        (state3, updates3) = diff state2 $ do
            State.destroy_track (UiTest.mk_tid 2)
            State.create_track (Id.unpack_id (UiTest.mk_tid 2)) $
                Track.track "new" []
    -- TODO hook up a fs simulator so I can test this exhaustively without
    -- hitting git

    -- No previous checkpoints, so this is a full save.
    Right commit1 <- SaveGit.checkpoint repo state1 []
    Right commit2 <- SaveGit.checkpoint repo state2 updates2
    Right commit3 <- SaveGit.checkpoint repo state3 updates3
    io_equal (SaveGit.load repo Nothing) (Right state3)
    -- Previous states load correctly.
    io_equal (SaveGit.load repo (Just commit1)) (Right state1)
    io_equal (SaveGit.load repo (Just commit2)) (Right state2)
    io_equal (SaveGit.load repo (Just commit3)) (Right state3)

diff :: State.State -> State.StateId a -> (State.State, [Update.CmdUpdate])
diff state modify = case Diff.diff cmd_updates state state2 of
        Left err -> error $ "diff: " ++ show err
        Right (updates, _) -> (state2, updates)
    where
    (state2, cmd_updates) = case State.run_id state modify of
        Left err -> error $ "State.run: " ++ show err
        Right (_, state, updates) -> (state, updates)


new_repo = do
    let repo = "build/test/test-repo"
    File.ignore_enoent $ Directory.removeDirectoryRecursive repo
    return repo
