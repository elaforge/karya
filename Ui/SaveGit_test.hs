module Ui.SaveGit_test where
import qualified System.Directory as Directory

import Util.Control
import qualified Util.File as File
import qualified Util.Git as Git
import Util.Test

import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.Events as Events
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
    [(state1, commit1), (state2, commit2), (state3, commit3),
            (state4, commit4)] <- checkpoint_sequence repo
        [ (,) "create" $ mkview
            [ ("1", [(0, 1, "1a"), (1, 1, "1b")])
            , ("2", [(0, 1, "2a")])
            ]
        , (,) "hi" $ State.insert_event (UiTest.mk_tid 1) 2 (Event.event "hi" 2)
        , (,) "new track" $ do
            State.destroy_track (UiTest.mk_tid 2)
            void $ State.create_track (Id.unpack_id (UiTest.mk_tid 2)) $
                Track.track "new" Events.empty
        , (,) "destroy" $ do
            State.destroy_view UiTest.default_view_id
            State.destroy_block UiTest.default_block_id
        ]
    -- TODO hook up a fs simulator so I can test this exhaustively without
    -- hitting git

    io_equal (SaveGit.load repo Nothing) (Right state4)
    -- Previous states load correctly.
    io_equal (SaveGit.load repo (Just commit1)) (Right state1)
    io_equal (SaveGit.load repo (Just commit2)) (Right state2)
    io_equal (SaveGit.load repo (Just commit3)) (Right state3)
    io_equal (SaveGit.load repo (Just commit4)) (Right state4)

    -- Make sure incremental loads work.
    (_, secs) <- timer $ do
        io_equal (SaveGit.load_from repo commit1 (Just commit2) state1)
            (Right (state2, []))
        io_equal (SaveGit.load_from repo commit2 (Just commit3) state2)
            (Right (state3, []))
        io_equal (SaveGit.load_from repo commit3 (Just commit4) state3)
            (Right (state4, []))
        io_equal (SaveGit.load_from repo commit1 (Just commit4) state1)
            (Right (state4, []))
    print secs

-- TODO do more exhaustive testing
check_sequence = do
    repo <- new_repo
    void $ print_timer "sequence" (const "") $ checkpoint_sequence repo $
        [ (,) "create" $ mkview
            [ ("1", [(0, 1, "1a"), (1, 1, "1b")])
            , ("2", [(0, 1, "2a")])
            ]
        ] ++ [('+' : show n, insert_event 2 1 (show n) 1) | n <- [0..20]]
    -- mapM_ (check_load repo) states
    -- mapM_ (uncurry (check_load_from repo)) (zip states (drop 1 states))

insert_event tracknum pos text dur =
    State.insert_event (UiTest.mk_tid tracknum) pos (Event.event text dur)

check_load :: FilePath -> (State.State, Git.Commit) -> IO Bool
check_load repo (state, commit) =
    io_equal (SaveGit.load repo (Just commit)) (Right state)

check_load_from :: FilePath -> (State.State, Git.Commit)
    -> (State.State, Git.Commit) -> IO Bool
check_load_from repo (state1, commit1) (state2, commit2) =
    io_equal (SaveGit.load_from repo commit1 (Just commit2) state1)
        (Right (state2, []))

checkpoint_sequence :: Git.Repo -> [(String, State.StateId ())]
    -> IO [(State.State, Git.Commit)]
checkpoint_sequence repo actions = apply State.empty actions
    where
    apply _ [] = return []
    apply prev_state ((name, action) : actions) = do
        let (state, updates) = diff prev_state action
        Right commit <- SaveGit.checkpoint repo
            (SaveGit.History state updates [name])
        rest <- apply state actions
        return $ (state, commit) : rest

diff :: State.State -> State.StateId a -> (State.State, [Update.CmdUpdate])
diff state modify = case Diff.diff cmd_updates state state2 of
        Left err -> error $ "diff: " ++ show err
        Right (updates, _) -> (state2, updates)
    where
    (state2, cmd_updates) = case State.run_id state modify of
        Left err -> error $ "State.run: " ++ show err
        Right (_, state, updates) -> (state, updates)

mkview :: [UiTest.TrackSpec] -> State.StateId ()
mkview tracks = void $ UiTest.mkblock_view (UiTest.default_block_name, tracks)

new_repo = do
    let repo = "build/test/test-repo"
    File.ignore_enoent $ Directory.removeDirectoryRecursive repo
    return repo
