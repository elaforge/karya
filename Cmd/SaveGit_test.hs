-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.SaveGit_test where
import qualified Data.Map as Map

import qualified Util.Git as Git
import qualified Util.Ranges as Ranges
import qualified Util.Test.Testing as Testing

import qualified Cmd.Create as Create
import qualified Cmd.SaveGit as SaveGit
import qualified Ui.Diff as Diff
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import           Global
import           Util.Test


test_do_save = Git.initialize $ do
    repo <- new_repo
    let state = snd $ UiTest.run_mkview
            [ ("1", [(0, 1, "1a"), (1, 1, "1b")])
            , ("2", [(0, 1, "2a")])
            ]
    SaveGit.save user repo state ["save"]
    state2 <- fmap (\(a, _, _) -> a) <$> SaveGit.load repo Nothing
    right_equal (strip_views <$> state2) (strip_views state)

test_checkpoint = Git.initialize $ do
    repo <- new_repo
    [(state1, commit1), (state2, commit2), (state3, commit3),
            (state4, commit4)] <- checkpoint_sequence repo
        [ (,) "create" $ mkview
            [ ("1", [(0, 1, "1a"), (1, 1, "1b")])
            , ("2", [(0, 1, "2a")])
            ]
        , (,) "hi" $ UiTest.insert_event 1 (2, 2, "hi")
        , (,) "new track" $ do
            Ui.destroy_track (UiTest.mk_tid 2)
            void $ Ui.create_track (Id.unpack_id (UiTest.mk_tid 2)) $
                Track.track "new" Events.empty
        , (,) "destroy" $ do
            Ui.destroy_view UiTest.default_view_id
            Ui.destroy_block UiTest.default_block_id
        ]
    io_equal (Git.read_log_head repo) [commit4, commit3, commit2, commit1]

    io_equal (SaveGit.load repo Nothing) (Right (state4, commit4, ["destroy"]))
    -- Previous states load correctly.
    io_equal (SaveGit.load repo (Just commit1)) $
        Right (state1, commit1, ["create"])
    io_equal (SaveGit.load repo (Just commit2)) $
        Right (state2, commit2, ["hi"])
    io_equal (SaveGit.load repo (Just commit3)) $
        Right (state3, commit3, ["new track"])
    io_equal (SaveGit.load repo (Just commit4)) $
        Right (state4, commit4, ["destroy"])

    let damage num start end = mempty
            { Update._tracks =
                Map.singleton (UiTest.mk_tid num) (Ranges.range start end)
            }
    -- Make sure incremental loads work.
    io_equal (load_from repo commit1 (Just commit2) state1)
        -- UiTest.insert_event 1 (2, 2, "hi")
        (Right (state2, damage 1 2 4))
    io_equal (load_from repo commit2 (Just commit3) state2)
        -- destroy_track 2, create_track 2, but generate no updates
        -- because normal diff will catch that.
        (Right (state3, mempty))
    io_equal (load_from repo commit3 (Just commit4) state3)
        (Right (state4, mempty))
    io_equal (load_from repo commit1 (Just commit4) state1)
        (Right (state4, damage 1 2 4))

test_ruler_checkpoint = Git.initialize $ do
    repo <- new_repo
    states <- checkpoint_sequence repo
        [ ("create", mkview [("1", [])])
        , (,) "destroy" $ do
            Ui.modify_ruler UiTest.default_ruler_id
                (const (Right Ruler.no_ruler))
            Ui.destroy_ruler UiTest.default_ruler_id
        ]
    -- Mostly just verify that when a ruler is modified and deleted the update
    -- will be cancelled by 'Ui.Diff.cancel_updates' and won't crash or log
    -- here.  Of course I can't test for the log :/
    equal (length states) 2

test_more_checkpoints = Git.initialize $ check_sequence
    [ mkview [("1", [])]
    , void $ Create.block_from_template False UiTest.default_block_id
    , void $ Create.empty_track UiTest.default_block_id 2
    , Create.destroy_track UiTest.default_block_id 2
    , Create.destroy_block UiTest.default_block_id
    ]

check_sequence :: [Ui.StateId ()] -> IO ()
check_sequence actions = do
    repo <- new_repo
    state_commits <- checkpoint_sequence repo (zip (map showt [0..]) actions)
    forM_ (zip state_commits (drop 1 state_commits)) $
        \((state1, commit1), (state2, commit2)) -> do
            io_equal (load_from repo commit1 (Just commit2) state1)
                (Right (state2, mempty))

load_from :: Git.Repo -> SaveGit.Commit -> Maybe SaveGit.Commit -> Ui.State
    -> IO (Either Text (Ui.State, Update.UiDamage))
load_from repo commit_from maybe_commit_to state =
    fmap (first strip_views) <$>
        SaveGit.load_from repo commit_from maybe_commit_to state

-- | Views aren't saved, so I shouldn't compare them.
strip_views :: Ui.State -> Ui.State
strip_views state = state { Ui.state_views = mempty }

check_load :: FilePath -> (Ui.State, SaveGit.Commit, [Text]) -> IO ()
check_load repo (state, commit, names) =
    io_equal (SaveGit.load repo (Just commit)) (Right (state, commit, names))

check_load_from :: FilePath -> (Ui.State, SaveGit.Commit)
    -> (Ui.State, SaveGit.Commit) -> IO ()
check_load_from repo (state1, commit1) (state2, commit2) =
    io_equal (load_from repo commit1 (Just commit2) state1)
        (Right (state2, mempty))

checkpoint_sequence :: Git.Repo -> [(Text, Ui.StateId ())]
    -> IO [(Ui.State, SaveGit.Commit)]
checkpoint_sequence repo actions = apply (Ui.empty, Nothing) actions
    where
    apply _ [] = return []
    apply (prev_state, prev_commit) ((name, action) : actions) = do
        let (state, ui_updates) = diff prev_state action
        Right commit <- SaveGit.checkpoint user repo
            (SaveGit.SaveHistory state prev_commit ui_updates [name])
        rest <- apply (state, Just commit) actions
        return $ (strip_views state, commit) : rest

diff :: Ui.State -> Ui.StateId a -> (Ui.State, [Update.UiUpdate])
diff state modify = (state2, ui_updates)
    where
    (ui_updates, _) = Diff.diff damage state state2
    (state2, damage) = case Ui.run_id state modify of
        Left err -> error $ "Ui.run: " ++ show err
        Right (_, state, damage) -> (state, damage)

mkview :: [UiTest.TrackSpec] -> Ui.StateId ()
mkview tracks = void $ UiTest.mkblock_view (UiTest.default_block_name, tracks)

new_repo :: IO FilePath
new_repo = (++ SaveGit.git_suffix) <$> Testing.tmp_dir "git"

user :: SaveGit.User
user = SaveGit.User "name" "email"
