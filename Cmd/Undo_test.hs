-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Undo_test where
import qualified Data.Map as Map

import qualified Util.File as File
import qualified Util.Git as Git
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.Testing as Testing

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Selection as Selection
import qualified Cmd.Undo as Undo

import qualified App.Config as Config
import Global
import Types


test_undo = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]

    res <- ResponderTest.respond_cmd states $ Cmd.name "+z" $ insert_event 0 "z"
    res <- next res $ Cmd.name "+q" $ insert_event 1 "q"

    equal (extract_ui res) "zq"
    equal (e_updates res) [track_update 1 1 2]

    res <- next res Undo.undo
    -- Make sure the past and future have the expected names and states.
    equal (e_hist_names res) (["setup: 12"], "+z: z2", ["+q: zq"])
    equal (extract_ui res) "z2"
    equal (e_updates res) [track_update 1 1 2]

    res <- next res Undo.undo
    equal (e_hist_names res) ([], "setup: 12", ["+z: z2", "+q: zq"])
    equal (extract_ui res) "12"
    equal (e_updates res) [track_update 1 0 1]

    -- no past to undo
    res <- next res Undo.undo
    equal (extract_ui res) "12"
    equal (e_updates res) []

    res <- next res Undo.redo
    equal (e_hist_names res) (["setup: 12"], "+z: z2", ["+q: zq"])
    equal (extract_ui res) "z2"
    equal (e_updates res) [track_update 1 0 1]

    res <- next res Undo.redo
    equal (extract_ui res) "zq"
    equal (e_hist_names res) (["+z: z2", "setup: 12"], "+q: zq", [])
    equal (e_updates res) [track_update 1 1 2]

    -- no future to redo
    res <- next res Undo.redo
    equal (extract_ui res) "zq"
    equal (e_updates res) []

test_suppress_history = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]
    let suppress = Cmd.suppress_history Cmd.ValEdit

    res <- ResponderTest.respond_cmd states $
        Cmd.name "toggle" Edit.cmd_toggle_val_edit
    res <- next res $ suppress "+z" $ insert_event 0 "z"
    equal (e_hist_names res) ([], "setup: 12", [])
    res <- next res $ suppress "+q" $ insert_event 1 "q"
    equal (e_hist_names res) ([], "setup: 12", [])
    -- A non-recording cmd will cause the suppressed cmds to be recorded.
    res <- next res $ set_sel 1
    res <- next res $ Cmd.name "toggle" Edit.cmd_toggle_val_edit
    equal (e_hist_names res) (["setup: 12"], "+z: zq", [])

test_undo_while_suppressed = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]
    let suppress = Cmd.suppress_history Cmd.ValEdit

    -- Unsuppressed.
    res <- ResponderTest.respond_cmd states $ Cmd.name "+a" $ insert_event 0 "a"

    -- Suppressed.
    res <- next res $ Cmd.name "toggle" Edit.cmd_toggle_val_edit
    res <- next res $ suppress "+b" $ insert_event 0 "b"
    -- The 'b' cmd is buried in suppression.
    equal (e_hist_names res) (["setup: 12"], "+a: a2", [])
    equal (extract_ui res) "b2"
    res <- next res Undo.undo
    -- Undo turns the suppressed cmd turns into a real cmd, and goes into the
    -- future.
    equal (e_hist_names res) (["setup: 12"], "+a: a2", ["+b: b2"])
    equal (extract_ui res) "a2"

    -- And I can go back to it.
    res <- next res Undo.redo
    equal (e_hist_names res) (["+a: a2", "setup: 12"], "+b: b2", [])
    equal (extract_ui res) "b2"

test_undo_merge = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [])]
        vid = UiTest.default_view_id
    res1 <- ResponderTest.respond_cmd states $ do
        State.set_namespace (Id.namespace "oogabooga")
        State.set_view_rect vid $ Rect.xywh 40 40 100 100
        insert_event 0 "z"
    res2 <- ResponderTest.respond_cmd (ResponderTest.result_states res1)
        Undo.undo
    -- some things aren't affected by undo
    equal (UiTest.eval (e_ui res1) (Block.view_rect <$> State.get_view vid))
        (Rect.xywh 40 40 100 100)
    equal (UiTest.eval (e_ui res2) (Block.view_rect <$> State.get_view vid))
        (Rect.xywh 40 40 100 100)

track_update :: TrackNum -> ScoreTime -> ScoreTime -> Update.DisplayUpdate
track_update tracknum from to = Update.Track (UiTest.mk_tid tracknum)
    (Update.TrackEvents from to)

test_load_previous_history = Git.initialize $ do
    repo <- get_repo
    -- Load a git repo and make sure its history comes with it.
    res <- save_git repo $ ResponderTest.mkstates [(">", [(0, 1, "1")])]
    res <- next res $ Cmd.name "+x" $ insert_event 0 "x"
    res <- next res $ Cmd.name "+y" $ insert_event 1 "y"
    -- pprint (e_commits res)
    -- pprint (e_hist_updates res)

    equal (extract_ui res) "xy"
    res <- ResponderTest.respond_cmd (ResponderTest.mkstates []) $
        Save.load_git repo Nothing
    equal (extract_ui res) "xy"

    res <- next res Undo.undo
    -- Previous history was loaded, y deleted.
    equal (e_hist_names res) ([], "+x: x", ["+y: xy"])
    equal (extract_ui res) "x"
    equal (e_updates res) [track_update 1 1 2]

    res <- next res Undo.undo
    -- Previous history was loaded, x replaced with 1.
    equal (e_hist_names res) ([], "save: 1", ["+x: x", "+y: xy"])
    equal (extract_ui res) "1"
    equal (e_updates res) [track_update 1 0 1]

    -- out of past
    res <- next res Undo.undo
    equal (extract_ui res) "1"
    equal (e_updates res) []

    res <- next res Undo.redo
    equal (extract_ui res) "x"
    equal (e_updates res) [track_update 1 0 1]
    res <- next res Undo.redo
    equal (extract_ui res) "xy"
    equal (e_updates res) [track_update 1 1 2]

test_load_next_history = Git.initialize $ do
    repo <- get_repo
    res <- save_git repo $ ResponderTest.mkstates [(">", [(0, 1, "1")])]
    res <- next res $ Cmd.name "+x" $ insert_event 0 "x"
    res <- next res $ Cmd.name "+y" $ insert_event 1 "y"
    pprint (e_commits res)
    -- The creation state wasn't committed, and the save isn't recorded in the
    -- history even though it has a commit, so start at the second-to-last.
    let ([ent, _], _, _) = e_commits res
        (_, Just commit) = ent
    res <- ResponderTest.respond_cmd (ResponderTest.mkstates []) $
        Save.load_git repo (Just commit)
    equal (e_hist_names res) ([], "+x: x", [])
    equal (extract_ui res) "x"

    res <- next res Undo.redo
    equal (extract_ui res) "xy"
    equal (e_hist_names res) (["+x: x"], "+y: xy", [])
    equal (e_updates res) [track_update 1 1 2]

    -- No future to redo.
    res <- next res Undo.redo
    equal (extract_ui res) "xy"
    equal (e_updates res) []

    -- Make sure I can do back again.
    res <- next res Undo.undo
    equal (extract_ui res) "x"
    equal (e_updates res) [track_update 1 1 2]

    res <- next res Undo.undo
    equal (extract_ui res) "1"
    equal (e_updates res) [track_update 1 0 1]

test_branching_history = Git.initialize $ do
    repo <- get_repo
    res <- save_git repo $ ResponderTest.mkstates [(">", [(0, 1, "1")])]
    res <- next res $ Cmd.name "+x" $ insert_event 0 "x"
    res <- next res $ Cmd.name "+y" $ insert_event 1 "y"
    res <- next res $ Cmd.name "save" Save.save_git
    res <- next res $ Cmd.name "revert" $ Save.revert (Just "0")
    equal (extract_ui res) "1"
    res <- next res $ Cmd.name "+a" $ insert_event 0 "a"
    res <- next res $ Cmd.name "+b" $ insert_event 1 "b"
    res <- next res $ Cmd.name "save" Save.save_git

    -- The second branch got 1.0 because 1 was taken.
    refs <- Git.read_ref_map repo
    equal (Map.keys refs) ["heads/master", "tags/0", "tags/1", "tags/1.0"]

    -- Each branch has its own history.
    io_equal (read_log repo =<< Git.read_log repo "tags/1.0")
        ["+b", "+a", "save"]
    io_equal (read_log repo =<< Git.read_log_head repo)
        ["+b", "+a", "save"]
    io_equal (read_log repo =<< Git.read_log repo "tags/1")
        ["+y", "+x", "save"]

    equal (extract_ui res) "ab"
    res <- next res $ Cmd.name "revert" $ Save.revert (Just "1")
    equal (extract_ui res) "xy"

read_log :: SaveGit.Repo -> [SaveGit.Commit] -> IO [Text]
read_log repo commits = do
    texts <- mapM (fmap Git.commit_text . Git.read_commit repo) commits
    mapM (fmap head . SaveGit.parse_names) texts

save_git :: SaveGit.Repo -> ResponderTest.States -> IO ResponderTest.Result
save_git repo states = do
    File.rmDirRecursive repo
    ResponderTest.respond_cmd states (Save.save_git_as repo)

get_repo :: IO SaveGit.Repo
get_repo = (++ SaveGit.git_suffix) <$> Testing.unique_tmp_dir "git"

-- * implementation

next :: ResponderTest.Result -> Cmd.CmdT IO a -> IO ResponderTest.Result
next = ResponderTest.respond_cmd . ResponderTest.result_states

insert_event :: State.M m => ScoreTime -> String -> m ()
insert_event pos text = UiTest.insert_event 1 (pos, 1, text)

set_sel :: Cmd.M m => ScoreTime -> m ()
set_sel pos = Cmd.name "select" $
    Selection.set_current Config.insert_selnum (Just (Sel.point 1 pos))


-- ** extract

e_hist_names :: ResponderTest.Result -> ([String], String, [String])
e_hist_names = extract_hist $ \(Cmd.HistoryEntry state _ names _) ->
    Seq.join "+" (map untxt names) ++ ": " ++ ui_notes 0 state

extract_hist :: (Cmd.HistoryEntry -> a) -> ResponderTest.Result -> ([a], a, [a])
extract_hist extract res =
    (map extract past, extract present, map extract future)
    where Cmd.History past present future _ = e_hist res

type Commit = ([Text], Maybe SaveGit.Commit)

e_commits :: ResponderTest.Result -> ([Commit], Commit, [Commit])
e_commits res = (map extract past, extract present, map extract future)
    where
    Cmd.History past present future _ = e_hist res
    extract hist = (Cmd.hist_names hist, Cmd.hist_commit hist)

extract_ui :: ResponderTest.Result -> String
extract_ui = ui_notes 0 . e_ui

ui_notes :: Int -> State.State -> String
ui_notes tracknum ui_state = [c | (_, _, c:_) <- tracks]
    where ('>' : _, tracks) = UiTest.extract_tracks ui_state !! tracknum

e_ui :: ResponderTest.Result -> State.State
e_ui = CmdTest.result_ui_state . ResponderTest.result_cmd

e_hist_updates :: ResponderTest.Result
    -> ([[Update.CmdUpdate]], [Update.CmdUpdate], [[Update.CmdUpdate]])
e_hist_updates = hist_updates . e_hist

e_hist :: ResponderTest.Result -> Cmd.History
e_hist = Cmd.state_history . CmdTest.result_cmd_state . ResponderTest.result_cmd

e_hist_collect :: ResponderTest.Result -> Cmd.HistoryCollect
e_hist_collect = Cmd.state_history_collect . CmdTest.result_cmd_state
    . ResponderTest.result_cmd

hist_updates :: Cmd.History
    -> ([[Update.CmdUpdate]], [Update.CmdUpdate], [[Update.CmdUpdate]])
hist_updates (Cmd.History past present future _undo_redo) =
    (map Cmd.hist_updates past, Cmd.hist_updates present,
        map Cmd.hist_updates future)

e_updates :: ResponderTest.Result -> [Update.DisplayUpdate]
e_updates = filter (not . is_view_update) . ResponderTest.result_updates

-- | Filter out \"side effect\" updates that I don't want to test.
is_view_update :: Update.DisplayUpdate -> Bool
is_view_update update = case update of
    Update.Block _ (Update.BlockConfig _) -> True
    _ -> False
