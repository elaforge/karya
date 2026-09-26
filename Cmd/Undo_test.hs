-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Undo_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Directory as Directory

import qualified Util.Git as Git
import qualified Util.Rect as Rect
import qualified Util.Test.Testing as Testing

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Selection as Selection
import qualified Cmd.Undo as Undo

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import Global
import Types
import Util.Test


test_undo :: Test
test_undo = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]

    res <- ResponderTest.respondCmd states $ Cmd.name "+z" $ insertEvent 0 "z"
    res <- next res $ Cmd.name "+q" $ insertEvent 1 "q"

    equal (extractUi res) "zq"
    equal (eUpdates res) [trackUpdate 1 1 2]

    res <- next res Undo.undo
    -- Make sure the past and future have the expected names and states.
    equal (eHistNames res) (["setup: 12"], "+z: z2", ["+q: zq"])
    equal (extractUi res) "z2"
    equal (eUpdates res) [trackUpdate 1 1 2]

    res <- next res Undo.undo
    equal (eHistNames res) ([], "setup: 12", ["+z: z2", "+q: zq"])
    equal (extractUi res) "12"
    equal (eUpdates res) [trackUpdate 1 0 1]

    -- no past to undo
    res <- next res Undo.undo
    equal (extractUi res) "12"
    equal (eUpdates res) []

    res <- next res Undo.redo
    equal (eHistNames res) (["setup: 12"], "+z: z2", ["+q: zq"])
    equal (extractUi res) "z2"
    equal (eUpdates res) [trackUpdate 1 0 1]

    res <- next res Undo.redo
    equal (extractUi res) "zq"
    equal (eHistNames res) (["+z: z2", "setup: 12"], "+q: zq", [])
    equal (eUpdates res) [trackUpdate 1 1 2]

    -- no future to redo
    res <- next res Undo.redo
    equal (extractUi res) "zq"
    equal (eUpdates res) []

test_suppress_history :: Test
test_suppress_history = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]
    let suppress = Cmd.suppressHistory Cmd.ValEdit

    res <- ResponderTest.respondCmd states $
        Cmd.name "toggle" Edit.cmdToggleValEdit
    res <- next res $ suppress "+z" $ insertEvent 0 "z"
    equal (eHistNames res) ([], "setup: 12", [])
    res <- next res $ suppress "+q" $ insertEvent 1 "q"
    equal (eHistNames res) ([], "setup: 12", [])
    -- A non-recording cmd will cause the suppressed cmds to be recorded.
    res <- next res $ setSel 1
    res <- next res $ Cmd.name "toggle" Edit.cmdToggleValEdit
    equal (eHistNames res) (["setup: 12"], "+z: zq", [])

test_undo_while_suppressed :: Test
test_undo_while_suppressed = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [(0, 1, "1"), (1, 1, "2")])]
    let suppress = Cmd.suppressHistory Cmd.ValEdit

    -- Unsuppressed.
    res <- ResponderTest.respondCmd states $ Cmd.name "+a" $ insertEvent 0 "a"

    -- Suppressed.
    res <- next res $ Cmd.name "toggle" Edit.cmdToggleValEdit
    res <- next res $ suppress "+b" $ insertEvent 0 "b"
    -- The 'b' cmd is buried in suppression.
    equal (eHistNames res) (["setup: 12"], "+a: a2", [])
    equal (extractUi res) "b2"
    res <- next res Undo.undo
    -- Undo turns the suppressed cmd turns into a real cmd, and goes into the
    -- future.
    equal (eHistNames res) (["setup: 12"], "+a: a2", ["+b: b2"])
    equal (extractUi res) "a2"

    -- And I can go back to it.
    res <- next res Undo.redo
    equal (eHistNames res) (["+a: a2", "setup: 12"], "+b: b2", [])
    equal (extractUi res) "b2"

test_undo_merge :: Test
test_undo_merge = Git.initialize $ do
    let states = ResponderTest.mkstates [(">", [])]
        vid = UiTest.defaultViewId
    res1 <- ResponderTest.respondCmd states $ do
        Ui.setNamespace (Id.namespace "oogabooga")
        Ui.setViewRect vid $ Rect.xywh 40 40 100 100
        insertEvent 0 "z"
    res2 <- ResponderTest.respondCmd (ResponderTest.resultStates res1)
        Undo.undo
    -- some things aren't affected by undo
    equal (UiTest.eval (eUi res1) (Block.viewRect <$> Ui.getView vid))
        (Rect.xywh 40 40 100 100)
    equal (UiTest.eval (eUi res2) (Block.viewRect <$> Ui.getView vid))
        (Rect.xywh 40 40 100 100)

trackUpdate :: TrackNum -> ScoreTime -> ScoreTime -> Update.DisplayUpdate
trackUpdate tracknum from to =
    Update.Track (UiTest.mkTid tracknum) (Update.TrackEvents from to)

test_load_previous_history :: Test
test_load_previous_history = Git.initialize $ do
    repo <- getRepo
    -- Load a git repo and make sure its history comes with it.
    res <- saveGit repo $ ResponderTest.mkstates [(">", [(0, 1, "1")])]
    res <- next res $ Cmd.name "+x" $ insertEvent 0 "x"
    res <- next res $ Cmd.name "+y" $ insertEvent 1 "y"
    -- pprint (e_commits res)
    -- pprint (e_hist_damage res)

    equal (extractUi res) "xy"
    res <- ResponderTest.respondCmd (ResponderTest.mkstates []) $
        Save.loadGit repo Nothing
    equal (extractUi res) "xy"

    res <- next res Undo.undo
    -- Previous history was loaded, y deleted.
    equal (eHistNames res) ([], "+x: x", ["+y: xy"])
    equal (extractUi res) "x"
    equal (eUpdates res) [trackUpdate 1 1 2]

    res <- next res Undo.undo
    -- Previous history was loaded, x replaced with 1.
    equal (eHistNames res) ([], "save: 1", ["+x: x", "+y: xy"])
    equal (extractUi res) "1"
    equal (eUpdates res) [trackUpdate 1 0 1]

    -- out of past
    res <- next res Undo.undo
    equal (extractUi res) "1"
    equal (eUpdates res) []

    res <- next res Undo.redo
    equal (extractUi res) "x"
    equal (eUpdates res) [trackUpdate 1 0 1]
    res <- next res Undo.redo
    equal (extractUi res) "xy"
    equal (eUpdates res) [trackUpdate 1 1 2]

test_load_next_history :: Test
test_load_next_history = Git.initialize $ do
    repo <- getRepo
    res <- saveGit repo $ ResponderTest.mkstates [(">", [(0, 1, "1")])]
    res <- next res $ Cmd.name "+x" $ insertEvent 0 "x"
    res <- next res $ Cmd.name "+y" $ insertEvent 1 "y"
    pprint (eCommits res)
    -- The creation state wasn't committed, and the save isn't recorded in the
    -- history even though it has a commit, so start at the second-to-last.
    let ([ent, _], _, _) = eCommits res
        (_, Just commit) = ent
    res <- ResponderTest.respondCmd (ResponderTest.mkstates []) $
        Save.loadGit repo (Just commit)
    equal (eHistNames res) ([], "+x: x", [])
    equal (extractUi res) "x"

    res <- next res Undo.redo
    equal (extractUi res) "xy"
    equal (eHistNames res) (["+x: x"], "+y: xy", [])
    equal (eUpdates res) [trackUpdate 1 1 2]

    -- No future to redo.
    res <- next res Undo.redo
    equal (extractUi res) "xy"
    equal (eUpdates res) []

    -- Make sure I can do back again.
    res <- next res Undo.undo
    equal (extractUi res) "x"
    equal (eUpdates res) [trackUpdate 1 1 2]

    res <- next res Undo.undo
    equal (extractUi res) "1"
    equal (eUpdates res) [trackUpdate 1 0 1]

test_branching_history :: Test
test_branching_history = Git.initialize $ do
    repo <- getRepo
    res <- saveGit repo $ ResponderTest.mkstates [(">", [(0, 1, "1")])]
    res <- next res $ Cmd.name "+x" $ insertEvent 0 "x"
    res <- next res $ Cmd.name "+y" $ insertEvent 1 "y"
    res <- next res $ Cmd.name "save" Save.saveGit
    res <- next res $ Cmd.name "revert" $ Save.revert (Just "0")
    equal (extractUi res) "1"
    res <- next res $ Cmd.name "+a" $ insertEvent 0 "a"
    res <- next res $ Cmd.name "+b" $ insertEvent 1 "b"
    res <- next res $ Cmd.name "save" Save.saveGit

    -- The second branch got 1.0 because 1 was taken.
    refs <- Git.readRefMap repo
    equal (Map.keys refs) ["heads/master", "tags/0", "tags/1", "tags/1.0"]

    -- Each branch has its own history.
    ioEqual (readLog repo =<< Git.readLog repo "tags/1.0")
        ["+b", "+a", "save"]
    ioEqual (readLog repo =<< Git.readLogHead repo)
        ["+b", "+a", "save"]
    ioEqual (readLog repo =<< Git.readLog repo "tags/1")
        ["+y", "+x", "save"]

    equal (extractUi res) "ab"
    res <- next res $ Cmd.name "revert" $ Save.revert (Just "1")
    equal (extractUi res) "xy"

readLog :: SaveGit.Repo -> [SaveGit.Commit] -> IO [Text]
readLog repo commits = do
    texts <- mapM (fmap Git.commitText . Git.readCommit repo) commits
    mapM (fmap head . SaveGit.parseNames) texts

saveGit :: SaveGit.Repo -> ResponderTest.States -> IO ResponderTest.Result
saveGit repo states = do
    whenM (Directory.doesDirectoryExist repo) $
        Directory.removeDirectoryRecursive repo
    ResponderTest.respondCmd states (Save.saveGitAs repo)

getRepo :: IO SaveGit.Repo
getRepo = (++ SaveGit.gitSuffix) <$> Testing.tmpDir "git"

-- * implementation

next :: ResponderTest.Result -> Cmd.CmdT IO a -> IO ResponderTest.Result
next = ResponderTest.respondCmd . ResponderTest.resultStates

insertEvent :: Ui.M m => ScoreTime -> Text -> m ()
insertEvent pos text = UiTest.insertEvent 1 (pos, 1, text)

setSel :: Cmd.M m => ScoreTime -> m ()
setSel pos = Cmd.name "select" $
    Selection.setCurrent Config.insertSelnum $
        Just (Sel.point 1 pos Sel.Positive)


-- ** extract

eHistNames :: ResponderTest.Result -> ([Text], Text, [Text])
eHistNames = extractHist $ \(Cmd.HistoryEntry state _ names _) ->
    Text.intercalate "+" names <> ": " <> txt (uiNotes 0 state)

extractHist :: (Cmd.HistoryEntry -> a) -> ResponderTest.Result -> ([a], a, [a])
extractHist extract res =
    (map extract past, extract present, map extract future)
    where Cmd.History past present future _ = eHist res

type Commit = ([Text], Maybe SaveGit.Commit)

eCommits :: ResponderTest.Result -> ([Commit], Commit, [Commit])
eCommits res = (map extract past, extract present, map extract future)
    where
    Cmd.History past present future _ = eHist res
    extract hist = (Cmd.histNames hist, Cmd.histCommit hist)

extractUi :: ResponderTest.Result -> [Char]
extractUi = uiNotes 0 . eUi

uiNotes :: Int -> Ui.State -> [Char]
uiNotes tracknum uiState
    | not (">" `Text.isPrefixOf` title) =
        error $ "not a note track: " <> show title
    | otherwise = [Text.head text | (_, _, text) <- tracks]
    where (title, tracks) = UiTest.extractTracks uiState !! tracknum

eUi :: ResponderTest.Result -> Ui.State
eUi = CmdTest.resultUiState . ResponderTest.resultCmd

eHistDamage :: ResponderTest.Result
    -> ([Update.UiDamage], Update.UiDamage, [Update.UiDamage])
eHistDamage = histDamage . eHist

eHist :: ResponderTest.Result -> Cmd.History
eHist = Cmd.stateHistory . CmdTest.resultCmdState . ResponderTest.resultCmd

eHistCollect :: ResponderTest.Result -> Cmd.HistoryCollect
eHistCollect = Cmd.stateHistoryCollect . CmdTest.resultCmdState
    . ResponderTest.resultCmd

histDamage :: Cmd.History
    -> ([Update.UiDamage], Update.UiDamage, [Update.UiDamage])
histDamage (Cmd.History past present future _undoRedo) =
    ( map Cmd.histDamage past
    , Cmd.histDamage present
    , map Cmd.histDamage future
    )

eUpdates :: ResponderTest.Result -> [Update.DisplayUpdate]
eUpdates = filter (not . isViewUpdate) . ResponderTest.resultUpdates

-- | Filter out \"side effect\" updates that I don't want to test.
isViewUpdate :: Update.DisplayUpdate -> Bool
isViewUpdate update = case update of
    Update.Block _ (Update.BlockConfig _) -> True
    _ -> False
