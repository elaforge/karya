-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Clip_test where
import qualified App.Config as Config
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest

import qualified Ui.Id as Id
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


track1 = ("t1", [(0, 2, "e11"), (4, 2, "e12"), (8, 2, "e13")])
track2 = ("t2", [(1, 2, "e21"), (5, 2, "e22")])
clipTracks = [("t1", [(0, 2, "c1"), (4, 2, "c2")])]

-- * copy

test_cmd_copy_selection :: Test
test_cmd_copy_selection = do
    let state = UiTest.exec Ui.empty $
            UiTest.mkviews [(UiTest.defaultBlockName, [track1, track2])]
        run strack spos ctrack cpos =
            eTracks clipId $ CmdTest.runUi state $ do
                CmdTest.setSel strack spos ctrack cpos
                Clip.cmdCopySelection

    equal (run 1 4 1 8) $ Right [("t1", [(0, 2, "e12")])]

    -- I get the same event, but also the empty space before it.
    -- Events are not clipped.
    equal (run 1 1 1 5) $ Right [("t1", [(3, 2, "e12")])]
    equal (run 1 1 2 5) $ Right
        [ ("t1", [(3, 2, "e12")])
        , ("t2", [(0, 2, "e21")])
        ]

    -- Copy zero length selection gets the events underneath.
    equal (run 1 4 1 4) $ Right [("t1", [(0, 2, "e12")])]


-- * paste

eTracks :: BlockId -> CmdTest.Result val -> Either Text [UiTest.TrackSpec]
eTracks blockId = CmdTest.traceLogs . CmdTest.extractState
    (\state _ -> UiTest.extractTracksOf blockId state)

runSel :: Ui.State -> Cmd.CmdId a -> TrackNum -> ScoreTime -> TrackNum
    -> ScoreTime -> Either Text [UiTest.TrackSpec]
runSel state cmd strack spos ctrack cpos = eTracks UiTest.defaultBlockId $
    CmdTest.runUi state $ do
        CmdTest.setSel strack spos ctrack cpos
        cmd

test_paste_past_block_end :: Test
test_paste_past_block_end = do
    -- Events pasted past the end of the block are clipped off.
    let state = mkstate [("t1", [])] clipTracks
        run = runSel state Clip.cmdPasteOverwrite
    equal (run 1 0 1 0) $ Right [("t1", [(0, 2, "c1"), (4, 2, "c2")])]
    let end = UiTest.defaultBlockEnd
    equal (run 1 (end-1) 1 (end-1)) $ Right [("t1", [(end-1, 1, "c1")])]
    equal (run 1 end 1 end) $ Right [("t1", [])]

test_cmd_paste_overwrite :: Test
test_cmd_paste_overwrite = do
    let run state = runSel state Clip.cmdPasteOverwrite
    let f = run (mkstate [track1, track2] clipTracks)

    -- From sel onwards replaced by clipboard.
    equal (f 1 1 1 1) $ Right
        [ ("t1", [(0, 1, "e11"), (1, 2, "c1"), (5, 2, "c2"), (8, 2, "e13")])
        , track2
        ]
    -- Second track isn't overwritten because clip has no second track.
    -- So this is the same as above.
    equal (f 1 1 2 1) $ Right
        [ ("t1", [(0, 1, "e11"), (1, 2, "c1"), (5, 2, "c2"), (8, 2, "e13")])
        , track2
        ]
    -- Only replace the second event, since clipboard is clipped to sel.
    equal (f 1 1 1 4) $ Right
        [ ("t1", [(0, 1, "e11"), (1, 2, "c1"), (4, 2, "e12"), (8, 2, "e13")])
        , track2
        ]
    -- Pasted events are clipped to the selection.
    equal (f 1 2 1 7) $ Right
        [ ("t1", [(0, 2, "e11"), (2, 2, "c1"), (6, 1, "c2"), (8, 2, "e13")])
        , track2
        ]

    -- A point selection will paste a zero dur event.
    let f = run (mkstate [("t1", [])] [("t1", [(0, 0, "e")])])
    equal (f 1 1 1 1) $ Right [("t1", [(1, 0, "e")])]
    let f = run (mkstate [("t1", [])] [("t1", [(0, 0, "a"), (1, 0, "b")])])
    equal (f 1 1 1 1) $ Right [("t1", [(1, 0, "a"), (2, 0, "b")])]

test_cmd_paste_merge :: Test
test_cmd_paste_merge = do
    let state = mkstate [track1, track2] clipTracks
        run = runSel state

    equal (run Clip.cmdPasteMerge 1 1 1 1) $ Right
        [ ("t1", [(0, 1, "e11"), (1, 2, "c1"), (4, 1, "e12"), (5, 2, "c2"),
            (8, 2, "e13")])
        , track2
        ]
    -- Not much more to test here since it's all the same as
    -- cmd_paste_overwrite.

    -- They all overlap so nothing happens.
    equal (run Clip.cmdPasteSoftMerge 1 1 1 1) $ Right
        [track1, track2]

    -- This time they make it in.
    equal (run Clip.cmdPasteSoftMerge 1 2 1 2) $ Right
        [ ("t1", [(0, 2, "e11"), (2, 2, "c1"), (4, 2, "e12"), (6, 2, "c2"),
            (8, 2, "e13")])
        , track2
        ]

test_cmd_paste_insert :: Test
test_cmd_paste_insert = do
    let state = mkstate [track1, track2] clipTracks
        run = runSel state Clip.cmdPasteInsert
    -- Point selection pushes by inserted length.
    equal (run 1 1 1 1) $ Right
        [ ("t1", [(0, 1, "e11"), (1, 2, "c1"), (5, 2, "c2"),
            (10, 2, "e12"), (14, 2, "e13")])
        , track2
        ]
    -- Selection pushes by selection length.
    equal (run 1 1 1 3) $ Right
        [ ("t1", [(0, 1, "e11"), (1, 2, "c1"), (6, 2, "e12"), (10, 2, "e13")])
        , track2
        ]
    -- Events pushed off the end of the block are clipped.
    let end = UiTest.defaultBlockEnd
        state = mkstate [("t1", [(end-2, 2, "t")])] [("c1", [(0, 2, "c")])]
        run = runSel state Clip.cmdPasteInsert
    -- Inserted event shortened because of selection, original event shortened
    -- because of block end.
    equal (run 1 (end-2) 1 (end-1)) $ Right
        [("t1", [(end-2, 1, "c"), (end-1, 1, "t")])]
    equal (run 1 (end-2) 1 (end-2)) $ Right
        [("t1", [(end-2, 2, "c")])]

test_cmd_paste_stretch :: Test
test_cmd_paste_stretch = do
    let run clipTracks = runSel (mkstate [("t1", []), ("t2", [])] clipTracks)
            Clip.cmdPasteStretch

    equal (run [("t1", [(0, 2, "c1"), (2, 2, "c2")])] 1 1 1 2) $ Right
        [("t1", [(1, 0.5, "c1"), (1.5, 0.5, "c2")]), ("t2", [])]
    equal (run [("t1", [(0, 2, "c1"), (2, 2, "c2")])] 1 4 1 8) $ Right
        [("t1", [(4, 2, "c1"), (6, 2, "c2")]), ("t2", [])]
    -- Not confused by pasting an empty track.
    equal (run [("t1", [(0, 2, "c1"), (2, 2, "c2")])] 1 1 2 2) $ Right
        [("t1", [(1, 0.5, "c1"), (1.5, 0.5, "c2")]), ("t2", [])]
    -- When there are multiple tracks stretch them all the same.
    equal (run [("t1", [(0, 2, "c1")]), ("t2", [(2, 2, "c2")])] 1 1 2 2) $
        Right [("t1", [(1, 0.5, "c1")]), ("t2", [(1.5, 0.5, "c2")])]


-- * util

clipId :: BlockId
clipId = Id.BlockId $ Id.id Clip.clipNamespace Config.clipBlockName

mkstate :: [UiTest.TrackSpec] -> [UiTest.TrackSpec] -> Ui.State
mkstate blockTracks clipTracks = UiTest.exec Ui.empty $ do
    UiTest.mkviews [(UiTest.defaultBlockName, blockTracks)]
    Clip.stateToNamespace
        (UiTest.exec Ui.empty
            (UiTest.mkblocks [(Config.clipBlockName, clipTracks)]))
        Clip.clipNamespace
