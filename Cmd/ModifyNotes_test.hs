-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ModifyNotes_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Test.Testing as Testing
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_selected_notes :: Test
test_selected_notes = do
    let run state start end = CmdTest.resultVal $
            CmdTest.run state CmdTest.defaultCmdState $ do
                CmdTest.setSel 1 start 8 end
                ModifyNotes.selectedNotes
    let state = mkstate
            [ ("tempo", [])
            , (">", [(0, 1, "1")]), ("*", [(0, 0, "4c")])
            , (">", [(1, 1, "2")]), ("*", [(1, 0, "4d")])
            ]
            [ (1, 2), (1, 4)
            , (2, 3), (4, 5)
            ]
    equal (run state 0.5 0.5) $ Right $ Just
        [ (mknote (0, 1, "1", [("*", [(0, "4c")])], 0, [3]), UiTest.mkTid 2)
        ]
    equal (run state 0 2) $ Right $ Just
        [ (mknote (0, 1, "1", [("*", [(0, "4c")])], 0, [3]), UiTest.mkTid 2)
        , (mknote (1, 1, "2", [("*", [(1, "4d")])], 1, [5]), UiTest.mkTid 4)
        ]

    let complicated = mkstate
            [(">", [(0, 1, "")]), ("*", []), ("*", [])] [(1, 2), (1, 3)]
    leftLike (run complicated 0 2) ">1 subtrack"
    let fancy = mkstate [(">", [(0, 1, "")]), ("add c", [])] [(1, 2)]
    leftLike (run fancy 0 2) "complicated controls unsupported"

test_selected_remove :: Test
test_selected_remove = do
    let run tracks start end = CmdTest.eTracks $
            CmdTest.runTracks tracks $ do
                CmdTest.setSel 1 start 2 end
                ModifyNotes.selection (ModifyNotes.notes (const []))
    let mkpitch negative ts = ("*",
            [ (ScoreTime.fromDouble t, if negative then -0 else 0, "")
            | t <- ts
            ])
    equalT (run (UiTest.noteTrack [(0, 2, "a"), (2, 2, "b")]) 0 0) $
        Right ([(">", [(2, 2, "")]), ("*", [(2, 0, "b")])], [])
    equalT (run ((">", [(0, 2, "1"), (2, 2, "2")]) : [mkpitch False [0..4]])
            0 0) $
        Right ([(">", [(2, 2, "2")]), mkpitch False [2..4]], [])
    -- The pitch at 0 is still there because "1" was negative.
    equalT (run ((">", [(2, -2, "1"), (4, -2, "2")]) : [mkpitch True [0..4]])
            0 0) $
        Right ([(">", [(4, -2, "2")]), mkpitch True [0, 3, 4]], [])

test_merge_notes :: Test
test_merge_notes = do
    let f = extract . head . ModifyNotes.mergeNotes . mknotes
        mknotes notes =
            [ mknote (start, 1, showt n, controls, 0, [])
            | (n, (start, controls)) <- zip [0..] notes
            ]
        extract (ModifyNotes.NoteTrack events controls) =
            (extractEvents events, controls)
    equal (f [(0, []), (1, [])])
        ([(0, "0"), (1, "1")], mkcontrols [])
    equal (f [(0, [("*", [(0, "4c")])]), (1, [("c", [(1, "1")])])])
        ([(0, "0"), (1, "1")],
            mkcontrols [("*", [(0, "4c")]), ("c", [(1, "1")])])

test_write_tracks :: Test
test_write_tracks = do
    let f state tracknums = writeTracks state tracknums . map mk
        mk (events, controls) =
            ModifyNotes.NoteTrack (mkevents events) (mkcontrols controls)
        empty = f (snd $ UiTest.runMkblock []) []
        pitch pos = ("*", [(pos, "4c")])
        control name pos = (name, [(pos, "1")])
    equal (empty []) ([], [])
    equal (empty [([], [])]) ([], [])
    equal (empty [([(0, "a"), (1, "b")], [pitch 0, control "c" 1])])
        ( [ (">", [(0, 0, "a"), (1, 0, "b")])
          , ("*", [(0, 0, "4c")])
          , ("c", [(1, 0, "1")])
          ]
        , [(1, 2), (2, 3)]
        )
    -- Add multiple tracks.
    equal (empty [([(0, "a")], [pitch 0]), ([(1, "b")], []), ([(2, "c")], [])])
        ( [ (">", [(0, 0, "a")])
          , ("*", [(0, 0, "4c")])
          , (">", [(1, 0, "b")])
          , (">", [(2, 0, "c")])
          ]
        , [(1, 2)]
        )

    -- New tracks get the same parent as the old ones.
    equal (f (snd $ UiTest.runMkblock [("tempo", []), (">", [])]) [2]
            [([(0, "a")], []), ([(0, "b")], [])])
        ( [ ("tempo", [])
          , (">", [(0, 0, "a")])
          , (">", [(0, 0, "b")])
          ]
        , [(1, 2), (1, 3)]
        )

    -- Merge with existing tracks.
    let state = mkstate
            [ tempo
            , (">", []), ("*", [])
            , (">", []), ("c", [])
            ]
            [ (1, 2), (1, 4)
            , (2, 3), (4, 5)
            ]
        tempo = ("tempo", [(0, 0, "1")])
    equal (f state [2..5] [])
        ( [ tempo
          , (">", []), ("*", [])
          , (">", []), ("c", [])
          ]
        , [(1, 2), (1, 4), (2, 3), (4, 5)]
        )

    equal (f state [2..5] [([(0, "x")], [pitch 0, control "c" 1])])
        ( [ tempo
          , (">", [(0, 0, "x")])
          , ("*", [(0, 0, "4c")]), ("c", [(1, 0, "1")])
          , (">", []), ("c", [])
          ]
        , [ (1, 2), (1, 5)
          , (2, 3), (3, 4), (5, 6)
          ]
        )
    let linear tracks = snd $ UiTest.runMkblock $
            (">", [(0, 1, "a")]) : tracks
    equal (f (linear [("*", [(4, 0, "5c")]), ("d", [(4, 0, "4")])]) [1..3]
            [([(0, "x")], [pitch 0, control "c" 1])])
        ( [ (">", [(0, 0, "x")])
          , ("*", [(0, 0, "4c"), (4, 0, "5c")])
          , ("d", [(4, 0, "4")]), ("c", [(1, 0, "1")])
          ]
        , [(1, 2), (2, 3), (3, 4)]
        )

writeTracks :: Ui.State -> [TrackNum] -> [ModifyNotes.NoteTrack]
    -> ([UiTest.TrackSpec], [Skeleton.Edge])
writeTracks state tracknums tracks = extract $ UiTest.exec state $
    ModifyNotes.writeTracks UiTest.defaultBlockId
        (map UiTest.mkTid tracknums) tracks
    where
    extract state =
        (UiTest.extractTracks state, UiTest.extractSkeleton state)

-- * util

equalT :: (Eq logs, Show logs) => Either Text ([UiTest.TrackSpec], logs)
    -> Either Text ([UiTest.TrackSpec], logs) -> Test
equalT = Testing.equalFmt (UiTest.rightFst UiTest.fmtTracks)

extractEvents :: Events.Events -> [(TrackTime, Text)]
extractEvents = map event . Events.ascending
    where event e = (Event.start e, Event.text e)

mkevents :: [(TrackTime, Text)] -> Events.Events
mkevents = Events.fromList . map mkevent
    where mkevent (start, text) = Event.event start 0 text

mknote :: (TrackTime, TrackTime, Text, [(Text, [(TrackTime, Text)])],
    ModifyNotes.Index, [TrackNum]) -> ModifyNotes.Note
mknote (start, dur, text, controls, index, controlTrackIds) = ModifyNotes.Note
    { noteStart = start
    , noteDuration = dur
    , noteText = text
    , noteControls = mkcontrols controls
    , noteIndex = index
    , noteControlTrackIds = map UiTest.mkTid controlTrackIds
    }

mkcontrols :: [(Text, [(TrackTime, Text)])] -> ModifyNotes.Controls
mkcontrols = Map.fromList . map mk
    where
    mk (name, events) = (control, mkevents events)
        where
        control = case Text.uncons name of
            Just ('*', s) -> ModifyNotes.Pitch (Pitch.ScaleId s)
            _ -> ModifyNotes.Control (ScoreT.Control name)

mkstate :: [UiTest.TrackSpec] -> [Skeleton.Edge] -> Ui.State
mkstate tracks skel = UiTest.exec Ui.empty $ do
    UiTest.mkblocksSkel [((UiTest.defaultBlockName, tracks), skel)]
    UiTest.mkview UiTest.defaultBlockId
    pure ()
