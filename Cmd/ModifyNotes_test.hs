-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ModifyNotes_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import Global
import Types


test_notes_from_range = do
    let run state tracknums start end = UiTest.eval state $
            notes_from_range tracknums start end
    let state = mkstate
            [ ("tempo", [])
            , (">", [(0, 1, "1")]), ("*", [(0, 0, "4c")])
            , (">", [(1, 1, "2")]), ("*", [(1, 0, "4d")])
            ]
            [ (1, 2), (1, 4)
            , (2, 3), (4, 5)
            ]
    equal (run state [1] 0 2) (Right [])
    equal (run state [2] 0 2) $ Right
        [ (mknote (0, 1, "1", [("*", [(0, "4c")])], 0, [3]), UiTest.mk_tid 2)
        ]
    equal (run state [1..5] 0 2) $ Right
        [ (mknote (0, 1, "1", [("*", [(0, "4c")])], 0, [3]), UiTest.mk_tid 2)
        , (mknote (1, 1, "2", [("*", [(1, "4d")])], 1, [5]), UiTest.mk_tid 4)
        ]
    equal (run state [1..5] 0 0) $ Right
        [ (mknote (0, 1, "1", [("*", [(0, "4c")])], 0, [3]), UiTest.mk_tid 2)
        ]

    let complicated = mkstate
            [(">", [(0, 1, "")]), ("*", []), ("*", [])] [(1, 2), (1, 3)]
    left_like (run complicated [1] 0 2) ">1 subtrack"
    let fancy = mkstate [(">", [(0, 1, "")]), ("add c", [])] [(1, 2)]
    left_like (run fancy [1] 0 2) "complicated controls unsupported"

test_notes_from_range_negative = do
    let run tracks start end = fmap (fmap (fmap fst)) $ CmdTest.result_val $
            CmdTest.run_tracks tracks $
            State.require_right id =<< notes_from_range [1..4] start end
        negative n = n { ModifyNotes.note_orientation = Event.Negative }
    let tracks =
            [ (">", [(2, -2, "1")]), ("*", [(2, 0, "4c")])
            , (">", [(4, -2, "2")]), ("*", [(4, 0, "4d")])
            ]
    equal (run tracks 0 4) $ Right $ Just $ map (negative . mknote)
        [ (0, 2, "1", [("*", [(2, "4c")])], 0, [2])
        , (2, 2, "2", [("*", [(4, "4d")])], 1, [4])
        ]
    let tracks = [(">", [(2, -2, "1"), (2, 2, "2")]), ("*", [(2, 0, "4c")])]
    equal (run tracks 0 4) $ Right $ Just
        [ negative $ mknote (0, 2, "1", [("*", [(2, "4c")])], 0, [2])
        , mknote (2, 2, "2", [("*", [(2, "4c")])], 0, [2])
        ]

notes_from_range :: State.M m => [TrackNum] -> TrackTime -> TrackTime
    -> m (Either Text [(ModifyNotes.Note, TrackId)])
notes_from_range tracknums start end = do
    note_trees <- ModifyNotes.extract_note_trees
        UiTest.default_block_id (map UiTest.mk_tid tracknums)
    ModifyNotes.notes_from_range note_trees start end

test_merge_notes = do
    let f = extract . head . ModifyNotes.merge_notes . mknotes
        mknotes notes =
            [ mknote (start, 1, showt n, controls, 0, [])
            | (n, (start, controls)) <- zip [0..] notes
            ]
        extract (ModifyNotes.NoteTrack events controls) =
            (extract_events events, controls)
    equal (f [(0, []), (1, [])])
        ([(0, "0"), (1, "1")], mkcontrols [])
    equal (f [(0, [("*", [(0, "4c")])]), (1, [("c", [(1, "1")])])])
        ([(0, "0"), (1, "1")],
            mkcontrols [("*", [(0, "4c")]), ("c", [(1, "1")])])

test_write_tracks = do
    let f state tracknums = write_tracks state tracknums . map mk
        mk (events, controls) =
            ModifyNotes.NoteTrack (mkevents events) (mkcontrols controls)
        empty = f (snd $ UiTest.run_mkblock []) []
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
    equal (f (snd $ UiTest.run_mkblock [("tempo", []), (">", [])]) [2]
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
    let linear tracks = snd $ UiTest.run_mkblock $
            (">", [(0, 1, "a")]) : tracks
    equal (f (linear [("*", [(4, 0, "5c")]), ("d", [(4, 0, "4")])]) [1..3]
            [([(0, "x")], [pitch 0, control "c" 1])])
        ( [ (">", [(0, 0, "x")])
          , ("*", [(0, 0, "4c"), (4, 0, "5c")])
          , ("d", [(4, 0, "4")]), ("c", [(1, 0, "1")])
          ]
        , [(1, 2), (2, 3), (3, 4)]
        )

write_tracks :: State.State -> [TrackNum] -> [ModifyNotes.NoteTrack]
    -> ([UiTest.TrackSpec], [Skeleton.Edge])
write_tracks state tracknums tracks = extract $ UiTest.exec state $
    ModifyNotes.write_tracks UiTest.default_block_id
        (map UiTest.mk_tid tracknums) tracks
    where
    extract state =
        (UiTest.extract_tracks state, UiTest.extract_skeleton state)

extract_events :: Events.Events -> [(TrackTime, Text)]
extract_events = map event . Events.ascending
    where event e = (Event.start e, Event.text e)

mkevents :: [(TrackTime, Text)] -> Events.Events
mkevents = Events.from_list . map mkevent
    where mkevent (start, text) = Event.event start 0 text

mknote :: (TrackTime, TrackTime, Text, [(Text, [(TrackTime, Text)])],
    ModifyNotes.Index, [TrackNum]) -> ModifyNotes.Note
mknote (start, dur, text, controls, index, control_track_ids) = ModifyNotes.Note
    { note_start = start
    , note_duration = dur
    , note_orientation = Event.Positive
    , note_text = text
    , note_controls = mkcontrols controls
    , note_index = index
    , note_control_track_ids = map UiTest.mk_tid control_track_ids
    }

mkcontrols :: [(Text, [(TrackTime, Text)])] -> ModifyNotes.Controls
mkcontrols = Map.fromList . map mk
    where
    mk (name, events) = (control, mkevents events)
        where
        control = case Text.uncons name of
            Just ('*', s) -> ModifyNotes.Pitch (Pitch.ScaleId s)
            _ -> ModifyNotes.Control (Score.unchecked_control name)

mkstate :: [UiTest.TrackSpec] -> [Skeleton.Edge] -> State.State
mkstate tracks skel = UiTest.exec State.empty $ UiTest.mkblocks_skel
    [((UiTest.default_block_name, tracks) , skel)]
