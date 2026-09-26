-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Slice_test where
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import Data.Tree (Tree(Node))

import qualified Util.Lists as Lists
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.TrackTree as TrackTree
import qualified Ui.UiTest as UiTest

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Slice as Slice

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import Global
import Types


test_slice :: Test
test_slice = do
    let f exclusive s e insert = map (fmap extractTree)
            . map (Slice.slice exclusive s e insert . makeTree)
    equal (f False 1 2 Nothing [Node (makeNotes 2 "ab") []])
        [Node (">", []) []]
    equal (f False 1 2 Nothing [Node (makeNotes 1 "ab") []])
        [Node (">", [(1, 1, "a")]) []]
    equal (f True 1 2 Nothing [Node (makeNotes 1 "ab") []])
        [Node (">", []) []]

    -- control tracks get neighbors
    equal (f False 1 2 Nothing [Node (makeControls "c" [0, 2..10]) []])
        [Node (makeControls "c" [0, 2]) []]
    equal (f False 2 5 Nothing [Node (makeControls "c" [0, 2..10]) []])
        [Node (makeControls "c" [2, 4, 6]) []]

    -- Zero duration slice still slices exact matches. This is so zero duration
    -- note slicing works.
    equal (f False 0 0 Nothing [Node (">", [(0, 0, "a")]) []])
        [Node (">", [(0, 0, "a")]) []]

test_slice_neighbors :: Test
test_slice_neighbors = do
    let f exclusive s e =
            extract . Slice.slice exclusive s e Nothing . makeTree
        extract = fmap $ \track ->
            extractAround (TrackTree.trackAround track)
        extractAround (before, after) =
            (mconcatMap Event.text before, mconcatMap Event.text after)
    let notes offset ns = Node (makeNotes offset ns)
        controls cs = Node (makeControls "c" cs)
    equal (f False 1 2 (controls [0..4] [notes 0 "xyz" []]))
        (Node ("0", "34") [Node ("x", "z") []])

test_slice_notes :: Test
test_slice_notes = do
    let f = sliceNotes False
    let notes ns = Node (makeNotes 0 ns)
        control cs = Node (makeControls "c" cs)
        control2 cs = Node (makeControls2 "c" cs)

    -- no sub tracks works too
    equal (f 0 1 [notes "abc" []])
        [[(0, 1, [notes "a" []])]]

    -- simple sub tracks
    equal (f 0 2 [notes "ab" []])
        [ [ (0, 1, [Node (">", [(0, 1, "a")]) []])
          , (1, 1, [Node (">", [(0, 1, "b")]) []])
        ] ]
    equal (f 0 1 [notes "ab" []])
        [[(0, 1, [Node (">", [(0, 1, "a")]) []])]]

    -- a zero length note grabs events that have the same start
    equal (f 1 1 [notes "abc" []])
        [[(1, 1, [notes "b" []])]]
    equal (f 1 1 [notes "abc" [control [0..6] []]])
        [[(1, 1, [notes "b" [control2 [(0, "1"), (1, "2")] []]])]]

    -- no note tracks, no output
    equal (f 0 1 [control [0..6] []]) []

    -- make sure parent track order doesn't get messed up
    equal (f 0 1
        [Node (makeControls "c1" [0..6])
            [Node (makeControls "c2" [0..6])
                [Node (makeNotes 0 "a") []]]])
        [[ (0, 1,
            [Node (makeControls "c1" [0, 1])
                [Node (makeControls "c2" [0, 1])
                    [Node (">", [(0, 1, "a")]) []]]])
        ]]

    -- simple child control slicing
    -- also note events have been moved to 0
    equal (f 1 2 [notes "abc" [control [0..6] []]])
        [[ (1, 1,
            [Node (">", [(0, 1, "b")])
                [control2 [(0, "1"), (1, "2")] []]])
        ]]

    -- control tracks above and below
    equal (f 0 1 [control [0, 1] [notes "ab" [control [2, 3] []]]])
        [[(0, 1, [control [0, 1] [notes "a" [control [2] []]]])]]

    -- multiple note tracks
    equal (f 0 0 [notes "abc" [], notes "def" []])
        [ [(0, 1, [notes "a" []])]
        , [(0, 1, [notes "d" []])]
        ]
    -- with different controls
    equal (f 0 0
        [notes "ab" [control [0, 1] []],
            notes "cd" [control2 [(0, "2"), (1, "3")] []]])
        [ [(0, 1, [notes "a" [control [0, 1] []]])]
        , [(0, 1, [notes "c" [control2 [(0, "2"), (1, "3")] []]])]
        ]

    -- -- Different slice points when the tree branches.
    -- equal (f 0 4 [notes "a" [Node (">", [(1, 2, "x")]) [],
    --         Node (">", [(2, 2, "y")]) []]])
    --     [ [(0, 1, [notes "a" []])] -- empty tracks stripped
    --     , [(1, 3, [notes "" [Node (">", [(0, 2, "x")]) []]])]
    --     , [(2, 4, [notes "" [Node (">", [(0, 2, "y")]) []]])]
    --     ]

test_slice_notes_track_id :: Test
test_slice_notes_track_id = do
    let f = sliceNotesTrackId False
    let notes inst ns = Node (makeNotesInst inst 0 ns)
        control cs = Node (makeControls "c" cs)
    equal (f 0 1 [notes "i1" "ab" [], notes "i2" "cd" []])
        [ (Just (UiTest.tid "i1"), [(0, 1, [notes "i1" "a" []])])
        , (Just (UiTest.tid "i2"), [(0, 1, [notes "i2" "c" []])])
        ]
    -- control tracks above and below
    equal (f 0 1 [control [0, 1] [notes "i1" "ab" [control [2, 3] []]]])
        [ (Just (UiTest.tid "i1"),
            [ (0, 1, [control [0, 1] [notes "i1" "a" [control [2] []]]])
            ])
        ]

test_slice_notes_zero_dur :: Test
test_slice_notes_zero_dur = do
    let f = sliceNotes False
    equal (f 0 2 [Node (">", [(0, 0, "a")]) []])
        [[(0, 0, [Node (">", [(0, 0, "a")]) []])]]
    equal (f 0 2 [Node (">", [(0, 0, "a"), (1, 0, "b")]) []])
        [[ (0, 0, [Node (">", [(0, 0, "a")]) []])
         , (1, 0, [Node (">", [(0, 0, "b")]) []])
         ]]
    equal (f 0 2 [Node (">", [(0, 0, "a")]) [Node (">", [(0, 1, "b")]) []]])
        [[(0, 0, [Node (">", [(0, 0, "a")]) [Node (">", [(0, 1, "b")]) []]])]]

test_slice_notes_shift :: Test
test_slice_notes_shift = do
    -- Verify that shifting and slicing modify track_start, track_end and
    -- track_shifted properly.
    let f s e = map snd . extractNotes extract . Slice.sliceNotes False s e
        extract t = ((TrackTree.trackStart t, TrackTree.trackEnd t),
            TrackTree.trackShifted t)
    let tree start trackEnd = Node (track start trackEnd) []
        track start trackEnd = (makeTrack ">" [(start, 1, "a")] 32)
            { TrackTree.trackEnd = trackEnd
            , TrackTree.trackShifted = 1
            }
    -- No shift, so the values remain the same.
    equal (f 0 1 [tree 0 1]) [[(0, 1, [Node ((0, 1), 1) []])]]
    -- Shifted by 1.  track_range goes up by one but I'm not sure why.
    -- track_end moves back so it's still at Event.end event + 1.
    equal (f 1 2 [tree 1 2]) [[(1, 1, [Node ((0, 1), 2) []])]]
    equal (f 0 32 [tree 0 32]) [[(0, 1, [Node ((0, 32), 1) []])]]
    -- The end is shorter by 1 because of the shift.
    equal (f 0 32 [tree 1 32]) [[(1, 1, [Node ((0, 31), 2) []])]]

test_slice_include_end :: Test
test_slice_include_end = do
    let f = sliceNotes
    let notes ns = Node (makeNotes 0 ns)
    equal (f False 1 3 [notes "abcde" []])
        [[(1, 1, [notes "b" []]), (2, 1, [notes "c" []])]]
    equal (f True 1 3 [notes "abcde" []])
        [[(1, 1, [notes "b" []]), (2, 1, [notes "c" []]),
            (3, 1, [notes "d" []])]]

test_slice_notes_sparse :: Test
test_slice_notes_sparse = do
    -- Ensure that an intervening empty note track doesn't hide the notes
    -- on the track below it.  This is analogous to orphan extraction in the
    -- top level.
    let f = sliceNotes False
    let notes offset ns = Node (makeNotes offset ns)
        control cs = Node (makeControls "c" cs)
        empty = notes 0 ""

    -- Intervening track is empty.
    equal (f 0 2 [empty [notes 0 "a" []]])
        [[(0, 1, [empty [notes 0 "a" []]])]]
    equal (f 0 2 [empty [empty [notes 0 "a" []]]])
        [[(0, 1, [empty [empty [notes 0 "a" []]]])]]

    -- One note is orphaned.
    --   z-
    -- a-b-
    -- => a (z b)
    equal (f 0 2 [notes 1 "z" [notes 0 "ab" []]])
        [ [ (0, 1, [empty [notes 0 "a" []]])
          , (1, 1, [notes 0 "z" [notes 0 "b" []]])
          ]
        ]

    -- Two levels of orphanage.
    -- a
    --  b
    -- xyz
    equal (f 0 3 [notes 0 "a" [notes 1 "b" [notes 0 "xyz" []]]])
        [ [ (0, 1, [notes 0 "a" [empty [notes 0 "x" []]]])
          , (1, 1, [empty [notes 0 "b" [notes 0 "y" []]]])
          , (2, 1, [empty [empty [notes 0 "z" []]]])
          ]
        ]

    -- Branches, but one is a control.
    equal (f 0 3 [control [0..3] [], notes 0 "a" []])
        [[(0, 1, [notes 0 "a" []])]]

    -- Branches.
    equal (f 0 3 [notes 0 "ab" [], notes 1 "xy" []])
        [ [(0, 1, [notes 0 "a" []]), (1, 1, [notes 0 "b" []])]
        , [(1, 1, [notes 0 "x" []]), (2, 1, [notes 0 "y" []])]
        ]

    -- a-b-
    -- c---
    -- 1-2-
    -- => (a (c 1)) (b 2)
    let dur = Node . makeNotesDur
    equal (f 0 2 [notes 0 "ab" [dur [(0, 2, 'c')] [notes 0 "12" []]]])
        [ [ (0, 1, [notes 0 "a" [dur [(0, 2, 'c')] [notes 0 "1" []]]])
          , (1, 1, [notes 0 "b" [empty [notes 0 "2" []]]])
          ]
        ]

sliceNotes :: Bool -> ScoreTime -> ScoreTime -> [EventsTree]
    -> [[(ScoreTime, ScoreTime, [EventsTree])]]
sliceNotes includeEnd s e = map snd . sliceNotesTrackId includeEnd s e

sliceNotesTrackId :: Bool -> ScoreTime -> ScoreTime -> [EventsTree]
    -> [(Maybe TrackId, [(ScoreTime, ScoreTime, [EventsTree])])]
sliceNotesTrackId includeEnd s e = extractNotes extractTree
    . Slice.sliceNotes includeEnd s e . map makeTree

extractNotes :: (TrackTree.Track -> a)
    -> [Slice.Track]
    -> [(Maybe TrackId, [(ScoreTime, ScoreTime, [Tree a])])]
extractNotes f = map $ \(Slice.Track trackId notes) ->
    (trackId, map (\(s, e, t) -> (s, e, map (fmap f) t)) notes)

test_slur :: Test
test_slur = do
    let run = DeriveTest.extract extract
            . LilypondTest.deriveTracksLinear
        extract e =
            ( DeriveTest.eNote e
            , LilypondTest.eLyEnv e
            , ShowVal.showVal (Score.eventAttributes e)
            )
    -- Yeah, a slur test should probably go in Attribute_test, but I'm also
    -- testing that the slicing mechanic interacts with calls how I expect it
    -- to.
    let (events, logs) = run $
            [ (">", [(0, 2, "(")])
            , (">", [(0, 1, "+a")])
            ] ++ UiTest.regularNotes 2
    let key pos dist = Constants.positionKey $
            Constants.CodePosition Constants.Chord pos dist
    equal events
        [ ((0, 1, "3c"), [(key Constants.Append Constants.First, "'('")], "+a")
        , ((1, 1, "3d"), [(key Constants.Append Constants.Last, "')'")], "+")
        ]
    equal logs []

test_strip_empty_tracks :: Test
test_strip_empty_tracks = do
    let f = map (fmap extractTree) . Slice.stripEmptyTracks . makeTree
    let empty = Node (makeNotes 0 "")
        notes c = Node (makeNotes 0 (c:""))
        controls = Node (makeControls "c" [1])
    equal (f $ notes 'a' [empty []]) [notes 'a' [empty []]]
    equal (f $ empty [empty [controls []]]) []
    -- Empty branch is stripped.
    equal (f $ empty [empty [controls []], notes 'a' []]) [empty [notes 'a' []]]
    -- Don't strip when there are notes below.
    equal (f $ controls [notes 'a' []]) [controls [notes 'a' []]]

test_overlaps :: Test
test_overlaps = do
    let run = DeriveTest.extract extract . DeriveTest.deriveTracksLinear ""
        extract e = (DeriveTest.ePitch e, DeriveTest.eAttributes e)
        overlappingLog = "slice has overlap"

    -- +b overlaps with the orphan spaces after +a.
    -- +a
    -- +b--
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 1, "+a")])
            , (">", [(0, 2, "+b")])
            ] ++ UiTest.regularNotes 2
    equal events [("3c", "+a+b"), ("3d", "+a+b")]
    stringsLike logs [overlappingLog]

    -- It's ok if +a duration is 0, though.
    equal (run $ [(">", [(0, 0, "+a")]), (">", [(0, 2, "+b")])]
            ++ UiTest.regularNotes 1)
        ([("3c", "+a+b")], [])

    -- Zero dur with a note afterwards.
    equal (run $ (">", [(0, 0, "+a")]) : UiTest.regularNotes 2)
        ([("3c", "+a"), ("3d", "+")], [])

    -- +a
    --   +b
    -- 3c3d
    equal (run $ [(">", [(0, 0, "+a"), (1, 0, "+b")])]
            ++ UiTest.regularNotes 2)
        ([("3c", "+a"), ("3d", "+b")], [])

    -- But not if there are multiple notes underneath.  Otherwise, the
    -- uncovered gap after +a will cause 3d to be evaluated twice.
    -- +a
    -- +b--
    -- 3c3d
    let (events, logs) = run $
            [ (">", [(0, 0, "+a")])
            , (">", [(0, 2, "+b")])
            ] ++ UiTest.regularNotes 2
    equal events [("3d", "+")]
    stringsLike logs ["zero duration slice"]

    -- +b overlaps with +a, but +b is an orphan.
    --   +a
    -- +b--
    -- a-b-
    -- 3c3d
    let (events, logs) = run $
            [ (">", [(1, 1, "+a")])
            , (">", [(0, 2, "+b")])
            ] ++ UiTest.regularNotes 2
    -- ("3c", "+b") doesn't happen because derive_orphans is not isolated from
    -- the event derive when it throws.  Not a big deal.
    equal events []
    stringsLike logs [overlappingLog]

    -- +c overlaps with +b.
    -- +a+b
    -- +c--
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 1, "+a"), (1, 1, "+b")])
            , (">", [(0, 2, "+c")])
            ] ++ UiTest.regularNotes 2
    equal events [("3c", "+a+c"), ("3d", "+a+c")]
    stringsLike logs [overlappingLog]

    -- No overlaps.
    -- +a--
    -- +b+c
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 2, "+a")])
            , (">", [(0, 1, "+b"), (1, 1, "+c")])
            ] ++ UiTest.regularNotes 2
    equal events [("3c", "+a+b"), ("3d", "+a+c")]
    equal logs []
    -- No overlaps.
    equal (run $ UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
        ([("4c", "+"), ("4d", "+")], [])

    -- +d overlaps with +b.
    -- +a--+b--
    -- +c+d----
    -- a-b-c-d-
    let (events, logs) = run $
            [ (">", [(0, 2, "+a"), (2, 2, "+b")])
            , (">", [(0, 1, "+c"), (1, 3, "+d")])
            ] ++ UiTest.regularNotes 4
    equal events
        [ ("3c", "+a+c")
        , ("3d", "+a+d")
        , ("3e", "+a+d")
        , ("3f", "+a+d")
        ]
    stringsLike logs [overlappingLog]

test_note_transformer_stack :: Test
test_note_transformer_stack = do
    -- The stack should be correct even in the presence of slicing and
    -- inversion.
    let run = DeriveTest.extract (DeriveTest.stackToUi . Score.eventStack)
            . DeriveTest.deriveTracksLinear ""
    let (stacks, logs) = run
            [ (">", [(1, 1, "ap")])
            , (">", [(1, 1, "")])
            , ("*", [(0, 0, "4c")])
            ]
    equal logs []
    equal stacks [["test/b1 test/b1.t2 1-2"]]

    let (stacks, logs) = run
            [ (">", [(1, 1, "ap")])
            , (">", [(1, 1, "") , (4, 1, "")])
            , ("*", [(1, 0, "4a"), (4, 0, "4b")])
            ]
    equal logs []
    equal stacks [["test/b1 test/b1.t2 1-2"], ["test/b1 test/b1.t2 4-5"]]


-- * util

type EventsTree = Tree.Tree (Text, [Event])
type Event = (ScoreTime, ScoreTime, Text)

extractTree :: TrackTree.Track -> (Text, [Event])
extractTree track =
    (TrackTree.trackTitle track, extractTrack (TrackTree.trackEvents track))

extractTrack :: Events.Events -> [Event]
extractTrack events =
    [(Event.start e, Event.duration e, Event.text e)
        | e <- Events.ascending events]

makeTree :: EventsTree -> TrackTree.EventsNode
makeTree = fmap $ \(title, events) -> makeTrack title events 32

makeTrack :: Text -> [Event] -> TrackTime -> TrackTree.Track
makeTrack title events end =
    (TrackTree.makeTrack title tevents end)
        { TrackTree.trackId =
            Just $ UiTest.tid $ Text.filter Id.isIdChar title
        }
    where
    tevents = Events.fromList
        [Event.event start dur text | (start, dur, text) <- events]

makeControls :: Text -> [Int] -> (Text, [Event])
makeControls title ps = (title, [(toScore p, 0, showt p) | p <- ps])

makeControls2 :: Text -> [(Int, Text)] -> (Text, [Event])
makeControls2 title ps = (title, [(toScore p, 0, val) | (p, val) <- ps])

toScore :: Int -> ScoreTime
toScore = ScoreTime.fromDouble . fromIntegral

makeNotes :: ScoreTime -> [Char] -> (Text, [Event])
makeNotes = makeNotesInst ""

makeNotesInst :: Text -> ScoreTime -> [Char] -> (Text, [Event])
makeNotesInst inst offset notes =
    ( ">" <> inst
    , zipWith (\start note -> (start, 1, Text.singleton note))
        (Lists.range_ offset 1) notes
    )

makeNotesDur :: [(ScoreTime, ScoreTime, Char)] -> (Text, [Event])
makeNotesDur notes =
    (">", [(start, dur, Text.singleton c) | (start, dur, c) <- notes])
