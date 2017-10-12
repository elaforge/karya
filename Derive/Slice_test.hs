-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Slice_test where
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import Data.Tree (Tree(Node))

import qualified Util.Seq as Seq
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


test_slice = do
    let f exclusive s e insert = map (fmap extract_tree)
            . map (Slice.slice exclusive s e insert . make_tree)
    equal (f False 1 2 Nothing [Node (make_notes 2 "ab") []])
        [Node (">", []) []]
    equal (f False 1 2 Nothing [Node (make_notes 1 "ab") []])
        [Node (">", [(1, 1, "a")]) []]
    equal (f True 1 2 Nothing [Node (make_notes 1 "ab") []])
        [Node (">", []) []]

    -- control tracks get neighbors
    equal (f False 1 2 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [0, 2]) []]
    equal (f False 2 5 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [2, 4, 6]) []]

    -- Zero duration slice still slices exact matches. This is so zero duration
    -- note slicing works.
    equal (f False 0 0 Nothing [Node (">", [(0, 0, "a")]) []])
        [Node (">", [(0, 0, "a")]) []]

test_slice_neighbors = do
    let f exclusive s e =
            extract . Slice.slice exclusive s e Nothing . make_tree
        extract = fmap $ \track ->
            extract_around (TrackTree.track_around track)
        extract_around (before, after) =
            (mconcatMap Event.text before, mconcatMap Event.text after)
    let notes offset ns = Node (make_notes offset ns)
        controls cs = Node (make_controls "c" cs)
    equal (f False 1 2 (controls [0..4] [notes 0 "xyz" []]))
        (Node ("0", "34") [Node ("x", "z") []])

test_slice_notes = do
    let f = slice_notes False
    let notes ns = Node (make_notes 0 ns)
        control cs = Node (make_controls "c" cs)
        control2 cs = Node (make_controls2 "c" cs)

    -- no sub tracks works too
    equal (f 0 1 [notes "abc" []])
        [[(0, 1, [notes "a" []])]]

    -- simple sub tracks
    equal (f 0 2 [notes "ab" []])
        [[ (0, 1, [Node (">", [(0, 1, "a")]) []])
        , (1, 1, [Node (">", [(0, 1, "b")]) []])
        ]]
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
    equal (f 0 1 [Node (make_controls "c1" [0..6])
            [Node (make_controls "c2" [0..6])
                [Node (make_notes 0 "a") []]]])
        [[ (0, 1, [Node (make_controls "c1" [0, 1])
            [Node (make_controls "c2" [0, 1])
                [Node (">", [(0, 1, "a")]) []]]])
        ]]

    -- simple child control slicing
    -- also note events have been moved to 0
    equal (f 1 2 [notes "abc" [control [0..6] []]])
        [[(1, 1, [Node (">", [(0, 1, "b")])
            [control2 [(0, "1"), (1, "2")] []]])]]

    -- multiple note tracks
    equal (f 0 0 [notes "abc" [], notes "def" []])
        [ [(0, 1, [notes "a" []])]
        , [(0, 1, [notes "d" []])]
        ]
    -- with different controls
    equal (f 0 0 [notes "ab" [control [0, 1] []],
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

test_slice_notes_zero_dur = do
    let f = slice_notes False
    equal (f 0 2 [Node (">", [(0, 0, "a")]) []])
        [[(0, 0, [Node (">", [(0, 0, "a")]) []])]]
    equal (f 0 2 [Node (">", [(0, 0, "a"), (1, 0, "b")]) []])
        [[ (0, 0, [Node (">", [(0, 0, "a")]) []])
         , (1, 0, [Node (">", [(0, 0, "b")]) []])
         ]]
    equal (f 0 2 [Node (">", [(0, 0, "a")]) [Node (">", [(0, 1, "b")]) []]])
        [[(0, 0, [Node (">", [(0, 0, "a")]) [Node (">", [(0, 1, "b")]) []]])]]

test_slice_notes_shift = do
    -- Verify that shifting and slicing modify track_start, track_end and
    -- track_shifted properly.
    let f s e = extract_notes extract . Slice.slice_notes False s e
        extract t = ((TrackTree.track_start t, TrackTree.track_end t),
            TrackTree.track_shifted t)
    let tree start track_end = Node (track start track_end) []
        track start track_end = (make_track ">" [(start, 1, "a")] 32)
            { TrackTree.track_end = track_end
            , TrackTree.track_shifted = 1
            }
    -- No shift, so the values remain the same.
    equal (f 0 1 [tree 0 1]) [[(0, 1, [Node ((0, 1), 1) []])]]
    -- Shifted by 1.  track_range goes up by one but I'm not sure why.
    -- track_end moves back so it's still at Event.end event + 1.
    equal (f 1 2 [tree 1 2]) [[(1, 1, [Node ((0, 1), 2) []])]]
    equal (f 0 32 [tree 0 32]) [[(0, 1, [Node ((0, 32), 1) []])]]
    -- The end is shorter by 1 because of the shift.
    equal (f 0 32 [tree 1 32]) [[(1, 1, [Node ((0, 31), 2) []])]]

test_slice_include_end = do
    let f = slice_notes
    let notes ns = Node (make_notes 0 ns)
    equal (f False 1 3 [notes "abcde" []])
        [[(1, 1, [notes "b" []]), (2, 1, [notes "c" []])]]
    equal (f True 1 3 [notes "abcde" []])
        [[(1, 1, [notes "b" []]), (2, 1, [notes "c" []]),
            (3, 1, [notes "d" []])]]

test_slice_notes_sparse = do
    -- Ensure that an intervening empty note track doesn't hide the notes
    -- on the track below it.  This is analogous to orphan extraction in the
    -- top level.
    let f = slice_notes False
    let notes offset ns = Node (make_notes offset ns)
        control cs = Node (make_controls "c" cs)
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
    let dur = Node . make_notes_dur
    equal (f 0 2 [notes 0 "ab" [dur [(0, 2, 'c')] [notes 0 "12" []]]])
        [ [ (0, 1, [notes 0 "a" [dur [(0, 2, 'c')] [notes 0 "1" []]]])
          , (1, 1, [notes 0 "b" [empty [notes 0 "2" []]]])
          ]
        ]

slice_notes :: Bool -> ScoreTime -> ScoreTime -> [EventsTree]
    -> [[(ScoreTime, ScoreTime, [EventsTree])]]
slice_notes include_end s e = extract_notes extract_tree
    . Slice.slice_notes include_end s e . map make_tree

extract_notes :: (TrackTree.Track -> a)
    -> [[(ScoreTime, ScoreTime, TrackTree.EventsTree)]]
    -> [[(ScoreTime, ScoreTime, [Tree.Tree a])]]
extract_notes f = map $ map $ \(s, e, t) -> (s, e, map (fmap f) t)

test_slur = do
    let run = DeriveTest.extract extract
            . LilypondTest.derive_tracks_linear
        extract e =
            ( DeriveTest.e_note e
            , LilypondTest.e_ly_env e
            , ShowVal.show_val (Score.event_attributes e)
            )
    -- Yeah, a slur test should probably go in Attribute_test, but I'm also
    -- testing that the slicing mechanic interacts with calls how I expect it
    -- to.
    let (events, logs) = run $
            [ (">", [(0, 2, "(")])
            , (">", [(0, 1, "+a")])
            ] ++ UiTest.regular_notes 2
    let key pos dist = Constants.position_key $
            Constants.CodePosition Constants.Chord pos dist
    equal events
        [ ((0, 1, "3c"), [(key Constants.Append Constants.First, "'('")], "+a")
        , ((1, 1, "3d"), [(key Constants.Append Constants.Last, "')'")], "+")
        ]
    equal logs []

test_strip_empty_tracks = do
    let f = map (fmap extract_tree) . Slice.strip_empty_tracks . make_tree
    let empty = Node (make_notes 0 "")
        notes c = Node (make_notes 0 (c:""))
        controls = Node (make_controls "c" [1])
    equal (f $ notes 'a' [empty []]) [notes 'a' [empty []]]
    equal (f $ empty [empty [controls []]]) []
    -- Empty branch is stripped.
    equal (f $ empty [empty [controls []], notes 'a' []]) [empty [notes 'a' []]]
    -- Don't strip when there are notes below.
    equal (f $ controls [notes 'a' []]) [controls [notes 'a' []]]

test_overlaps = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (DeriveTest.e_pitch e, DeriveTest.e_attributes e)
        overlapping_log = "slice has overlap"

    -- +b overlaps with the orphan spaces after +a.
    -- +a
    -- +b--
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 1, "+a")])
            , (">", [(0, 2, "+b")])
            ] ++ UiTest.regular_notes 2
    equal events [("3c", "+a+b"), ("3d", "+a+b")]
    strings_like logs [overlapping_log]

    -- It's ok if +a duration is 0, though.
    equal (run $ [(">", [(0, 0, "+a")]), (">", [(0, 2, "+b")])]
            ++ UiTest.regular_notes 1)
        ([("3c", "+a+b")], [])

    -- Zero dur with a note afterwards.
    equal (run $ (">", [(0, 0, "+a")]) : UiTest.regular_notes 2)
        ([("3c", "+a"), ("3d", "+")], [])

    -- +a
    --   +b
    -- 3c3d
    equal (run $ [(">", [(0, 0, "+a"), (1, 0, "+b")])]
            ++ UiTest.regular_notes 2)
        ([("3c", "+a"), ("3d", "+b")], [])

    -- But not if there are multiple notes underneath.  Otherwise, the
    -- uncovered gap after +a will cause 3d to be evaluated twice.
    -- +a
    -- +b--
    -- 3c3d
    let (events, logs) = run $
            [ (">", [(0, 0, "+a")])
            , (">", [(0, 2, "+b")])
            ] ++ UiTest.regular_notes 2
    equal events [("3d", "+")]
    strings_like logs ["zero duration slice"]

    -- +b overlaps with +a, but +b is an orphan.
    --   +a
    -- +b--
    -- a-b-
    -- 3c3d
    let (events, logs) = run $
            [ (">", [(1, 1, "+a")])
            , (">", [(0, 2, "+b")])
            ] ++ UiTest.regular_notes 2
    -- ("3c", "+b") doesn't happen because derive_orphans is not isolated from
    -- the event derive when it throws.  Not a big deal.
    equal events []
    strings_like logs [overlapping_log]

    -- +c overlaps with +b.
    -- +a+b
    -- +c--
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 1, "+a"), (1, 1, "+b")])
            , (">", [(0, 2, "+c")])
            ] ++ UiTest.regular_notes 2
    equal events [("3c", "+a+c"), ("3d", "+a+c")]
    strings_like logs [overlapping_log]

    -- No overlaps.
    -- +a--
    -- +b+c
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 2, "+a")])
            , (">", [(0, 1, "+b"), (1, 1, "+c")])
            ] ++ UiTest.regular_notes 2
    equal events [("3c", "+a+b"), ("3d", "+a+c")]
    equal logs []
    -- No overlaps.
    equal (run $ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        ([("4c", "+"), ("4d", "+")], [])

    -- +d overlaps with +b.
    -- +a--+b--
    -- +c+d----
    -- a-b-c-d-
    let (events, logs) = run $
            [ (">", [(0, 2, "+a"), (2, 2, "+b")])
            , (">", [(0, 1, "+c"), (1, 3, "+d")])
            ] ++ UiTest.regular_notes 4
    equal events
        [ ("3c", "+a+c")
        , ("3d", "+a+d")
        , ("3e", "+a+d")
        , ("3f", "+a+d")
        ]
    strings_like logs [overlapping_log]

test_note_transformer_stack = do
    -- The stack should be correct even in the presence of slicing and
    -- inversion.
    let run = DeriveTest.extract (DeriveTest.stack_to_ui . Score.event_stack)
            . DeriveTest.derive_tracks_linear ""
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

extract_tree :: TrackTree.Track -> (Text, [Event])
extract_tree track =
    (TrackTree.track_title track, extract_track (TrackTree.track_events track))

extract_track :: Events.Events -> [Event]
extract_track events =
    [(Event.start e, Event.duration e, Event.text e)
        | e <- Events.ascending events]

make_tree :: EventsTree -> TrackTree.EventsNode
make_tree = fmap $ \(title, events) -> make_track title events 32

make_track :: Text -> [Event] -> TrackTime -> TrackTree.Track
make_track title events end =
    (TrackTree.make_track title tevents end)
        { TrackTree.track_id =
            Just $ UiTest.tid $ Text.filter Id.is_id_char title
        }
    where
    tevents = Events.from_list
        [Event.event start dur text | (start, dur, text) <- events]

make_controls :: Text -> [Int] -> (Text, [Event])
make_controls title ps = (title, [(to_score p, 0, showt p) | p <- ps])

make_controls2 :: Text -> [(Int, Text)] -> (Text, [Event])
make_controls2 title ps = (title, [(to_score p, 0, val) | (p, val) <- ps])

to_score :: Int -> ScoreTime
to_score = ScoreTime.double . fromIntegral

make_notes :: ScoreTime -> [Char] -> (Text, [Event])
make_notes offset notes = (">",
    zipWith (\start note -> (start, 1, Text.singleton note))
        (Seq.range_ offset 1) notes)

make_notes_dur :: [(ScoreTime, ScoreTime, Char)] -> (Text, [Event])
make_notes_dur notes =
    (">", [(start, dur, Text.singleton c) | (start, dur, c) <- notes])
