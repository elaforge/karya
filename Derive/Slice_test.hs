module Derive.Slice_test where
import qualified Data.List as List
import qualified Data.Tree as Tree
import Data.Tree (Tree(Node))

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.TrackTree as TrackTree
import qualified Ui.UiTest as UiTest

import qualified Derive.Call.Attribute as Attribute
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Slice as Slice

import qualified Perform.Lilypond.Constants as Constants
import Types


test_extract_orphans = do
    let f events = second extract_tree . unzip . fst
            . Slice.extract_orphans (uncurry make_track events) . make_tree
    equal (f (make_notes 1 "a") [Node (make_notes 1 "b") []]) ([], [])
    equal (f (make_notes 1 "x") [Node (make_notes 0 "abc") []])
        ( [(0, 1), (2, 100)]
        , [ Node (make_notes 0 "a") []
          , Node (make_notes 2 "c") []
          ]
        )
    equal (f (make_notes 1 "x") [Node (make_notes 0 "abc") []])
        ( [(0, 1), (2, 100)]
        , [ Node (make_notes 0 "a") []
          , Node (make_notes 2 "c") []
          ]
        )
    -- zero duration event excludes events that match exactly
    equal (f (">", [(1, 1, "a")]) [Node (make_notes 0 "abc") []])
        ( [(0, 1), (2, 100)]
        , [ Node (make_notes 0 "a") []
          , Node (make_notes 2 "c") []
          ]
        )
    -- orphan control tracks are stripped out
    equal (f (make_notes 1 "a") [Node (make_controls "c" [0..4]) []])
        ([], [])

test_extract_orphans_empty = do
    let f events = snd . Slice.extract_orphans (uncurry make_track events)
            . make_tree
        empty title = Node (first (++title) $ make_notes 0 "")
        notes title start = first (++title) . make_notes start
        controls name = Node . make_controls name
    equal (f (notes "top" 0 "a") [Node (notes "b" 1 "b") []]) []
    equal (f (notes "top" 0 "") [empty "empty" [Node (notes "b" 1 "b") []]])
        [UiTest.tid "empty"]
    equal (f (notes "top" 0 "a")
            [controls "c" [0] [], Node (notes "b" 1 "b") []])
        []

test_event_gaps = do
    let f = Slice.event_gaps
    equal (f 1 []) [(False, 0, 1)]
    equal (f 4 [(1, 2), (2, 3)]) [(False, 0, 1), (False, 3, 4)]
    equal (f 4 [(2, 2)]) [(False, 0, 2), (True, 2, 4)]

test_slice = do
    let f exclusive after s e insert =
            extract_tree . Slice.slice exclusive (1, after) s e insert
            . make_tree
    equal (f False 1 1 2 Nothing [Node (make_notes 2 "ab") []])
        [Node (">", []) []]
    equal (f False 1 1 2 Nothing [Node (make_notes 1 "ab") []])
        [Node (">", [(1, 1, "a")]) []]
    equal (f True 1 1 2 Nothing [Node (make_notes 1 "ab") []])
        [Node (">", []) []]

    -- control tracks get neighbors
    equal (f False 1 1 2 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [0, 2]) []]
    equal (f False 1 2 5 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [2, 4, 6]) []]

    -- more neighbors for control tracks
    equal (f False 2 1 2 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [0, 2, 4]) []]

test_slice_neighbors = do
    let f exclusive around s e =
            extract . Slice.slice exclusive around s e Nothing
            . make_tree
        extract = map $ fmap $ \track ->
            extract_around (TrackTree.tevents_around track)
        extract_around (before, after) = (concatMap Event.event_string before,
            concatMap Event.event_string after)
    let notes offset ns = Node (make_notes offset ns)
        controls cs = Node (make_controls "c" cs)
    equal (f False (1, 1) 1 2 [controls [0..4] [notes 0 "xyz" []]])
        [Node ("0", "34") [Node ("x", "z") []]]
    equal (f False (1, 2) 1 2 [controls [0..4] [notes 0 "xyz" []]])
        [Node ("0", "4") [Node ("x", "z") []]]

test_slice_notes = do
    let f = slice_notes
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

test_slice_notes_sparse = do
    -- Ensure that an intervening empty note track doesn't hide the notes
    -- on the track below it.  This is analogous to orphan extraction in the
    -- top level.
    let f = slice_notes
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

slice_notes :: ScoreTime -> ScoreTime -> EventsTree
    -> [[(ScoreTime, ScoreTime, EventsTree)]]
slice_notes s e = extract_notes extract_tree . Slice.slice_notes s e . make_tree

extract_notes :: (TrackTree.EventsTree -> a)
    -> [[(ScoreTime, ScoreTime, TrackTree.EventsTree)]]
    -> [[(ScoreTime, ScoreTime, a)]]
extract_notes f = map $ map $ \(s, e, t) -> (s, e, f t)

test_slur = do
    let run tracks = DeriveTest.extract extract $
            DeriveTest.derive_tracks_with_ui with
                (DeriveTest.linear_skel tracks) tracks
        extract e =
            ( DeriveTest.e_note e
            , DeriveTest.e_environ ("ly-" `List.isPrefixOf`) e
            , ShowVal.show_val (Score.event_attributes e)
            )
        with = CallTest.with_note_call "(" Attribute.c_ly_slur
    -- Yeah, a slur test should probably go in Attribute_test, but I'm also
    -- testing that the slicing mechanic interacts with calls how I expect it
    -- to.
    let (events, logs) = run $
            [ (">", [(0, 2, "(")])
            , (">", [(0, 1, "+a")])
            ] ++ UiTest.regular_notes 2
    equal events
        [ ((0, 1, "4a"), [(Constants.v_ly_append_first, "'('")], "+a")
        , ((1, 1, "4b"), [(Constants.v_ly_append_last, "')'")], "-")
        ]
    equal logs []

test_overlaps = do
    let run = DeriveTest.extract extract . DeriveTest.linear_derive_tracks id
        extract e = ( DeriveTest.e_note e, DeriveTest.e_attributes e)
        overlapping_log = "slice has overlaps"

    -- +b overlaps with +a, but +b is an orphan.
    --   +a
    -- +b--
    -- a-b-
    let (events, logs) = run $
            [ (">", [(1, 1, "+a")])
            , (">", [(0, 2, "+b")])
            ] ++ UiTest.regular_notes 2
    equal events [((0, 1, "4a"), "+b")]
    strings_like logs [overlapping_log]

    -- +c overlaps with +b.
    -- +a+b
    -- +c--
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 1, "+a"), (1, 1, "+b")])
            , (">", [(0, 2, "+c")])
            ] ++ UiTest.regular_notes 2
    equal events
        [ ((0, 1, "4a"), "+a+c")
        , ((1, 1, "4b"), "+a+c")
        ]
    strings_like logs [overlapping_log]

    -- No overlaps.
    -- +a--
    -- +b+c
    -- a-b-
    let (events, logs) = run $
            [ (">", [(0, 2, "+a")])
            , (">", [(0, 1, "+b"), (1, 1, "+c")])
            ] ++ UiTest.regular_notes 2
    equal events
        [ ((0, 1, "4a"), "+a+b")
        , ((1, 1, "4b"), "+a+c")
        ]
    equal logs []

    -- +d overlaps with +b.
    -- +a--+b--
    -- +c+d----
    -- a-b-c-d-
    let (events, logs) = run $
            [ (">", [(0, 2, "+a"), (2, 2, "+b")])
            , (">", [(0, 1, "+c"), (1, 3, "+d")])
            ] ++ UiTest.regular_notes 4
    equal events
        [ ((0, 1, "4a"), "+a+c")
        , ((1, 1, "4b"), "+a+d")
        , ((2, 1, "4c"), "+a+d")
        , ((3, 1, "4d"), "+a+d")
        ]
    strings_like logs [overlapping_log]

    -- TODO
    -- this also tests that Derive.Call.Note.invert calls strip_empty_tracks
    -- after slice.
    let (events, logs) = run $
            [ (">", [(0, 2, "+a")])
            , (">", [(1, 2, "+b")])
            , (">", [(2, 2, "+c")])
            , (">", [(1, 2, "--0")])
            , ("*", [(2, 0, "4a")])
            ]
    prettyp events
    prettyp logs


-- * util

type Event = (ScoreTime, ScoreTime, String)
type EventsTree = [Tree.Tree (String, [Event])]

extract_tree :: TrackTree.EventsTree -> EventsTree
extract_tree = map $ fmap $ \track ->
    (TrackTree.tevents_title track,
        extract_track (TrackTree.tevents_events track))

extract_track :: Events.Events -> [Event]
extract_track events =
    [(Event.start e, Event.duration e, Event.event_string e)
        | e <- Events.ascending events]

make_tree :: EventsTree -> TrackTree.EventsTree
make_tree = map $ \(Node (title, events) subs) ->
    Node (make_track title events) (make_tree subs)

make_track :: String -> [Event] -> TrackTree.TrackEvents
make_track title events =
    (TrackTree.track_events title tevents 100)
        { TrackTree.tevents_track_id = UiTest.tid <$> clean title }
    where
    clean s
        | null ident = Nothing
        | otherwise = Just ident
        where ident = fst (Id.clean_id True s)
    tevents = Events.from_list
        [Event.event start dur text | (start, dur, text) <- events]

make_controls :: String -> [Int] -> (String, [Event])
make_controls title ps = (title, [(to_score p, 0, show p) | p <- ps])

make_controls2 :: String -> [(Int, String)] -> (String, [Event])
make_controls2 title ps = (title, [(to_score p, 0, val) | (p, val) <- ps])

to_score = ScoreTime.double . fromIntegral

make_notes :: ScoreTime -> String -> (String, [Event])
make_notes offset notes = (">",
    zipWith (\start note -> (start, 1, note : "")) (Seq.range_ offset 1) notes)

make_notes_dur :: [(ScoreTime, ScoreTime, Char)] -> (String, [Event])
make_notes_dur notes = (">", [(start, dur, c:"") | (start, dur, c) <- notes])
