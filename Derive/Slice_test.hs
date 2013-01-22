module Derive.Slice_test where
import qualified Data.Tree as Tree
import Data.Tree (Tree(Node))

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Slice as Slice
import Types


test_extract_orphans = do
    let f events subs = extract_tree $
            Slice.extract_orphans False Nothing (uncurry make_track events)
                (make_tree subs)
    equal (f (make_notes 1 "a") [Node (make_notes 1 "b") []]) []
    equal (f (make_notes 1 "x") [Node (make_notes 0 "abc") []])
        [ Node (make_notes 0 "a") []
        , Node (make_notes 2 "c") []
        ]
    -- zero duration event excludes events that match exactly
    equal (f (">", [(1, 1, "a")]) [Node (make_notes 0 "abc") []])
        [ Node (make_notes 0 "a") []
        , Node (make_notes 2 "c") []
        ]

    -- orphan control tracks are stripped out
    equal (f (make_notes 1 "a") [Node (make_controls "c" [0..4]) []]) []

test_event_gaps = do
    let f es end = Slice.event_gaps (mkevents es) end
        mkevents ranges = [Event.event s (e-s) "" | (e, s) <- ranges]
    equal (f [] 1) [(False, 0, 1)]
    equal (f [(1, 2), (2, 3)] 4) [(False, 0, 1), (False, 3, 4)]
    equal (f [(2, 2)] 4) [(False, 0, 2), (True, 2, 4)]

test_slice = do
    let f exclusive after s e insert =
            extract_tree . Slice.slice exclusive (1, after) s e insert
            . make_tree
    equal (f False 1 1 2 Nothing [Node (make_notes 2 "ab") []]) []
    equal (f False 1 1 2 Nothing [Node (make_notes 1 "ab") []])
        [Node (">", [(1, 1, "a")]) []]
    equal (f True 1 1 2 Nothing [Node (make_notes 1 "ab") []]) []

    -- control tracks get neighbors
    equal (f False 1 1 2 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [0, 2]) []]
    equal (f False 1 2 5 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [2, 4, 6]) []]

    -- more neighbors for control tracks
    equal (f False 2 1 2 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [0, 2, 4]) []]

test_slice_notes = do
    let f s e = extract . Slice.slice_notes s e . make_tree
        extract = map (map (\(s, e, t) -> (s, e, extract_tree t)))
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

    -- empty note track is ignored
    equal (f 0 1 [notes "abc" [notes "" [control [0] []]]])
        [[(0, 1, [notes "a" [control [0] []]])]]

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

test_slice_notes_orphans = do
    -- Ensure that an intervening empty note track doesn't hide the notes
    -- on the track below it.  This is analogous to orphan extraction in the
    -- top level.
    let f s e = extract . Slice.slice_notes s e . make_tree
        extract = map (map (\(s, e, t) -> (s, e, extract_tree t)))
    let notes offset ns = Node (make_notes offset ns)

    -- Intervening track is empty.
    equal (f 0 2 [notes 0 "" [notes 0 "a" []]])
        [[(0, 1, [notes 0 "a" []])]]
    equal (f 0 2 [notes 0 "" [notes 0 "" [notes 0 "a" []]]])
        [[(0, 1, [notes 0 "a" []])]]

    -- One note is orphaned.
    equal (f 0 2 [notes 1 "z" [notes 0 "ab" []]])
        [ [(1, 1, [notes 0 "z" [notes 0 "b" []]])]
        , [(0, 1, [notes 0 "a" []])]
        ]

    -- Two levels of orphanage.
    equal (f 0 3 [notes 0 "a" [notes 1 "b" [notes 2 "c" []]]])
        [ [(0, 1, [notes 0 "a" []])]
        , [(1, 1, [notes 0 "b" []])]
        , [(2, 1, [notes 0 "c" []])]
        ]
    -- The 'b' doesn't lie in the range, so it's omitted.  BlockUtil's extract
    -- orphans should get that one.
    equal (f 0 1 [notes 0 "a" [notes 1 "b" []]])
        [[(0, 1, [notes 0 "a" []])]]

    equal (f 0 2 [notes 0 "" [notes 0 "a" [], notes 0 "b" []]])
        [ [(0, 1, [notes 0 "a" []])]
        , [(0, 1, [notes 0 "b" []])]
        ]

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
make_track title events = TrackTree.track_events title tevents 100
    where
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
