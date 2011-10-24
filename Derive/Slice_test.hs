module Derive.Slice_test where
import qualified Data.Tree as Tree
import Data.Tree (Tree(Node))

import qualified Util.Seq as Seq
import Util.Test
import Ui
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State

import qualified Derive.Slice as Slice


test_extract_orphans = do
    let f events subs = extract_tree $
            Slice.extract_orphans (uncurry make_track events) (make_tree subs)
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
        mkevents ranges = [(s, Event.event "" (e-s)) | (e, s) <- ranges]
    equal (f [] 1) [(False, 0, 1)]
    equal (f [(1, 2), (2, 3)] 4) [(False, 0, 1), (False, 3, 4)]
    equal (f [(2, 2)] 4) [(False, 0, 2), (True, 2, 4)]

test_slice = do
    let f exclusive s e insert =
            extract_tree . Slice.slice exclusive s e insert . make_tree
    equal (f False 1 2 Nothing [Node (make_notes 2 "ab") []]) []
    equal (f False 1 2 Nothing [Node (make_notes 1 "ab") []])
        [Node (">", [(1, 1, "a")]) []]
    equal (f True 1 2 Nothing [Node (make_notes 1 "ab") []]) []

    -- control tracks get neighbors
    equal (f False 1 2 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [0, 2]) []]
    equal (f False 2 5 Nothing [Node (make_controls "c" [0, 2..10]) []])
        [Node (make_controls "c" [2, 4, 6]) []]

test_slice_notes = do
    let extract = map (map (\(s, e, t) -> (s, e, extract_tree t)))
    let f s e = extract . Slice.slice_notes s e . make_tree
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

-- * util

type Event = (ScoreTime, ScoreTime, String)
type EventsTree = [Tree.Tree (String, [Event])]

extract_tree :: State.EventsTree -> EventsTree
extract_tree = map $ fmap $ \track ->
    (State.tevents_title track, extract_track (State.tevents_events track))

extract_track :: Events.Events -> [Event]
extract_track events =
    [(p, Event.event_duration e, Event.event_string e)
        | (p, e) <- Events.ascending events]

make_tree :: EventsTree -> State.EventsTree
make_tree = map $ \(Node (title, events) subs) ->
    Node (make_track title events) (make_tree subs)

make_track :: String -> [Event] -> State.TrackEvents
make_track title events =
    State.TrackEvents title tevents Nothing 100 (0, 100) False
    where
    tevents = Events.from_list
        [(start, Event.event text dur) | (start, dur, text) <- events]

make_controls :: String -> [Int] -> (String, [Event])
make_controls title ps = (title, [(to_score p, 0, show p) | p <- ps])

make_controls2 :: String -> [(Int, String)] -> (String, [Event])
make_controls2 title ps = (title, [(to_score p, 0, val) | (p, val) <- ps])

to_score = ScoreTime.double . fromIntegral

make_notes :: ScoreTime -> String -> (String, [Event])
make_notes offset notes = (">",
    zipWith (\start note -> (start, 1, note : "")) (Seq.range_ offset 1) notes)
