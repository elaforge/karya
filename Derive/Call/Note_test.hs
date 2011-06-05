module Derive.Call.Note_test where
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import Util.Test
import Ui
import qualified Ui.Event as Event
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Derive.Call.Note as Note
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal


test_tuplet = do
    let extract e = (Score.event_start e, Score.event_duration e,
            PitchSignal.unsignal_degree $ Score.event_pitch e)
    let run = DeriveTest.extract_events extract
            . DeriveTest.derive_tracks_with_ui id set_skel
        set_skel state = UiTest.exec state $
            State.set_skeleton UiTest.default_block_id $
                Skeleton.make [(1, 2), (2, 3)]
    let tracks =
            [ (">", [(0, 12, "t")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            ]
    equal (run tracks)
        [ (0, 4, [(0, 60)])
        , (4, 4, [(4, 62)])
        , (8, 4, [(8, 64)])
        ]

    -- tuplet + inversion
    let tracks =
            [ (">", [(0, 12, "t")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            ]
    equal (run tracks)
        [ (0, 4, [(0, 60)])
        , (4, 4, [(4, 62)])
        , (8, 4, [(8, 64)])
        ]

    -- notes of unequal length
    let tracks =
            [ (">", [(0, 6, "t")])
            , (">", [(0, 1, ""), (1, 2, "")])
            ]
    equal (run tracks)
        [ (0, 2, [(0, -1)])
        , (2, 4, [(0, -1)])
        ]

    equal (run [(">", [(0, 1, "")]), (">", []), ("*twelve", [(0, 0, "4c")])])
        [(0, 1, [(0, 60)])]

test_slice_notes = do
    let extract = map (\(s, e, t) -> (s, e, extract_tree t))
    let f s e t = Seq.sort_on (\(s, _, _) -> s) $ extract $
            Note.slice_notes s e t
        t0 = make_tree [Tree.Node (make_notes 0 "ab") []]

    -- no sub tracks works too
    equal (f 0 1 (make_tree [Tree.Node (make_notes 0 "abc") []]))
        [(0, 1, [Tree.Node (make_notes 0 "a") []])]

    -- simple sub tracks
    equal (f 0 2 t0)
        [ (0, 1, [Tree.Node (">", [(0, 1, "a")]) []])
        , (1, 1, [Tree.Node (">", [(0, 1, "b")]) []])
        ]
    equal (f 0 1 t0)
        [(0, 1, [Tree.Node (">", [(0, 1, "a")]) []])]

    -- no note tracks, no output
    equal (f 0 1 (make_tree [Tree.Node (make_controls "c" [0..6]) []]))
        []

    -- empty note track is ignored
    equal (f 0 1 (make_tree [Tree.Node (make_notes 0 "abc")
            [Tree.Node (make_notes 0 "")
                [Tree.Node (make_controls "c" [0]) []]]]))
        [(0, 1, [Tree.Node (make_notes 0 "a")
            [Tree.Node (make_controls "c" [0]) []]])]

    -- make sure parent track order doesn't get messed up
    equal (f 0 1 (make_tree [Tree.Node (make_controls "c1" [0..6])
            [Tree.Node (make_controls "c2" [0..6])
                [Tree.Node (make_notes 0 "a") []]]]))
        [ (0, 1, [Tree.Node (make_controls "c1" [0, 1])
            [Tree.Node (make_controls "c2" [0, 1])
                [Tree.Node (">", [(0, 1, "a")]) []]]])
        ]

    -- simple child control slicing
    equal (f 1 2 (make_tree [Tree.Node (make_notes 0 "abc")
            [Tree.Node (make_controls "c" [0..6]) []]]))
        [(1, 1, [Tree.Node (">", [(0, 1, "b")])
            [Tree.Node (make_controls "c" [1, 2]) []]])]
    let t1 = make_tree
            [ Tree.Node (make_controls "c1" [0..4])
                [Tree.Node (make_notes 1 "ab")
                    [Tree.Node (make_controls "c2" [0..4]) []]]
            , Tree.Node (make_notes 1 "cd") []
            ]
    equal (f 1 3 t1)
        [ (1, 1, [Tree.Node (make_controls "c1" [1, 2])
                    [Tree.Node (">", [(0, 1, "a")])
                        [Tree.Node (make_controls "c2" [1, 2]) []]]])
        , (1, 1, [Tree.Node (">", [(0, 1, "c")]) []])
        , (2, 1, [Tree.Node (make_controls "c1" [2, 3])
                    [Tree.Node (">", [(0, 1, "b")])
                        [Tree.Node (make_controls "c2" [2, 3]) []]]])
        , (2, 1, [Tree.Node (">", [(0, 1, "d")]) []])
        ]

extract_tree :: State.EventsTree -> Tree.Forest (String, [Event])
extract_tree = map $ \(Tree.Node track subs) ->
    Tree.Node
        (State.tevents_title track, extract_track (State.tevents_events track))
        (extract_tree subs)

extract_track :: Track.TrackEvents -> [Event]
extract_track events =
    [(p, Event.event_duration e, Event.event_string e)
        | (p, e) <- Track.event_list events]

make_tree :: Tree.Forest (String, [Event]) -> State.EventsTree
make_tree = map $ \(Tree.Node (title, events) subs) ->
    Tree.Node (make_track title events) (make_tree subs)

make_track :: String -> [Event] -> State.TrackEvents
make_track title events = State.TrackEvents title
    (Track.make_track_events
        [(start, Event.event text dur) | (start, dur, text) <- events])
    100 Nothing Nothing

make_controls :: String -> [Int] -> (String, [Event])
make_controls title vals =
    (title, zipWith (\start val -> (start, 0, show val)) [0..] vals)

make_notes :: ScoreTime -> String -> (String, [Event])
make_notes offset notes =
    (">", zipWith (\start note -> (start, 1, note : "")) [offset..] notes)

type Event = (ScoreTime, ScoreTime, String)
