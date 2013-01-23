module Derive.Call.Note_test where
import Data.Tree (Tree(Node))

import Util.Test
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State

import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Note as Note
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Slice_test as Slice_test


test_random = do
    -- make sure notes in different tracks get different starts
    let extract e = (Score.event_start e, Score.event_duration e)
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks
    let ([e1, e2], []) = run [("start-rnd", [(0, 0, ".1")]),
            (">", [(0, 1, "")]), (">", [(0, 1, "")])]
    check (e1 /= e2)
    equal (fst e1 + snd e1) 1
    equal (fst e2 + snd e2) 1

    let ([e1, e2], []) = run [("dur-rnd", [(0, 0, ".1")]),
            (">", [(0, 1, "")]), (">", [(0, 1, "")])]
    equal (fst e1) (fst e2)
    check (snd e1 /= snd e2)

test_invert_call = do
    let run args = DeriveTest.extract_run extract $
            DeriveTest.run State.empty (Note.invert_call (1, 1) args)
        extract = fmap Slice_test.extract_tree
    -- it's ok, it's empty
    equal (run (mkargs "" [Node (">", []) []])) $
        Right $ Just [Node (">", [(0, 1, "")]) []]
    -- but put an event in there and we have a problem
    left_like (run (mkargs "" [Node (">", [(0, 0, "")]) []]))
        "will lead to an endless loop: Nothing"

    let control = make_controls "c"
        note text = (">", [(0, 1, text)])
    equal (run (mkargs "" [Node (control [0..4]) []])) $ Right $
        Just [Node (control [0..4]) [Node (note "") []]]
    equal (run (mkargs "x | y" [Node (control [0..4]) []])) $ Right $
        Just [Node (control [0..4]) [Node (note "y") []]]
    -- if there are multiple subs, it gets inverted below all of them
    equal (run (mkargs "x | y"
        [Node (control [0]) [], Node (control [0..4]) []])) $ Right $ Just
            [ Node (control [0]) [Node (note "y") []]
            , Node (control [0..4]) [Node (note "y") []]
            ]

test_orphan_notes = do
    -- Slice out orphans that aren't covered by a parent event.
    -- Also tested in 'Derive.Slice_test.test_slice_notes_orphans'.
    -- This is analogous to track level orphans, which are tested in
    -- "Ui.Call.BlockUtil_test".
    let run = DeriveTest.extract extract . DeriveTest.linear_derive_tracks id
        extract e = (DeriveTest.e_note2 e, DeriveTest.e_attributes e)
    equal (run
        [ (">inst", [(0, 2, "a = b")])
        , (">", [])
        , (">", [(0, 1, ""), (1, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d")])
        ])
        ([((0, 1, "4c"), "-"), ((1, 1, "4d"), "-")], [])
    equal (run
        [ (">inst", [(0, 2, "a = b")])
        , (">", [(0, 1, "+a")])
        , (">", [(0, 1, ""), (1, 1, "")])
        , ("*", [(0, 0, "4c"), (1, 0, "4d")])
        ])
        ([((0, 1, "4c"), "+a"), ((1, 1, "4d"), "-")], [])

make_tree = Slice_test.make_tree
make_controls :: String -> [Int] -> (String, [Slice_test.Event])
make_controls title ps = (title, [(to_score p, 0, show p) | p <- ps])
to_score = ScoreTime.double . fromIntegral

mkargs :: String -> Slice_test.EventsTree -> Derive.PassedArgs d
mkargs text subs = Derive.PassedArgs [] "call" info
    where
    event = Event.event 0 1 text
    info = Derive.CallInfo
        { Derive.info_expr = CallTest.expr (Event.event_string event)
        , Derive.info_prev_val = Nothing
        , Derive.info_event = event
        , Derive.info_prev_events = prev
        , Derive.info_next_events = next
        , Derive.info_event_end = event_end
        , Derive.info_track_range = (0, event_end)
        , Derive.info_sub_tracks = make_tree subs
        , Derive.info_track_type = Nothing
        }
    prev = []
    next = [Event.event (Event.end event) 0 "next"]
    event_end = 100
