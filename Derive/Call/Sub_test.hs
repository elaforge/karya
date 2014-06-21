-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Sub_test where
import Data.Tree (Tree(Node))

import Util.Control
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State

import qualified Derive.Call.Sub as Sub
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Slice_test as Slice_test


test_invert_call = do
    let run args = DeriveTest.extract_run extract $
            DeriveTest.run State.empty (Sub.invert_call args)
        extract = fmap $ fmap $ fmap Slice_test.extract_tree
    -- it's ok, it's empty
    equal (run (mkargs "" [Node (">", []) []])) $
        Right $ Just [Node (">", []) [Node (">", [(0, 1, "")]) []]]
    -- but put an event in there and we have a problem
    left_like (run (mkargs "" [Node (">", [(0, 0, "")]) []]))
        "will lead to an endless loop"

    let control = make_controls "c"
        note text = (">", [(0, 1, text)])
    equal (run (mkargs "" [Node (control [0..4]) []])) $ Right $
        Just [Node (control [0..4]) [Node (note "") []]]
    equal (run (mkargs "x | y" [Node (control [0..4]) []])) $ Right $
        Just [Node (control [0..4]) [Node (note "x | y") []]]
    -- if there are multiple subs, it gets inverted below all of them
    equal (run (mkargs "x | y"
        [Node (control [0]) [], Node (control [0..4]) []])) $ Right $ Just
            [ Node (control [0]) [Node (note "x | y") []]
            , Node (control [0..4]) [Node (note "x | y") []]
            ]

test_inverting = do
    let run = DeriveTest.extract_events extract
            . DeriveTest.derive_tracks_linear ""
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    -- Make sure empty tracks are stripped.  But the title of the empty track
    -- is still applied.
    equal (run [(">", [(0, 1, "")]), ("> | +a", []), ("*", [(0, 0, "4c")])])
        [((0, 1, "4c"), "+a")]
    -- Initial empty track is also stripped.
    equal (run [(">", []), (">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
        [((0, 1, "4c"), "+")]

test_sub_notes = do
    let run tracks = DeriveTest.extract extract $
            DeriveTest.derive_tracks_linear "" tracks
        extract = DeriveTest.e_attributes
    -- Ensure zero duration subs show up.
    equal (run [(">", [(0, 2, "+a")]), (">", [(0, 0, ""), (1, 0, "")])])
        (["+a", "+a"], [])
    -- It should show up only once!
    equal (run
            [(">", [(0, 0, "+a")]), (">", [(0, 0, "+b")]), (">", [(0, 1, "")])])
        (["+a+b"], [])

make_controls :: String -> [Int] -> (String, [Slice_test.Event])
make_controls title ps = (title, [(to_score p, 0, showt p) | p <- ps])
    where to_score = ScoreTime.double . fromIntegral

mkargs :: Text -> [Slice_test.EventsTree] -> Derive.PassedArgs d
mkargs text subs = Derive.PassedArgs [] "call" info
    where
    event = Event.event 0 1 text
    info = Derive.CallInfo
        { Derive.info_expr = Event.event_text event
        , Derive.info_prev_val = Nothing
        , Derive.info_event = event
        , Derive.info_prev_events = prev
        , Derive.info_next_events = next
        , Derive.info_event_end = event_end
        , Derive.info_track_shifted = 0
        , Derive.info_inverted = False
        , Derive.info_sub_tracks = map Slice_test.make_tree subs
        , Derive.info_sub_events = Nothing
        , Derive.info_track_type = Nothing
        }
    prev = []
    next = [Event.event (Event.end event) 0 "next"]
    event_end = 100
