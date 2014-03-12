-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Sub_test where
import Data.Tree (Tree(Node))

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
            DeriveTest.run State.empty (Sub.invert_call (1, 1) args)
        extract = fmap Slice_test.extract_tree
    -- it's ok, it's empty
    equal (run (mkargs "" [Node (">", []) []])) $
        Right $ Just [Node (">", [(0, 1, "")]) []]
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

make_controls :: String -> [Int] -> (String, [Slice_test.Event])
make_controls title ps = (title, [(to_score p, 0, show p) | p <- ps])
    where to_score = ScoreTime.double . fromIntegral

mkargs :: String -> Slice_test.EventsTree -> Derive.PassedArgs d
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
        , Derive.info_track_range = (0, event_end)
        , Derive.info_inverted = False
        , Derive.info_sub_tracks = Slice_test.make_tree subs
        , Derive.info_sub_events = Nothing
        , Derive.info_track_type = Nothing
        }
    prev = []
    next = [Event.event (Event.end event) 0 "next"]
    event_end = 100
