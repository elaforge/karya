-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Event_test where
import qualified Util.Lists as Lists
import Util.Test
import qualified Ui.Event as Event
import Global
import Types


test_set_start :: Test
test_set_start = do
    let f p = extract $ Event.set_start p (Event.event 4 (-2) "a")
    equal (map f (Lists.range 0 5 1))
        [(0, 2), (1, 1), (2, -0), (3, -1), (4, -2), (5, -3)]

test_set_end :: Test
test_set_end = do
    let f p = extract $ Event.end_ #= p $ Event.event 4 (-2) "a"
    equal (map f (Lists.range 0 5 1))
        [(4, -4), (4, -3), (4, -2), (4, -1), (4, 0), (4, 1)]

test_overlaps :: Test
test_overlaps = do
    let f = Event.overlaps
    equal [f p (Event.event 1 0 "") | p <- [0, 1, 2]] [False, True, False]
    equal [f p (Event.event 1 (-0) "") | p <- [0, 1, 2]] [False, True, False]
    equal [f p (Event.event 1 2 "") | p <- [0, 1, 2, 3, 4]]
        [False, True, True, False, False]
    equal [f p (Event.event 3 (-2) "") | p <- [0, 1, 2, 3, 4]]
        [False, False, True, True, False]

extract :: Event.Event -> (TrackTime, TrackTime)
extract e = (Event.start e, Event.duration e)
