-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Event_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Event as Event
import Types


test_set_start = do
    let f p = extract $ Event.set_start p (Event.event 4 (-2) "a")
    equal (map f (Seq.range 0 5 1))
        [(0, 2), (1, 1), (2, -0), (3, -1), (4, -2), (5, -3)]

test_set_end = do
    let f p = extract $ Event.set_end p (Event.event 4 (-2) "a")
    equal (map f (Seq.range 0 5 1))
        [(4, -4), (4, -3), (4, -2), (4, -1), (4, 0), (4, 1)]

extract :: Event.Event -> (TrackTime, TrackTime)
extract e = (Event.start e, Event.duration e)
