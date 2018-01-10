-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Events_test where
import qualified Data.Maybe as Maybe
import qualified Test.QuickCheck as Q

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.Testing as Testing

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import Ui.Events (Range(..))
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest

import Global
import Types


test_split_range = do
    let f range = e_ranges . Events.split_range range . from_start_dur
    -- Half-open for positive.
    equal (f (Range 1 2) [(0, 1), (1, 1), (2, 1), (3, 1)])
        ([0], [1], [2, 3])
    -- Half-open for negative.
    equal (f (Range 1 2) [(1, -1), (2, -1), (3, -1)])
        ([1], [2], [3])
    equal (f (Range 1 2) [(0, 1), (1, 0.5), (2, -0.5), (3, 1)])
        ([0], [1, 2], [3])
    equal (f (Range 1 2) [(0, 0), (1, 0), (2, 0), (3, 0)])
        ([0], [1], [2, 3])
    -- An empty range divides into before and after, even for negative events.
    equal (f (Range 1 1) [(1, 1)]) ([], [], [1])
    equal (f (Range 1 1) [(1, -1)]) ([1], [], [])

    equal (f (Point 1 Types.Positive) [(0, 1), (1, 1), (2, 1)])
        ([0], [1], [2])
    equal (f (Point 1 Types.Positive) [(1, -1), (1, 1), (2, 1)])
        ([1], [1], [2])
    equal (f (Point 1 Types.Negative) [(1, -1), (1, 1), (2, 1)])
        ([], [1], [1, 2])
    equal (f (Point 0.5 Types.Positive) [(1, -1), (1, 1), (2, 1)])
        ([], [], [1, 1, 2])

test_split = do
    let f = (e_start_dur *** e_start_dur) . Events.split 2 . from_start_dur
    equal (f [(0, 2), (2, 2), (4, 2)]) ([(0, 2)], [(2, 2), (4, 2)])
    equal (f [(2, -2), (2, 2)]) ([(2, -2)], [(2, 2)])
    equal (f [(2, -0), (2, 0)]) ([(2, -0)], [(2, 0)])

e_ranges :: (Events.Events, Events.Events, Events.Events)
    -> ([TrackTime], [TrackTime], [TrackTime])
e_ranges (pre, within, post) =
    ( map Event.start (Events.descending pre)
    , map Event.start (Events.ascending within)
    , map Event.start (Events.ascending post)
    )

test_split_at_before = do
    let f pos = extract $ Events.split_at_before pos e1
        e1 = from_list [(0, 1, "0"), (1, 1, "1"), (2, 1, "2")]
        extract = map Event.start *** map Event.start
    equal (f 0) ([], [0, 1, 2])
    equal (f 0.5) ([], [0, 1, 2])
    equal (f 1) ([0], [1, 2])
    equal (f 1.5) ([0], [1, 2])

test_clip = do
    let f include_end end = map extract_event
            . Events.clip include_end end . pos_events
    let positive = [(0, 2, "a"), (2, 2, "b")]
    equal [f False end positive | end <- Seq.range 0 5 1]
        [ []
        , [(0, 1, "a")]
        , [(0, 2, "a")]
        , [(0, 2, "a"), (2, 1, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        ]
    equal [f True end positive | end <- Seq.range 0 5 1]
        [ [(0, 0, "a")]
        , [(0, 1, "a")]
        , [(0, 2, "a"), (2, 0, "b")]
        , [(0, 2, "a"), (2, 1, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        ]
    equal [f False end [(2, -2, "a"), (4, -2, "b")] | end <- Seq.range 0 3 1]
        [ []
        , []
        , [(2, -2, "a")]
        , [(2, -2, "a")]
        ]

test_insert = do
    let f new old = extract $ Events.insert (pos_events new) (from_list old)
    equal_e (f [(0, 1, "a0")] [(3, 1, "b0")])
        [(0, 1, "a0"), (3, 1, "b0")]
    equal_e (f [(0, 4, "a0"), (4, 4, "a1")] [(2, 4, "b0"), (6, 4, "b1")])
        [(0, 2, "a0"), (2, 2, "b0"), (4, 2, "a1"), (6, 4, "b1")]
    -- Inserting overlapping events are clipped.
    equal_e (f [(0, 4, "a0"), (2, 4, "a1")] []) [(0, 2, "a0"), (2, 4, "a1")]
    equal_e (f [(1, 3, "new")] [(0, 2, "old")]) [(0, 1, "old"), (1, 3, "new")]
    -- If the start is coincident, the existing events are replaced.
    equal_e (f [(0, 1, "a"), (2, 1, "b")] [(0, 0, "1"), (2, 0, "2")])
        [(0, 1, "a"), (2, 1, "b")]
    equal_e (f [(1, 0, "1"), (1.25, 0, "1.25"), (2, 0, "2")]
            [(0, 0, "0"), (0.25, 0, "0.25")])
        [ (0, 0, "0"), (0.25, 0, "0.25"), (1, 0, "1"), (1.25, 0, "1.25")
        , (2, 0, "2")
        ]
    -- Negative events.
    equal_e (f [(4, -2, "a")] [(3, -3, "b")]) [(3, -3, "b"), (4, -1, "a")]
    equal_e (f [(3, -3, "b")] [(4, -2, "a")]) [(3, -3, "b"), (4, -1, "a")]
    equal_e (f [(2, -2, "a")] [(0, 2, "b")]) [(0, 1, "b"), (2, -1, "a")]

    -- New events win over old ones.
    equal_e (f [(0, 2, "new")] [(0, 2, "old")]) [(0, 2, "new")]
    equal_e (f [(1, 0, "new")] [(1, 0, "old")]) [(1, 0, "new")]
    equal_e (f [(2, -2, "new")] [(2, -2, "old")]) [(2, -2, "new")]
    equal_e (f [(1, -0, "new")] [(1, -0, "old")]) [(1, -0, "new")]

test_from_list_qc = quickcheck $ Q.forAll gen_events $ \es -> do
    let events = map mkevent es
    q_equal (Events.ascending (Events.from_list events))
        (Events.clip_events (Seq.sort_on Events.event_key events))

test_insert_qc = quickcheck $ Q.forAll ((,) <$> gen_events <*> gen_events) $
    \(es1, es2) -> q_equal
        (find_overlaps $ Events.ascending $ Events.insert
            (map mkevent es1) (Events.from_list (map mkevent es2)))
        []

test_clip_events_qc = quickcheck $ Q.forAll gen_events $ \es -> do
    let clipped = Events.clip_events (Seq.sort_on Event.start (map mkevent es))
    q_equal (find_overlaps clipped) []

test_clip_events = do
    let f = map extract_event .  Events.clip_events . pos_events
    -- Drop ones with the same start and orientation.
    equal_e (f [(0, 1, "a"), (0, 2, "b")]) [(0, 2, "b")]
    equal_e (f [(2, -1, "a"), (2, -2, "b")]) [(2, -2, "b")]
    equal_e (f [(2, -2, "a"), (2, 2, "b")]) [(2, -2, "a"), (2, 2, "b")]
    -- Positive.
    equal_e (f [(0, 1, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equal_e (f [(0, 2, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equal_e (f [(0, 0, "a"), (1, 0, "b")]) [(0, 0, "a"), (1, 0, "b")]
    -- Negative and Positive compromise in the middle.
    equal_e (f [(1, -1, "a"), (1, 1, "b")]) [(1, -1, "a"), (1, 1, "b")]
    equal_e (f [(1, -0, "a"), (1, 0, "b")]) [(1, -0, "a"), (1, 0, "b")]
    equal_e (f [(0, 2, "a"), (2, -1, "b")]) [(0, 1.5, "a"), (2, -0.5, "b")]
    equal_e (f [(0, 8, "a"), (2, -1, "b")]) [(0, 1.5, "a"), (2, -0.5, "b")]
    equal_e (f [(0, 2, "a"), (2, -8, "b")]) [(0, 1, "a"), (2, -1, "b")]
    equal_e (f [(0, 2, "a"), (3, -2, "b")]) [(0, 1.5, "a"), (3, -1.5, "b")]

test_remove = do
    let f range = e_start_dur . Events.remove range . from_start_dur
    -- able to remove 0 dur events
    equal (f (Point 0 Types.Positive) [(0, 0), (1, 1)]) [(1, 1)]
    equal (f (Point 1 Types.Negative) [(0, 0), (1, -0), (1, 0), (2, 0)])
        [(0, 0), (1, 0), (2, 0)]
    equal (f (Point 1 Types.Positive) [(0, 0), (1, -0), (1, 0), (2, 0)])
        [(0, 0), (1, -0), (2, 0)]

    equal (f (Range 0 1) [(0, 0), (1, 0)]) [(1, 0)]
    equal (f (Range 0 1) [(0, 0), (1, -0)]) []
    equal (f (Range 0 1) [(0, -0), (1, -0)]) [(0, -0)]
    equal (f (Range 1 2) [(0, 1), (3, 1)]) [(0, 1), (3, 1)]

test_round_events = do
    let move events = Events.insert [next] $
            Events.remove (Point (Event.start e) Types.Positive) events
            where
            e = Maybe.fromJust $ Events.head events
            next = Event.start_ %= (+ 1/3) $ e
    -- If round_event is doing its thing, this won't fall victim to
    -- imprecision.
    equal (to_list $ iterate move (from_list [(0, 1, "a")]) !! (20 * 3))
        [(20, 1, "a")]


-- * util

equal_e :: CallStack.Stack => [Event] -> [Event] -> IO Bool
equal_e = Testing.equal_fmt UiTest.fmt_events

type Event = (ScoreTime, ScoreTime, Text)

from_start_dur :: [(TrackTime, TrackTime)] -> Events.Events
from_start_dur evts = from_list [(p, d, showt p) | (p, d) <- evts]

from_list :: [Event] -> Events.Events
from_list = Events.from_list . pos_events

to_list :: Events.Events -> [Event]
to_list = map (\e -> (Event.start e, Event.duration e, Event.text e))
    . Events.ascending

pos_events :: [Event] -> [Event.Event]
pos_events = map (\(pos, dur, text) -> Event.event pos dur text)

extract :: Events.Events -> [(TrackTime, TrackTime, Text)]
extract = map extract_event . Events.ascending

e_start_dur :: Events.Events -> [(TrackTime, TrackTime)]
e_start_dur = map (\(s, d, _) -> (s, d)) . extract

extract_event :: Event.Event -> (TrackTime, TrackTime, Text)
extract_event e = (Event.start e, Event.duration e, Event.text e)

mkevent :: (Int, Int) -> Event.Event
mkevent (start, dur) = Event.event (fromIntegral start) (fromIntegral dur) ""

gen_events :: Q.Gen [(Int, Int)]
gen_events = Q.listOf ((,) <$> Q.choose (-4, 4) <*> Q.choose (-4, 4))

find_overlaps :: [Event.Event] -> [(Event.Event, Event.Event)]
find_overlaps = mapMaybe check . Seq.zip_next
    where
    check (cur, Just next)
        | Event.end cur > Event.start next = Just (cur, next)
        | otherwise = Nothing
    check _ = Nothing
