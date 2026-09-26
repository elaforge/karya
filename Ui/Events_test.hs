-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Events_test where
import qualified Data.Maybe as Maybe
import           GHC.Stack (HasCallStack)
import qualified Test.QuickCheck as Q

import qualified Util.Lists as Lists
import qualified Util.Test.Testing as Testing
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import           Ui.Events (Range(..))
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_split_range :: Test
test_split_range = do
    let f range = eRanges . Events.splitRange range . fromStartDur
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

test_split :: Test
test_split = do
    let f = bimap eStartDur eStartDur . Events.split 2 . fromStartDur
    equal (f [(0, 2), (2, 2), (4, 2)]) ([(0, 2)], [(2, 2), (4, 2)])
    equal (f [(2, -2), (2, 2)]) ([(2, -2)], [(2, 2)])
    equal (f [(2, -0), (2, 0)]) ([(2, -0)], [(2, 0)])

eRanges :: (Events.Events, Events.Events, Events.Events)
    -> ([TrackTime], [TrackTime], [TrackTime])
eRanges (pre, within, post) =
    ( map Event.start (Events.descending pre)
    , map Event.start (Events.ascending within)
    , map Event.start (Events.ascending post)
    )

test_at_before :: Test
test_at_before = do
    let f pos = map Event.start $ Events.atBefore pos e1
        e1 = fromList [(0, 1, "0"), (1, 1, "1"), (2, 1, "2")]
    equal (f 0) [0]
    equal (f 0.5) [0]
    equal (f 1) [1, 0]
    equal (f 2.5) [2, 1, 0]

test_split_at_before :: Test
test_split_at_before = do
    let f pos = extract $ Events.splitAtBefore pos e1
        e1 = fromList [(0, 1, "0"), (1, 1, "1"), (2, 1, "2")]
        extract = bimap (map Event.start) (map Event.start)
    equal (f 0) ([], [0, 1, 2])
    equal (f 0.5) ([], [0, 1, 2])
    equal (f 1) ([0], [1, 2])
    equal (f 1.5) ([0], [1, 2])

test_clip :: Test
test_clip = do
    let f includeEnd end = map extractEvent
            . Events.clipList includeEnd end . posEvents
    let positive = [(0, 2, "a"), (2, 2, "b")]
    equal [f False end positive | end <- Lists.range 0 5 1]
        [ []
        , [(0, 1, "a")]
        , [(0, 2, "a")]
        , [(0, 2, "a"), (2, 1, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        ]
    equal [f True end positive | end <- Lists.range 0 5 1]
        [ [(0, 0, "a")]
        , [(0, 1, "a")]
        , [(0, 2, "a"), (2, 0, "b")]
        , [(0, 2, "a"), (2, 1, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        ]
    equal [f False end [(2, -2, "a"), (4, -2, "b")] | end <- Lists.range 0 3 1]
        [ []
        , []
        , [(2, -2, "a")]
        , [(2, -2, "a")]
        ]

test_insert :: Test
test_insert = do
    let f new old = extract $ Events.insert (posEvents new) (fromList old)
    equalE (f [(0, 1, "a0")] [(3, 1, "b0")])
        [(0, 1, "a0"), (3, 1, "b0")]
    equalE (f [(0, 4, "a0"), (4, 4, "a1")] [(2, 4, "b0"), (6, 4, "b1")])
        [(0, 2, "a0"), (2, 2, "b0"), (4, 2, "a1"), (6, 4, "b1")]
    -- Inserting overlapping events are clipped.
    equalE (f [(0, 4, "a0"), (2, 4, "a1")] []) [(0, 2, "a0"), (2, 4, "a1")]
    equalE (f [(1, 3, "new")] [(0, 2, "old")]) [(0, 1, "old"), (1, 3, "new")]
    -- If the start is coincident, the existing events are replaced.
    equalE (f [(0, 1, "a"), (2, 1, "b")] [(0, 0, "1"), (2, 0, "2")])
        [(0, 1, "a"), (2, 1, "b")]
    equalE (f [(1, 0, "1"), (1.25, 0, "1.25"), (2, 0, "2")]
            [(0, 0, "0"), (0.25, 0, "0.25")])
        [ (0, 0, "0"), (0.25, 0, "0.25"), (1, 0, "1"), (1.25, 0, "1.25")
        , (2, 0, "2")
        ]
    -- Negative events.
    equalE (f [(4, -2, "a")] [(3, -3, "b")]) [(3, -3, "b"), (4, -1, "a")]
    equalE (f [(3, -3, "b")] [(4, -2, "a")]) [(3, -3, "b"), (4, -1, "a")]
    equalE (f [(2, -2, "a")] [(0, 2, "b")]) [(0, 1, "b"), (2, -1, "a")]

    -- New events win over old ones.
    equalE (f [(0, 2, "new")] [(0, 2, "old")]) [(0, 2, "new")]
    equalE (f [(1, 0, "new")] [(1, 0, "old")]) [(1, 0, "new")]
    equalE (f [(2, -2, "new")] [(2, -2, "old")]) [(2, -2, "new")]
    equalE (f [(1, -0, "new")] [(1, -0, "old")]) [(1, -0, "new")]

test_from_list :: Test
test_from_list = do
    let f = Events.ascending . Events.fromList
    equal (f [Event.unmodified $ Event.event 0 1 "hi"])
        [Event.unmodified $ Event.event 0 1 "hi"]

test_from_list_qc :: Test
test_from_list_qc = quickcheck $ Q.forAll genEvents $ \es -> do
    let events = map mkevent es
    qEqual (Events.ascending (Events.fromList events))
        (Events.clipEvents (Lists.sortOn Events.eventKey events))

test_insert_qc :: Test
test_insert_qc = quickcheck $ Q.forAll ((,) <$> genEvents <*> genEvents) $
    \(es1, es2) -> qEqual
        (findOverlaps $ Events.ascending $ Events.insert
            (map mkevent es1) (Events.fromList (map mkevent es2)))
        []

test_clip_events_qc :: Test
test_clip_events_qc = quickcheck $ Q.forAll genEvents $ \es -> do
    let clipped = Events.clipEvents (Lists.sortOn Event.start (map mkevent es))
    qEqual (findOverlaps clipped) []

test_clip_events :: Test
test_clip_events = do
    let f = map extractEvent .  Events.clipEvents . posEvents
    -- Drop ones with the same start and orientation.
    equalE (f [(0, 1, "a"), (0, 2, "b")]) [(0, 2, "b")]
    equalE (f [(2, -1, "a"), (2, -2, "b")]) [(2, -2, "b")]
    equalE (f [(2, -2, "a"), (2, 2, "b")]) [(2, -2, "a"), (2, 2, "b")]
    -- Positive.
    equalE (f [(0, 1, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equalE (f [(0, 2, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equalE (f [(0, 0, "a"), (1, 0, "b")]) [(0, 0, "a"), (1, 0, "b")]
    -- Negative and Positive compromise in the middle.
    equalE (f [(1, -1, "a"), (1, 1, "b")]) [(1, -1, "a"), (1, 1, "b")]
    equalE (f [(1, -0, "a"), (1, 0, "b")]) [(1, -0, "a"), (1, 0, "b")]
    equalE (f [(0, 2, "a"), (2, -1, "b")]) [(0, 1.5, "a"), (2, -0.5, "b")]
    equalE (f [(0, 8, "a"), (2, -1, "b")]) [(0, 1.5, "a"), (2, -0.5, "b")]
    equalE (f [(0, 2, "a"), (2, -8, "b")]) [(0, 1, "a"), (2, -1, "b")]
    equalE (f [(0, 2, "a"), (3, -2, "b")]) [(0, 1.5, "a"), (3, -1.5, "b")]

test_remove :: Test
test_remove = do
    let f range = eStartDur . Events.remove range . fromStartDur
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

test_round_events :: Test
test_round_events = do
    let move events = Events.insert [next] $
            Events.remove (Point (Event.start e) Types.Positive) events
            where
            e = Maybe.fromJust $ Events.head events
            next = Event.start_ %= (+ 1/3) $ e
    -- If round_event is doing its thing, this won't fall victim to
    -- imprecision.
    equal (toList $ iterate move (fromList [(0, 1, "a")]) !! (20 * 3))
        [(20, 1, "a")]


-- * util

equalE :: HasCallStack => [Event] -> [Event] -> Test
equalE = Testing.equalFmt UiTest.fmtEvents

type Event = (ScoreTime, ScoreTime, Text)

fromStartDur :: [(TrackTime, TrackTime)] -> Events.Events
fromStartDur evts = fromList [(p, d, showt p) | (p, d) <- evts]

fromList :: [Event] -> Events.Events
fromList = Events.fromList . posEvents

toList :: Events.Events -> [Event]
toList = map (\e -> (Event.start e, Event.duration e, Event.text e))
    . Events.ascending

posEvents :: [Event] -> [Event.Event]
posEvents = map (\(pos, dur, text) -> Event.event pos dur text)

extract :: Events.Events -> [(TrackTime, TrackTime, Text)]
extract = map extractEvent . Events.ascending

eStartDur :: Events.Events -> [(TrackTime, TrackTime)]
eStartDur = map (\(s, d, _) -> (s, d)) . extract

extractEvent :: Event.Event -> (TrackTime, TrackTime, Text)
extractEvent e = (Event.start e, Event.duration e, Event.text e)

mkevent :: (Int, Int) -> Event.Event
mkevent (start, dur) = Event.event (fromIntegral start) (fromIntegral dur) ""

genEvents :: Q.Gen [(Int, Int)]
genEvents = Q.listOf ((,) <$> Q.choose (-4, 4) <*> Q.choose (-4, 4))

findOverlaps :: [Event.Event] -> [(Event.Event, Event.Event)]
findOverlaps = mapMaybe check . Lists.zipNext
    where
    check (cur, Just next)
        | Event.end cur > Event.start next = Just (cur, next)
        | otherwise = Nothing
    check _ = Nothing
