{- | Slicing is chopping the block horizontally, so that the horizontal chunks
    can act like their own little blocks.

    For the sake of convenient notation, this is done in several places.

    1. A note with subtracks will slice out the subevents within its range.
    This allows a note to take another note (or notes) as arguments, e.g.
    a tuplet.  This is what 'slice_notes' is for.

    2. A note with controls as subtracks can invert itself so it has control
    over the evaluation of the controls.  Documented further in
    "Derive.Call.Note".

    3. #1 above is a convenient way to apply a transformation to multiple
    notes: group the tracks under another, and put the transformations in the
    parent.  However, notes that have no transformation must be extracted
    from underneath the empty parent, otherwise they will not be evaluated
    at all.  This is done at the 'derive_track' level, by 'extract_orphans'.

    This is a nasty tricky bit of work, but is depended on by all the high
    level notation, e.g. calls that can manipulate the results of other
    calls, aka NoteTransformers.  I'd still love to figure out a better way
    to do it though!
-}
module Derive.Slice where
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Data.Tree as Tree

import qualified Util.Then as Then
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State

import qualified Derive.TrackInfo as TrackInfo
import Types


-- | Extract slices of subtracks that are not covered under the events of
-- the given one.  Assuming the given track is a note track, these subevents
-- will otherwise never be evaluated since it's the responsibility of the
-- supernote to evaluate its subs.
--
-- Since there's no point to a control track with no note track underneath,
-- control track orphans are stripped out.
extract_orphans :: State.TrackEvents -> State.EventsTree -> State.EventsTree
extract_orphans _ [] = []
extract_orphans track subs = filter has_note $
    concatMap (\(exclusive, s, e) -> slice exclusive 1 s e Nothing subs) $
        event_gaps (Events.ascending (State.tevents_events track))
            (State.tevents_end track)
    where
    has_note = Monoid.getAny . Foldable.foldMap
        (Monoid.Any . TrackInfo.is_note_track . State.tevents_title)

-- | Given a list of events, return the gaps in between those events as
-- ranges.  Each range also has an \"exclusive\" flag, which indicates whether
-- the previous event was zero length.
--
-- This matters because a zero length note event should not be captured in the
-- orphan slice, since everything under it by definition is not orphaned.
event_gaps :: [Events.PosEvent] -> ScoreTime -> [(Bool, ScoreTime, ScoreTime)]
event_gaps events end = reverse $
        (if last_end >= end then [] else [(last_exclusive, last_end, end)])
            ++ gaps_rev
    where
    (gaps_rev, last_end, last_exclusive) =
        List.foldl' make_gap ([], 0, False) events
    make_gap (gaps, prev, exclusive) event
        | cur <= prev = (gaps, next, cur == next)
        | otherwise = ((exclusive, prev, cur) : gaps, next, cur == next)
        where (cur, next) = Events.range event


-- | Ask 'slice' to synthesize a note track and insert it at the leaves of
-- the sliced tree.
-- Event text, duration, tevents_range, tevents_around
data InsertEvent = InsertEvent String ScoreTime (ScoreTime, ScoreTime)
        ([Events.PosEvent], [Events.PosEvent])
    deriving (Show)

-- | Slice the tracks below me to lie within start and end, and optionally put
-- a note track with a single event of given string at the bottom.  Control
-- tracks actually get an event at or before and after the slice boundaries
-- since control calls usually want to interpolate from the previous event or
-- to the next one.
--
-- Tracks thare are empty as a result of slicing are omitted from the output.
-- This is necessary for note tracks because otherwise empty ones will stop
-- evaluation entirely.  It's not necessary for control tracks, but I'm being
-- consistent by stripping them too.  If the track title has some effect the
-- results might be inconsistent, but I'm not sure that will be real problem.
-- The result is [] if there are no events intersecting the given range.
slice :: Bool -- ^ Omit events than begin at the start.  'event_gaps' documents
    -- why this is necessary.
    -> Int -- ^ Capture this many control points after the slice boundary.
    -- Usually this is 1 since control calls usually generate samples from
    -- their predecessor, but may be more for inverting calls that want to
    -- see control values of succeeding events.
    -> ScoreTime -> ScoreTime
    -> Maybe InsertEvent
    -- ^ If given, insert an event at the bottom with the given text and dur.
    -- The created track will have the given track_range, so it can create
    -- a Stack.Region entry.
    -> State.EventsTree -> State.EventsTree
slice exclusive after start end insert_event = concatMap strip . map do_slice
    where
    do_slice (Tree.Node track subs) = Tree.Node (slice_t track)
        (if null subs then insert else map do_slice subs)
    insert = case insert_event of
        Nothing -> []
        Just (InsertEvent text dur track_range around) ->
            [Tree.Node (make text dur track_range around) []]
    -- The synthesized bottom track.
    make text dur track_range around = State.TrackEvents
        { State.tevents_title = ">"
        , State.tevents_events = Events.singleton start (Event.event text dur)
        , State.tevents_track_id = Nothing
        , State.tevents_end = end
        , State.tevents_range = track_range
        , State.tevents_sliced = True
        , State.tevents_around = around
        , State.tevents_shifted = 0
        }
    slice_t track = track
        { State.tevents_events = events track
        , State.tevents_end = sliced_start + end
        , State.tevents_range = (sliced_start + start, sliced_start + end)
        , State.tevents_sliced = True
        } -- If the track has already been sliced then (start, end) are
        -- relative to that previous slicing.  But since cache is based on
        -- the stack, which is absolute, tevents_range must retain the true
        -- absolute range.
        where sliced_start = fst (State.tevents_range track)
    -- Note tracks don't include pre and post events like control tracks.
    events track
        | TrackInfo.is_note_track (State.tevents_title track) =
            (if exclusive then Events.remove_event start else id)
                (Events.in_range start end es)
        | otherwise = events_around after start end es
        where es = State.tevents_events track

    strip (Tree.Node track subs)
        | State.tevents_events track == Events.empty =
            concatMap strip subs
        | otherwise = [Tree.Node track (concatMap strip subs)]

events_around :: Int -> ScoreTime -> ScoreTime -> Events.Events -> Events.Events
events_around after start end events = Events.from_asc_list $
    prev ++ Then.takeWhile ((<end) . fst) (take after) post
    where
    (pre, post) = Events.split start events
    prev = take_repeats $ case post of
        at@(p, _) : _ | p == start -> at : pre
        _ -> pre
    -- This is an icky hack.  The problem is that the ' call just repeats the
    -- previous value.  So slicing back to it doesn't do any good, I need the
    -- event before it.  The problem is that this low level machinery isn't
    -- supposed to depend on implementation details of specific calls.
    -- TODO I would have to make the event evaluation lazy in a way that a call
    -- wanting the previous value will cause the previous value to be
    -- evaluated, and at that point I could get rid of slicing entirely.  But
    -- I can't think of how to do that at the moment.
    take_repeats = Then.takeWhile1 ((==repeat) . Event.event_bs . snd)
    repeat = UTF8.fromString "'"

-- | Expect a note track somewhere in the tree.  Slice the tracks above and
-- below it to each of its events.
--
-- The shift of each Event will be subtracted from the track events, so they
-- start at 0.  Control tracks caught in the middle are extended one event on
-- either edge of the slice boundary courtesy of the 'slice' function.  Note
-- that there will be control events at negative ScoreTime if they lie before
-- the note.
--
-- If there are no note tracks, return [].
--
-- Technically the children of the note track don't need to be sliced, since
-- if it is inverting it will do that anyway.  But slicing lets me shift fewer
-- events, so it's probably a good idea anyway.
--
-- Since empty slices are removed from the output, an empty sub note track
-- will be excluded from derivation and won't cause an inverting call to
-- recurse endlessly.
--
-- If the parent track is empty then nothing can be done because this point
-- will never even be reached.  However, this situation is handled by the
-- track deriver, which should have called 'extract_orphans' already.
slice_notes :: ScoreTime -> ScoreTime -> State.EventsTree
    -> [[(ScoreTime, ScoreTime, State.EventsTree)]]
    -- ^ One list per note track, in right to left order.  Each track is
    -- @[(shift, stretch, tree)]@, in no guaranteed order.
slice_notes start end =
    map (map shift) . map slice_track . concatMap note_tracks
    where
    note_tracks (Tree.Node track subs)
        | TrackInfo.is_note_track (State.tevents_title track) =
            [([], track, subs)]
        | otherwise = [(track : parents, ntrack, nsubs)
            | (parents, ntrack, nsubs) <- concatMap note_tracks subs]
    slice_track (parents, track, subs) =
        map (slice_event (make_tree parents)) (Events.ascending events)
        where
        tevents = State.tevents_events track
        events
            | start == end = case Events.at start tevents of
                Nothing -> Events.empty
                Just e -> Events.singleton start e
            | otherwise = Events.in_range start end tevents
        make_tree [] = [Tree.Node track subs]
        make_tree (p:ps) = [Tree.Node p (make_tree ps)]
    slice_event tree event = (s, e - s, slice False 1 s e Nothing tree)
        where (s, e) = Events.range event
    shift (shift, stretch, tree) =
        (shift, stretch, map (fmap (shift_tree shift)) tree)
    shift_tree shift track = track
        { State.tevents_end = State.tevents_end track - shift
        , State.tevents_events = Events.map_sorted
            (\(p, e) -> (p - shift, e)) (State.tevents_events track)
        , State.tevents_shifted = State.tevents_shifted track + shift
        }
