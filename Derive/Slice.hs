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
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import qualified Util.Then as Then
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Call.Control as Control
import qualified Derive.Call.Pitch as Pitch
import qualified Derive.ParseBs as ParseBs
import qualified Derive.TrackInfo as TrackInfo

import Types


-- | Extract slices of subtracks that are not covered under the events of
-- the given one.  Assuming the given track is a note track, these subevents
-- will otherwise never be evaluated since it's the responsibility of the
-- supernote to evaluate its subs.
--
-- The toplevel parent track is omitted entirely, so you want the transformers
-- in the titles of the skipped tracks to apply you have to apply them
-- yourself.  TODO but that's complicated if there are multiple skipped tracks,
-- why not return the intervening track titles?
--
-- Since there's no point to a control track with no note track underneath,
-- control track orphans are stripped out.
extract_orphans :: Bool -- ^ If true, if the extracted orphan ranges themselves
    -- have orphans, extract those too.  This must be False for BlockUtil
    -- because it evaluates the returned tracks recursively, at which point
    -- extract_orphans will be called again.  But even though I tried, I can't
    -- figure out how to get 'slice_notes' to have the same recursive
    -- structure, so magic flag it is.
    -> Maybe (ScoreTime, ScoreTime)
    -> TrackTree.TrackEvents -> TrackTree.EventsTree -> TrackTree.EventsTree
extract_orphans _ _ _ [] = []
extract_orphans recursive maybe_range track subs = filter has_note $
    concatMap slice_gap $
        event_gaps (track_events track) (TrackTree.tevents_end track)
    where
    slice_gap (exclusive, start, end) = concat $
        tree : if recursive
            then [extract_orphans True maybe_range track subs
                | Tree.Node track subs <- tree]
            else []
        where tree = slice exclusive (1, 1) start end Nothing subs
    track_events = Events.ascending
        . maybe id (uncurry Events.in_range_point) maybe_range
        . TrackTree.tevents_events
    has_note = Monoid.getAny . Foldable.foldMap
        (Monoid.Any . TrackInfo.is_note_track . TrackTree.tevents_title)

-- | Given a list of events, return the gaps in between those events as
-- ranges.  Each range also has an \"exclusive\" flag, which indicates whether
-- the previous event was zero length.
--
-- This matters because a zero length note event should not be captured in the
-- orphan slice, since everything under it by definition is not orphaned.
event_gaps :: [Event.Event] -> ScoreTime -> [(Bool, ScoreTime, ScoreTime)]
event_gaps events end = reverse $
        (if last_end >= end then [] else [(last_exclusive, last_end, end)])
            ++ gaps_rev
    where
    (gaps_rev, last_end, last_exclusive) =
        List.foldl' make_gap ([], 0, False) events
    make_gap (gaps, prev, exclusive) event
        | cur <= prev = (gaps, next, cur == next)
        | otherwise = ((exclusive, prev, cur) : gaps, next, cur == next)
        where (cur, next) = Event.range event


-- | Ask 'slice' to synthesize a note track and insert it at the leaves of
-- the sliced tree.
-- Event text, duration, tevents_range, tevents_around
data InsertEvent = InsertEvent {
    ins_text :: String
    , ins_duration :: ScoreTime
    , ins_range :: (ScoreTime, ScoreTime)
    , ins_around :: ([Event.Event], [Event.Event])
    , ins_track_id :: Maybe TrackId
    } deriving (Show)

-- | Slice the tracks below me to lie within start and end, and optionally put
-- a note track with a single event of given string at the bottom.  Sliced
-- control tracks usually get events beyond the slice boundaries for context.
--
-- Tracks thare are empty as a result of slicing are omitted from the output.
-- This is necessary for note tracks because otherwise empty ones will stop
-- evaluation entirely.  It's not necessary for control tracks, but I'm being
-- consistent by stripping them too.  If the track title has some effect the
-- results might be inconsistent, but I'm not sure that will be real problem.
-- The result is [] if there are no events intersecting the given range.
slice :: Bool -- ^ Omit events than begin at the start.  'event_gaps' documents
    -- why this is necessary.
    -> (Int, Int)
    -- ^ Capture this many control points at+before and after the slice
    -- boundaries.  For the purposes of getting events around, the event at the
    -- start is considered before.  This is so that getting 1 event before
    -- means at or before, which is is appropriate for many control calls which
    -- generate samples from the previous event.  Usually it's (1, 1) for one
    -- at+before event and one following event, which is enough context for the
    -- typical control call, but may be more for inverting calls that want to
    -- see control values of preceeding or succeeding events.
    --
    -- TODO the use of (before, after) is probably incorrect.  It should be
    -- that many note track events worth of control events before or after the
    -- slice range.
    -> ScoreTime -> ScoreTime
    -> Maybe InsertEvent
    -- ^ If given, insert an event at the bottom with the given text and dur.
    -- The created track will have the given track_range, so it can create
    -- a Stack.Region entry.
    -> TrackTree.EventsTree -> TrackTree.EventsTree
slice exclusive around start end insert_event = concatMap strip . map do_slice
    where
    do_slice (Tree.Node track subs) = Tree.Node (slice_t track)
        (if null subs then insert else map do_slice subs)
    insert = case insert_event of
        Nothing -> []
        Just insert_event -> [Tree.Node (make insert_event) []]
    -- The synthesized bottom track.
    make (InsertEvent text dur trange around track_id) =
        TrackTree.TrackEvents
            { TrackTree.tevents_title = ">"
            , TrackTree.tevents_events =
                Events.singleton (Event.event start dur text)
            , TrackTree.tevents_track_id = track_id
            , TrackTree.tevents_end = end
            , TrackTree.tevents_range = trange
            , TrackTree.tevents_sliced = True
            , TrackTree.tevents_around = around
            , TrackTree.tevents_shifted = 0
            }
    slice_t track = track
        { TrackTree.tevents_events = within
        , TrackTree.tevents_end = sliced_start + end
        , TrackTree.tevents_range = (sliced_start + start, sliced_start + end)
        , TrackTree.tevents_sliced = True
        , TrackTree.tevents_around = (before, after)
        } -- If the track has already been sliced then (start, end) are
        -- relative to that previous slicing.  But since cache is based on
        -- the stack, which is absolute, tevents_range must retain the true
        -- absolute range.
        where
        (before, within, after) = extract_events track
        sliced_start = fst (TrackTree.tevents_range track)
    -- Extract events from an intermediate track.
    extract_events track
        | TrackInfo.is_note_track title =
            extract_note_events exclusive start end es
        | otherwise = extract_control_events (TrackInfo.is_pitch_track title)
            around start end es
        where
        es = TrackTree.tevents_events track
        title = TrackTree.tevents_title track

    strip (Tree.Node track subs)
        | TrackTree.tevents_events track == Events.empty =
            concatMap strip subs
        | otherwise = [Tree.Node track (concatMap strip subs)]

-- | Note tracks don't include pre and post events like control tracks.
extract_note_events :: Bool -> ScoreTime -> ScoreTime
    -> Events.Events -> ([Event.Event], Events.Events, [Event.Event])
extract_note_events exclusive start end events =
    case (exclusive, Events.at start within) of
        (True, Just event) ->
            (event : pre, Events.remove_event start within, post)
        _ -> (pre, within, post)
    where
    (pre, within, post) = case Events.split_range start end events of
        (pre, within, post) ->
            (Events.descending pre, within, Events.ascending post)

extract_control_events :: Bool -> (Int, Int) -> ScoreTime -> ScoreTime
    -> Events.Events -> ([Event.Event], Events.Events, [Event.Event])
    -- ^ (descending_pre, within, ascending_post)
extract_control_events is_pitch_track (before, after) start end events =
    (pre2, Events.from_list $ reverse pre_within ++ within, post2)
    where
    -- Implements the "before is at+before" as documented in 'slice'.
    (pre1, post1) = case Events.split start events of
        (pre, at : post) | Event.start at == start -> (at : pre, post)
        a -> a
    (pre_within, pre2) = take_pre before pre1
    (within, post2) = Then.span ((<end) . Event.start) (splitAt after) post1

    -- This is an icky hack.  The problem is that some calls rely on the
    -- previous value.  So slicing back to them doesn't do any good, I need the
    -- event before.  The problem is that this low level machinery isn't
    -- supposed to depend on implementation details of specific calls.
    -- TODO I would have to make the event evaluation lazy in a way that a call
    -- wanting the previous value will cause the previous value to be
    -- evaluated, and at that point I could get rid of slicing entirely.  But
    -- I can't think of how to do that at the moment.
    call_of = Maybe.fromMaybe "" . ParseBs.parse_call . Event.event_bytestring
    take_pre before = Then.span ((`Set.member` require_previous) . call_of)
        (splitAt before)
    require_previous = if is_pitch_track then Pitch.require_previous
        else Control.require_previous

-- | Expect a note track somewhere in the tree.  Slice the tracks above and
-- below it to each of its events.
--
-- The shift of each Event will be subtracted from the track events, so they
-- start at 0.  Control tracks caught in the middle are extended one event on
-- either edge of the slice boundary courtesy of the 'slice' function.  Note
-- that there will be control events at negative ScoreTime if they lie before
-- the note.
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
--
-- But if a sliced note track has yet another a note track underneath it, and
-- that note track has orphan notes, then the track deriver's extract_orphans
-- will miss it because it's hidden under an event.  So this function will also
-- extract orphans in the same way.
--
-- Ick.
slice_notes :: ScoreTime -> ScoreTime -> TrackTree.EventsTree
    -> [[(ScoreTime, ScoreTime, TrackTree.EventsTree)]]
    -- ^ One list per note track, in right to left order.  Each track is
    -- @[(shift, stretch, tree)]@, in no guaranteed order.  Null if there
    -- were no note tracks.
slice_notes start end =
    filter (not . null) . map (map shift . slice_track) . concatMap note_tracks
    where
    -- Find the first note tracks.
    note_tracks (Tree.Node track subs)
        | TrackInfo.is_note_track (TrackTree.tevents_title track) =
            ([], track, subs)
            : [([], otrack, osubs) | Tree.Node otrack osubs
                <- extract_orphans True (Just (start, end)) track subs]
        | otherwise = [(track : parents, ntrack, nsubs)
            | (parents, ntrack, nsubs) <- concatMap note_tracks subs]
    -- For each note track, slice out each event.
    slice_track ::
        ([TrackTree.TrackEvents], TrackTree.TrackEvents, TrackTree.EventsTree)
        -> [(ScoreTime, ScoreTime, TrackTree.EventsTree)]
    slice_track (parents, track, subs) =
        map (slice_event (make_tree parents)) (Events.ascending events)
        where
        events = Events.in_range_point start end
            (TrackTree.tevents_events track)
        make_tree [] = [Tree.Node track subs]
        make_tree (p:ps) = [Tree.Node p (make_tree ps)]
    slice_event tree event = (s, e - s, slice False (1, 1) s e Nothing tree)
        where (s, e) = Event.range event
    shift (shift, stretch, tree) =
        (shift, stretch, map (fmap (shift_tree shift)) tree)
    shift_tree shift track = track
        { TrackTree.tevents_events =
            Events.map_events move (TrackTree.tevents_events track)
        , TrackTree.tevents_end = TrackTree.tevents_end track - shift
        , TrackTree.tevents_around =
            let (prev, next) = TrackTree.tevents_around track
            in (map move prev, map move next)
        , TrackTree.tevents_shifted = TrackTree.tevents_shifted track + shift
        }
        where move = Event.move (subtract shift)
