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

    I should always strip empty tracks from the output.  See
    'strip_empty_tracks' for details and rationale.

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

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Call.Control as Control
import qualified Derive.Call.Pitch as Pitch
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as ParseBs
import qualified Derive.TrackInfo as TrackInfo

import Types


-- | Extract slices of subtracks that are not covered under the events of
-- the given one.  Assuming the given track is a note track, these subevents
-- will otherwise never be evaluated since it's the responsibility of the
-- supernote to evaluate its subs.
--
-- The toplevel parent track is omitted entirely, so you want its title to
-- apply you have to do it yourself.
extract_orphans :: TrackTree.TrackEvents -> TrackTree.EventsTree
    -> [((ScoreTime, ScoreTime), TrackTree.EventsNode)]
    -- ^ [((start, end), track)]
extract_orphans _ [] = []
extract_orphans track subs = strip_empty $
    filter (Foldable.any is_note . snd) $ concatMap slice_gap gaps
    where
    gaps = event_gaps (TrackTree.tevents_end track) (track_events track)
    slice_gap (exclusive, start, end) = [((start, end), t) | t <- tree]
        where tree = slice exclusive (1, 1) start end Nothing subs
    track_events = map Event.range . Events.ascending . TrackTree.tevents_events
    strip_empty tracks =
        filter has_note_track [(range, stripped)
            | (range, node) <- tracks, stripped <- strip_empty_tracks node]
        where has_note_track (_, node) = Foldable.any is_note node

-- | Given a list of events, return the gaps in between those events as
-- ranges.  Each range also has an \"exclusive\" flag, which indicates whether
-- the previous event was zero length.
--
-- This matters because a zero length note event should not be captured in the
-- orphan slice, since everything under it by definition is not orphaned.
event_gaps :: ScoreTime -> [(ScoreTime, ScoreTime)]
    -> [(Bool, ScoreTime, ScoreTime)]
event_gaps end ranges = reverse $
        (if last_end >= end then [] else [(last_exclusive, last_end, end)])
            ++ gaps_rev
    where
    (gaps_rev, last_end, last_exclusive) =
        List.foldl' make_gap ([], 0, False) ranges
    make_gap (gaps, prev, exclusive) (cur, next)
        | cur <= prev = (gaps, next, cur == next)
        | otherwise = ((exclusive, prev, cur) : gaps, next, cur == next)


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
slice exclusive around start end insert_event = map do_slice
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

-- | Strip out tracks with no events.  This is necessary because otherwise they
-- would cause evaluation to stop, or in the case of inversion, cause the
-- \"inverting below a note track will cause an endless loop\" error from
-- 'Derive.Call.Note.invert'.  I used to do it inside 'slice', but I need to
-- keep them around until 'find_overlapping' has done its thing.
strip_empty_tracks :: TrackTree.EventsNode -> TrackTree.EventsTree
strip_empty_tracks (Tree.Node track subs)
    | TrackTree.tevents_events track == Events.empty = stripped
    | otherwise = [Tree.Node track stripped]
    where stripped = concatMap strip_empty_tracks subs

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
slice_notes :: ScoreTime -> ScoreTime -> TrackTree.EventsTree -> [[Note]]
    -- ^ One [Note] per sub note track, in right to left order.
slice_notes start end tracks =
    filter (not . null) $ map (map shift . slice_track) $
        concatMap note_tracks tracks
    where
    note_tracks :: TrackTree.EventsNode -> [Sliced]
    note_tracks node@(Tree.Node track subs)
        | is_note track = [([], track, event_ranges start end node, subs)]
        | otherwise =
            [ (track : parents, ntrack, slices, nsubs)
            | (parents, ntrack, slices, nsubs) <- concatMap note_tracks subs
            ]
    -- For each note track, slice out each event.
    slice_track :: Sliced -> [Note]
    slice_track (parents, track, slices, subs) =
        map (slice1 (make_tree parents)) slices
        where
        make_tree [] = [Tree.Node track subs]
        make_tree (p:ps) = [Tree.Node p (make_tree ps)]
    slice1 tree (s, e) = (s, e - s, slice False (1, 1) s e Nothing tree)
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

-- | Slice overlaps are when an event overlaps the start of the slice range.
-- They're bad because they tend to cause notes to get doubled.  This is
-- because a slice is expanded by larger sub-events, so the events under the
-- overlapping event have likely already be evaluated by the previous slice.
--
-- From another point of view, slices represent nested function calls.  An
-- overlapping event would then represent a function that somehow exists in
-- two call branches simultaneously, and there's no way to make sense of that
-- without duplicating the call branches.  E.g., visually:
--
-- > a-b-
-- > c---
-- > 1-2-
--
-- The call @c@ overlaps @b@.  To make this into a call graph, you have to
-- either omit @c@ even though it looks like it has scope over @2@:
--
-- > a (c 1) (b 2)
--
-- Or duplicate it:
--
-- > a (c 1) (b (c 2))
--
-- Duplication is reasonable for some calls, i.e. ones that treat all their
-- sub-events uniformly, but not the rest.  Besides, when @a@ slices, @c@ will
-- expand it to include @1@ and @2@, so the actual result is
--
-- > a (c 1 2) (b ...?)
--
-- It's ok for a sub-event to be larger than its caller, because there are
-- zero-duration note transformers that take non-zero-duration sub-events.
--
-- This check also requires me to delay stripping out empty tracks until after
-- the check has been done, because otherwise @b@ wouldn't notice that @c@
-- overlaps.
find_overlapping :: Note -> Maybe (Maybe TrackId, (TrackTime, TrackTime))
find_overlapping (_, _, subs) = msum (map (find overlaps) subs)
    where
    overlaps track = case TrackTree.tevents_around track of
        (prev : _, _)
            -- Since the events have been shifted back by the slice start, an
            -- event that extends over 0 means it overlaps the beginning of the
            -- slice.
            | Event.end prev > 0 ->
                Just (TrackTree.tevents_track_id track,
                    (shifted (Event.start prev), shifted (Event.end prev)))
        _ -> Nothing
        where shifted = (+ TrackTree.tevents_shifted track)

-- | This is 'slice_notes', but throw an error if 'find_overlapping' complains.
checked_slice_notes :: ScoreTime -> ScoreTime -> TrackTree.EventsTree
    -> Derive.Deriver [[Note]]
checked_slice_notes start end tracks = do
    let notes = slice_notes start end tracks
    -- Only check the first note of each slice.  Since the notes are
    -- increasing, this is the only one which might start before the slice.
    let overlapping = mapMaybe find_overlapping (mapMaybe Seq.head notes)
    unless (null overlapping) $ do
        Derive.throw $ "slice has overlaps: " <> Pretty.pretty overlapping
    return $ filter (not . null) $ map strip_notes notes
    where
    strip_notes :: [Note] -> [Note]
    strip_notes tracks =
        filter non_null [(start, end, concatMap strip_empty_tracks tree)
            | (start, end, tree) <- tracks ]
        where non_null (_, _, xs) = not (null xs)

-- | (parents, track, slices, subs)
type Sliced = ([TrackTree.TrackEvents], TrackTree.TrackEvents,
    [(ScoreTime, ScoreTime)], TrackTree.EventsTree)
-- | (start, dur, tracks)
type Note = (ScoreTime, ScoreTime, TrackTree.EventsTree)

-- | Get slice ranges for a track.  This gets the non-overlapping ranges of all
-- the note tracks events below.
event_ranges :: ScoreTime -> ScoreTime -> TrackTree.EventsNode
    -> [(ScoreTime, ScoreTime)]
event_ranges start end = nonoverlapping . to_ranges
    where
    to_ranges = Seq.merge_lists fst . map track_events . filter is_note
        . Tree.flatten
    track_events = map Event.range . Events.ascending
        . Events.in_range_point start end
        . TrackTree.tevents_events
    nonoverlapping [] = []
    nonoverlapping (r:rs) = r : nonoverlapping (dropWhile (overlaps r) rs)
    overlaps (s1, e1) (s2, e2) = not $ e1 <= s2 || e2 <= s1

-- * util

is_note :: TrackTree.TrackEvents -> Bool
is_note = TrackInfo.is_note_track . TrackTree.tevents_title

find :: (Foldable.Foldable t) => (a -> Maybe b) -> t a -> Maybe b
find f = Monoid.getFirst . Foldable.foldMap (Monoid.First . f)
