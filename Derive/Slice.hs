-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
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
    parent.  However, notes that have no transformation (aka orphans) must be
    extracted from underneath the empty parent, otherwise they will not be
    evaluated at all.  This is done by 'Derive.Call.derive_note_track'.

    This is a nasty tricky bit of work, but is depended on by all the high
    level notation, e.g. calls that can manipulate the results of other
    calls, aka NoteTransformers.  I'd still love to figure out a better way
    to do it though!
-}
module Derive.Slice (
    InsertEvent(..), slice
    , checked_slice_notes
#ifdef TESTING
    , slice_notes
#endif
) where
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import Types


-- | Ask 'slice' to synthesize a note track and insert it at the leaves of
-- the sliced tree.
data InsertEvent = InsertEvent {
    ins_text :: !Text
    , ins_duration :: !ScoreTime
    , ins_range :: !(ScoreTime, ScoreTime)
    , ins_around :: !([Event.Event], [Event.Event])
    -- | The TrackId for the track created for this event.  This is required
    -- so it can collect a TrackDynamic and when the Cmd level looks at at
    -- track with inverted note calls, it sees the environ established by the
    -- tracks that the calls are inverted beneath.  E.g., if the pitch track
    -- sets a scale, the Cmd layer should see the note track as having that
    -- scale.
    , ins_track_id :: !(Maybe TrackId)
    } deriving (Show)

{- | Slice the tracks below me to lie within start and end, and optionally put
    a note track with a single event of given string at the bottom.  Sliced
    control tracks usually get events beyond the slice boundaries for context.

    Tracks thare are empty as a result of slicing are omitted from the output.
    This is necessary for note tracks because otherwise empty ones will stop
    evaluation entirely.  It's not necessary for control tracks, but I'm being
    consistent by stripping them too.  If the track title has some effect the
    results might be inconsistent, but I'm not sure that will be real problem.
    The result is [] if there are no events intersecting the given range.
-}
slice :: Bool -- ^ Omit events than begin at the start.
    -- 'slice_notes' documents why this is necessary.
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
slice exclude_start around start end insert_event = map do_slice
    where
    do_slice (Tree.Node track subs) = Tree.Node (slice_t track)
        (if null subs then insert (TrackTree.tevents_shifted track)
            else map do_slice subs)
    insert shift = case insert_event of
        Nothing -> []
        Just insert_event -> [Tree.Node (make shift insert_event) []]
    -- The synthesized bottom track.
    make shift (InsertEvent text dur trange around track_id) =
        TrackTree.TrackEvents
            { TrackTree.tevents_title = ">"
            , TrackTree.tevents_events =
                Events.singleton (Event.event start dur text)
            , TrackTree.tevents_track_id = track_id
            , TrackTree.tevents_block_id = Nothing
            , TrackTree.tevents_end = end
            , TrackTree.tevents_range = trange
            , TrackTree.tevents_sliced = True
            , TrackTree.tevents_inverted = True
            , TrackTree.tevents_around = around
            -- Since a note may be inverted and inserted after 'slice_notes'
            -- and its shifting, I have to get the shift from the parent track.
            , TrackTree.tevents_shifted = shift
            }
    slice_t track = track
        { TrackTree.tevents_events = within
        , TrackTree.tevents_end = end
        -- TODO There's something fishy about this because for the inverted
        -- track this winds up being the track range of the unsliced parent,
        -- which means it can be larger than the range of the slice above it.
        -- But without this, the cache test fails.  Figure it out and
        -- clarify this field.
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
        | ParseTitle.is_note_track title =
            extract_note_events exclude_start start end es
        | otherwise = extract_control_events (ParseTitle.is_pitch_track title)
            around start end es
        where
        es = TrackTree.tevents_events track
        title = TrackTree.tevents_title track

-- | Note tracks don't include pre and post events like control tracks.
extract_note_events :: Bool -> ScoreTime -> ScoreTime
    -> Events.Events -> ([Event.Event], Events.Events, [Event.Event])
extract_note_events exclude_start start end events =
    case (exclude_start, Events.at start within) of
        (True, Just event) ->
            (event : pre, Events.remove_event start within, post)
        _ -> (pre, within, post)
    where
    (pre, within, post) = (Events.descending pre, within, Events.ascending post)
        where (pre, within, post) = Events.split_range start end events

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

    -- Some calls rely on the previous val.  'Derive.Args.prev_val' will
    -- evaluate it in that case, but if I know that the call will want the
    -- previous val for sure it's probably more efficient to extend the slice
    -- back a bit and let 'Derive.info_prev_val' provide the previous val.
    -- This way the previous call will only be evaluated once.  However, this
    -- hacky check is unreliable because it doesn't really know what calls are
    -- in scope, and doesn't work for val calls at all.
    call_of = Maybe.fromMaybe "" . Parse.parse_call . Event.event_text
    take_pre before = Then.span ((`Set.member` require_previous) . call_of)
        (splitAt before)
    require_previous = if is_pitch_track then pitch_require_previous
        else control_require_previous

-- | This should contain the calls that require the previous value.  It's used
-- by a hack in 'slice'.  TODO this is terrible, fix it.
control_require_previous :: Set.Set Text
control_require_previous = Set.fromList
    ["'", "i>", "i>>", "i<<", "e>", "e>>", "e<<", "u", "d"]

-- | TODO Same terrible hack as 'control_require_previous'.
pitch_require_previous :: Set.Set Text
pitch_require_previous = Set.fromList
    ["'", "i>", "i>>", "i<<", "e>", "e>>", "e<<", "a", "u", "d"]

{- | Expect a note track somewhere in the tree.  Slice the tracks above and
    below it to each of its events.

    The shift of each Event will be subtracted from the track events, so they
    start at 0.  Control tracks caught in the middle are extended one event on
    either edge of the slice boundary courtesy of the 'slice' function.  Note
    that there will be control events at negative ScoreTime if they lie before
    the note.

    Technically the children of the note track don't need to be sliced, since
    if it is inverting it will do that anyway.  But slicing lets me shift fewer
    events, so it's probably a good idea anyway.

    Since empty slices are removed from the output, an empty sub note track
    will be excluded from derivation and won't cause an inverting call to
    recurse endlessly.

    If the parent track is empty then nothing can be done because this point
    will never even be reached.  However, this situation is handled by the
    track deriver, which should have called 'extract_orphans' already.

    But if a sliced note track has yet another a note track underneath it, and
    that note track has orphan notes, then the track deriver's extract_orphans
    will miss it because it's hidden under an event.  So this function will also
    extract orphans in the same way.

    Ick.
-}
slice_notes :: Bool -> ScoreTime -> ScoreTime -> TrackTree.EventsTree
    -> [[Note]] -- ^ One [Note] per sub note track, in right to left order.
slice_notes exclude_start start end tracks
    | null tracks || if exclude_start then start >= end else start > end = []
    | otherwise = filter (not . null) $
        map (mapMaybe strip_note . slice_track) $
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
    slice1 tree (n_start, n_end, n_next) =
        (n_start, n_end - n_start,
            map (fmap (shift_tree n_start n_next)) $
            slice exclude (1, 1) n_start n_end Nothing tree)
        where exclude = exclude_start && n_start == start
        -- Only exclude_start if 's' is still the original 'start'.
    shift_tree shift next track = track
        { TrackTree.tevents_events =
            Events.map_events move (TrackTree.tevents_events track)
        , TrackTree.tevents_end = next - shift
        , TrackTree.tevents_around =
            let (prev, next) = TrackTree.tevents_around track
            in (map move prev, map move next)
        , TrackTree.tevents_shifted = TrackTree.tevents_shifted track + shift
        }
        where move = Event.move (subtract shift)

-- | (parents, track, 'event_ranges', subs)
type Sliced = ([TrackTree.TrackEvents], TrackTree.TrackEvents,
    [(ScoreTime, ScoreTime, ScoreTime)], TrackTree.EventsTree)

-- | (start, dur, tracks)
type Note = (ScoreTime, ScoreTime, TrackTree.EventsTree)

-- | Get slice ranges for a track.  This gets the non-overlapping ranges of all
-- the note tracks events below.
event_ranges :: ScoreTime -> ScoreTime -> TrackTree.EventsNode
    -> [(ScoreTime, ScoreTime, ScoreTime)]
    -- ^ [(start, end, next_start)]
event_ranges start end = nonoverlapping . to_ranges
    where
    to_ranges = Seq.merge_lists (\(s, _, _) -> s) . map track_events
        . filter is_note . Tree.flatten
    track_events = map range . Seq.zip_next . Events.ascending
        . Events.in_range_point start end
        . TrackTree.tevents_events
    range (event, next) =
        (Event.min event, Event.max event,
            max (Event.max event) (maybe end Event.min next))
    nonoverlapping [] = []
    nonoverlapping (r:rs) = r : nonoverlapping (dropWhile (overlaps r) rs)
    overlaps (s1, e1, _) (s2, e2, _) = not $ e1 <= s2 || e2 <= s1

-- More aggressive.
-- strip_note :: Note -> Maybe Note
-- strip_note (start, dur, tree)
--     | null stripped = Nothing
--     | otherwise = Just (start, dur, stripped)
--     where stripped = concatMap strip_empty_tracks tree
--
-- -- | If a branch has no note track children with events, there's no way it can
-- -- produce any events, so it can be dropped before derivation.
-- strip_empty_tracks :: TrackTree.EventsNode -> TrackTree.EventsTree
-- strip_empty_tracks (Tree.Node track subs)
--     | null stripped && (not (is_note track) || track_empty track) = []
--     | otherwise = [Tree.Node track stripped]
--     where stripped = concatMap strip_empty_tracks subs

strip_note :: Note -> Maybe Note
strip_note (start, dur, tree)
    | null stripped = Nothing
    | otherwise = Just (start, dur, tree)
    where stripped = filter has_note_events tree

-- | If a branch has no note track children with events, there's no way it can
-- produce any events, so it can be dropped before derivation.
has_note_events :: TrackTree.EventsNode -> Bool
has_note_events (Tree.Node track subs) =
    is_note track && not (track_empty track) || any has_note_events subs

{- | Slice overlaps are when an event overlaps the start of the slice range,
    or extends past the end.  They're bad because they tend to cause notes to
    get doubled.  This is because a slice is expanded by larger sub-events, so
    the events under the overlapping event have likely already be evaluated by
    the previous slice.

    From a certain point of view, slices represent nested function calls.  An
    overlapping event would then represent a function that somehow exists in
    two call branches simultaneously, and there's no way to make sense of that
    without duplicating the call branches.  E.g., visually:

    > a-b-
    > c---
    > 1-2-

    The call @c@ overlaps @b@.  To make this into a call graph, you have to
    either omit @c@ even though it looks like it has scope over @2@:

    > a (c 1) (b 2)

    Or duplicate it:

    > a (c 1) (b (c 2))

    Duplication is reasonable for some calls, i.e. ones that treat all their
    sub-events uniformly, but not the rest.  Besides, when @a@ slices, @c@ will
    expand it to include @1@ and @2@, so the actual result is

    > a (c 1 2) (b ...?)

    It's ok for a sub-event to be larger than its caller, because there are
    zero-duration note transformers that take non-zero-duration sub-events.

    This check also requires me to delay stripping out empty tracks until after
    the check has been done, because otherwise @b@ wouldn't notice that @c@
    overlaps.
-}
find_overlapping :: ScoreTime -> TrackTree.EventsNode
    -> Maybe (Maybe TrackId, (TrackTime, TrackTime))
find_overlapping start = find overlaps
    where
    overlaps track = case TrackTree.tevents_around track of
        (prev : _, _)
            | is_note track && Event.end prev > start ->
                Just (TrackTree.tevents_track_id track,
                    (shifted (Event.start prev), shifted (Event.end prev)))
            -- Or if end is > parent end, unless start == parent start
        _ -> Nothing
        where shifted = (+ TrackTree.tevents_shifted track)

-- | This is 'slice_notes', but throw an error if 'find_overlapping' complains.
checked_slice_notes :: Bool -> ScoreTime -> ScoreTime -> TrackTree.EventsTree
    -> Either String [[Note]]
checked_slice_notes exclude_start start end tracks
    | null overlaps = Right $ filter (not . null) notes
    | otherwise = Left $ "slice has overlaps: "
            <> Seq.join ", " (map show_overlap overlaps)
    where
    notes = slice_notes exclude_start start end tracks
    -- Only check the first note of each slice.  Since the notes are
    -- increasing, this is the only one which might start before the slice.
    -- Since the events have been shifted back by the slice start, an event
    -- that extends over 0 means it overlaps the beginning of the slice.
    overlaps = mapMaybe
            (\(_, _, subs) -> msum (map (find_overlapping 0) subs))
            (mapMaybe Seq.head notes)

show_overlap :: (Maybe TrackId, (TrackTime, TrackTime)) -> String
show_overlap (Nothing, (start, end)) =
    pretty start <> "--" <> pretty end
show_overlap (Just track_id, (start, end)) =
    pretty $ State.Range Nothing track_id start end

-- * util

is_note :: TrackTree.TrackEvents -> Bool
is_note = ParseTitle.is_note_track . TrackTree.tevents_title

track_empty :: TrackTree.TrackEvents -> Bool
track_empty = Events.null . TrackTree.tevents_events

-- | 'List.find' generalized to Foldable.
find :: Foldable.Foldable t => (a -> Maybe b) -> t a -> Maybe b
find f = Monoid.getFirst . Foldable.foldMap (Monoid.First . f)
