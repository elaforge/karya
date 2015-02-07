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
    , slice_orphans
#ifdef TESTING
    , strip_empty_tracks
    , slice_notes
#endif
) where
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import qualified Util.Then as Then
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree

import qualified Derive.ParseTitle as ParseTitle
import Global
import Types


-- | Ask 'slice' to synthesize a note track and insert it at the leaves of
-- the sliced tree.
data InsertEvent = InsertEvent {
    ins_duration :: !ScoreTime
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
    -> ScoreTime -> ScoreTime -> Maybe InsertEvent
    -- ^ If given, insert an event at the bottom with the given text and dur.
    -- The created track will have the given track_range, so it can create
    -- a Stack.Region entry.
    -> [TrackTree.EventsNode] -> [TrackTree.EventsNode]
slice exclude_start start end insert_event = map do_slice
    where
    do_slice (Tree.Node track subs) = Tree.Node (slice_t track) $ if null subs
        then insert (TrackTree.track_shifted track)
            (TrackTree.track_block_id track)
        else map do_slice subs
    insert shift block_id = case insert_event of
        Nothing -> []
        Just insert_event -> [Tree.Node (make shift block_id insert_event) []]
    -- The synthesized bottom track.  Since slicing only happens within
    -- a block, I assume the BlockId is the same as the parent.  I need
    -- a BlockId to look up the previous val in 'Derive.Threaded'.
    make shift block_id (InsertEvent dur around track_id) = TrackTree.Track
        { TrackTree.track_title = ">"
        , TrackTree.track_events = Events.singleton (Event.event start dur "")
        , TrackTree.track_id = track_id
        , TrackTree.track_block_id = block_id
        , TrackTree.track_end = end
        , TrackTree.track_sliced = True
        , TrackTree.track_inverted = True
        , TrackTree.track_around = around
        -- Since a note may be inverted and inserted after 'slice_notes'
        -- and its shifting, I have to get the shift from the parent track.
        , TrackTree.track_shifted = shift
        , TrackTree.track_voice = Nothing
        }
    slice_t track = track
        { TrackTree.track_events = within
        , TrackTree.track_end = end
        , TrackTree.track_sliced = True
        , TrackTree.track_around = (before, after)
        }
        where (before, within, after) = extract_events track
    -- Extract events from an intermediate track.
    extract_events track
        | ParseTitle.is_note_track title =
            extract_note_events exclude_start start end events
        | otherwise = extract_control_events start end events
        where
        events = TrackTree.track_events track
        title = TrackTree.track_title track

-- | Note tracks don't include pre and post events like control tracks.
extract_note_events :: Bool -> ScoreTime -> ScoreTime
    -> Events.Events -> ([Event.Event], Events.Events, [Event.Event])
extract_note_events exclude_start start end events =
    (if exclude_start then exclude_s else id) $
        let (pre, within, post) = Events.split_range_point start end events
        in (Events.descending pre, within, Events.ascending post)
    where
    exclude_s (pre, within, post) = case Events.at start within of
        Just event -> (event : pre, Events.remove_event start within, post)
        Nothing -> (pre, within, post)

extract_control_events :: ScoreTime -> ScoreTime
    -> Events.Events -> ([Event.Event], Events.Events, [Event.Event])
    -- ^ (descending_pre, within, ascending_post)
extract_control_events start end events = (pre, Events.from_list within, post2)
    where
    (pre, post1) = case Events.split start events of
        (at_1:pre, at:post) | Event.start at > start -> (pre, at_1:at:post)
        (at_1:pre, []) -> (pre, [at_1])
        a -> a
    -- Collect events until one at or after 'end'.
    (within, post2) = Then.span ((<end) . Event.start) (splitAt 1) post1

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
-}
slice_notes :: Bool -- ^ include a note at the end
    -> ScoreTime -> ScoreTime -> TrackTree.EventsTree
    -> [[Note]] -- ^ One [Note] per sub note track, in right to left order.
slice_notes include_end start end tracks
    | null tracks || start > end = []
    | otherwise = filter (not . null) $
        map (mapMaybe strip_note . slice_track) $ concatMap note_tracks tracks
    where
    note_tracks :: TrackTree.EventsNode -> [Sliced]
    note_tracks node@(Tree.Node track subs)
        | is_note track =
            [([], track, event_ranges include_end start end node, subs)]
        | otherwise =
            [ (track : parents, ntrack, slices, nsubs)
            | (parents, ntrack, slices, nsubs) <- concatMap note_tracks subs
            ]
    -- For each note track, slice out each event.
    slice_track :: Sliced -> [Note]
    slice_track (parents, track, slices, subs) =
        map (slice1 (make_tree parents)) (Seq.zip_prev slices)
        where
        make_tree [] = [Tree.Node track subs]
        make_tree (p:ps) = [Tree.Node p (make_tree ps)]
    slice1 tree (prev, (n_start, n_end, n_next)) =
        (n_start, n_end - n_start,
            map (fmap (shift_tree n_start n_next)) $
            slice prev_zero n_start n_end Nothing tree)
        where
        -- exclude_start if 's' is still the original 'start', or if the
        -- previous slice was zero dur and is the same as 'start', which means
        -- it already consumed any event at 'start'.
        prev_zero = case prev of
            Nothing -> False
            Just (s, e, _) -> s == e && s == n_start
    shift_tree shift next track = track
        { TrackTree.track_events =
            Events.map_events move (TrackTree.track_events track)
        , TrackTree.track_end = next - shift
        , TrackTree.track_around =
            let (prev, next) = TrackTree.track_around track
            in (map move prev, map move next)
        , TrackTree.track_shifted = TrackTree.track_shifted track + shift
        }
        where move = Event.move (subtract shift)

-- | (parents, track, 'event_ranges', subs)
type Sliced = ([TrackTree.Track], TrackTree.Track,
    [(ScoreTime, ScoreTime, ScoreTime)], TrackTree.EventsTree)

-- | (start, dur, tracks)
type Note = (ScoreTime, ScoreTime, [TrackTree.EventsNode])

-- | Get slice ranges for a track.  This gets the non-overlapping ranges of all
-- the note tracks events below.
event_ranges :: Bool -> TrackTime -> TrackTime -> TrackTree.EventsNode
    -> [(TrackTime, TrackTime, TrackTime)]
    -- ^ [(start, end, next_start)]
event_ranges include_end start end = nonoverlapping . to_ranges
    where
    to_ranges = Seq.merge_lists (\(s, _, _) -> s) . map track_events
        . filter is_note . Tree.flatten
    track_events = map range . Seq.zip_next . Events.ascending
        . events_in_range include_end start end
        . TrackTree.track_events
    range (event, next) =
        (Event.min event, Event.max event,
            max (Event.max event) (maybe end Event.min next))
    nonoverlapping [] = []
    nonoverlapping (r:rs) = r : nonoverlapping (dropWhile (overlaps r) rs)
    overlaps (s1, e1, _) (s2, e2, _) = not $ e1 <= s2 || e2 <= s1

events_in_range :: Bool -> TrackTime -> TrackTime -> Events.Events
    -> Events.Events
events_in_range include_end start end events
    | start == end = maybe mempty Events.singleton $ Events.at start events
    | include_end =
        maybe within (\e -> Events.insert [e] within) (Events.at end post)
    | otherwise = within
    where (_, within, post) = Events.split_range start end events

strip_note :: Note -> Maybe Note
strip_note (start, dur, tree)
    | null stripped = Nothing
    | otherwise = Just (start, dur, tree)
    where stripped = concatMap strip_empty_tracks tree

-- | If a branch has no note track children with events, there's no way it can
-- produce any events, so it can be dropped before derivation.
--
-- The branch has to have no notes from top to bottom, because any note in the
-- middle could invert below.
strip_empty_tracks :: TrackTree.EventsNode -> [TrackTree.EventsNode]
strip_empty_tracks (Tree.Node track subs)
    | not (is_note track) || track_empty track =
        if null stripped then [] else [Tree.Node track stripped]
    | otherwise = [Tree.Node track subs]
    where stripped = concatMap strip_empty_tracks subs

-- | This is 'slice_notes', but throw an error if 'find_overlapping' complains.
--
-- TODO I think I don't want to allow sub-events larger than their slice, but
-- currently I do.  Actually I think overlap checking needs an overhaul in
-- general.
checked_slice_notes :: Bool -> ScoreTime -> ScoreTime -> TrackTree.EventsTree
    -> Either Text [[Note]]
checked_slice_notes include_end start end tracks = case maybe_err of
    Nothing -> Right $ filter (not . null) notes
    Just err -> Left err
    where
    maybe_err = if start == end
        then check_greater_than 0 check_tracks
        else check_overlapping include_end 0 check_tracks
    notes = slice_notes include_end start end tracks
    -- Only check the first note of each slice.  Since the notes are
    -- increasing, this is the one which might start before the slice.  Since
    -- the events have been shifted back by the slice start, an event that
    -- extends over 0 means it overlaps the beginning of the slice.
    check_tracks = map (\(_, _, subs) -> subs) $ mapMaybe Seq.head notes

check_greater_than :: ScoreTime -> [[TrackTree.EventsNode]] -> Maybe Text
check_greater_than start tracks
    | null events = Nothing
    | otherwise = Just $ "zero duration slice has note events >"
        <> pretty start <> ": " <> Text.intercalate ", " (map pretty events)
    where events = mapMaybe (find_greater_than start) tracks

find_greater_than :: ScoreTime -> [TrackTree.EventsNode] -> Maybe Event.Event
find_greater_than start = msum . map (find (has_gt <=< note_track))
    where
    note_track track
        | is_note track = Just track
        | otherwise = Nothing
    has_gt = List.find ((>start) . Event.start) . Events.ascending
        . TrackTree.track_events

check_overlapping :: Bool -> ScoreTime -> [[TrackTree.EventsNode]]
    -> Maybe Text
check_overlapping include_end start tracks
    | null overlaps = Nothing
    | otherwise = Just $ "slice has overlaps: "
        <> Text.intercalate ", " (map show_overlap overlaps)
    -- TODO 'include_end' is used incorrectly, it becomes 'exclude_start'
    -- need to fix find_overlapping
    where overlaps = mapMaybe (find_overlapping include_end start) tracks

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
find_overlapping :: Bool -> ScoreTime -> [TrackTree.EventsNode]
    -> Maybe (Maybe TrackId, (TrackTime, TrackTime))
find_overlapping exclude_start start = msum . map (find has_overlap)
    where
    -- This relies on the 'strip_empty_tracks' having been called.  The
    -- problem is that a zero duration slice after (0, 0) looks like (0, n).
    -- I want to emit an error if and only if there are events starting at >0
    -- in the (0, n) slice.  Those events will be evaluated twice if the (0, 0)
    -- slice also got them.  But 'find_overlapping' only looks to see if prev
    -- events overlap 0, which means it emits an error for both the case with
    -- no events at >0, and with events >0.  Stripping empty tracks eliminates
    -- the false positive for no events at >0, while leaving the true positive
    -- for events at >0.
    --
    -- This seems pretty obscure and indirect, and I tried to come up with an
    -- algorithm that didn't rely on 'strip_empty_tracks', but failed.
    has_overlap track = case TrackTree.track_around track of
        (prev : _, _) | is_note track && edge prev > start ->
            Just (TrackTree.track_id track,
                (shifted (Event.start prev), shifted (Event.end prev)))
        _ -> Nothing
        where shifted = (+ TrackTree.track_shifted track)
    -- This works but I don't know why.  It's probably wrong, and the whole
    -- overlap checking strategy probably needs a redo, but it's defeated
    -- me for the moment so I'm letting it be.
    edge = if exclude_start then Event.min else Event.max

show_overlap :: (Maybe TrackId, (TrackTime, TrackTime)) -> Text
show_overlap (Nothing, (start, end)) =
    pretty start <> "--" <> pretty end
show_overlap (Just track_id, (start, end)) =
    pretty $ State.Range Nothing track_id start end

-- * orphans

-- | This is a variant of 'slice' used by note track evaluation to derive
-- orphan events.
slice_orphans :: Bool -> ScoreTime -> ScoreTime -> [TrackTree.EventsNode]
    -> Either Text [TrackTree.EventsNode]
slice_orphans exclude_start start end subs =
    maybe (Right slices) Left $ check_overlapping exclude_start start [slices]
    where
    slices = concatMap strip_empty_tracks $
        slice exclude_start start end Nothing subs

-- * util

is_note :: TrackTree.Track -> Bool
is_note = ParseTitle.is_note_track . TrackTree.track_title

track_empty :: TrackTree.Track -> Bool
track_empty = Events.null . TrackTree.track_events

-- | Get the first Just from the structure.
find :: Foldable.Foldable t => (a -> Maybe b) -> t a -> Maybe b
find f = Monoid.getFirst . Foldable.foldMap (Monoid.First . f)
