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
    calls, aka parent calls.  I'd still love to figure out a better way
    to do it though!
-}
module Derive.Slice (
    InsertEvent(..), Track(..), slice
    , checkedSliceNotes
    , sliceOrphans
#ifdef TESTING
    , stripEmptyTracks
    , sliceNotes
#endif
) where
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Util.Then as Then

import qualified Derive.ParseTitle as ParseTitle
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | Ask 'slice' to synthesize a note track and insert it at the leaves of
-- the sliced tree.
data InsertEvent = InsertEvent
    { eventDuration :: !ScoreTime
    -- | A Negative orientation means that the controls at the Event.end time
    -- are not trimmed off.
    , eventOrientation :: !Types.Orientation
    , eventAround :: !([Event.Event], [Event.Event])
    -- | The TrackId for the track created for this event.  This is required
    -- so it can collect a TrackDynamic and when the Cmd level looks at at
    -- track with inverted note calls, it sees the environ established by the
    -- tracks that the calls are inverted beneath.  E.g., if the pitch track
    -- sets a scale, the Cmd layer should see the note track as having that
    -- scale.
    , eventTrackId :: !(Maybe TrackId)
    } deriving (Show)

{- | Slice a track between start and end, and optionally put a note track with
    a single event of given string at the bottom.  Sliced control tracks
    usually get events beyond the slice boundaries for context.
-}
slice :: Bool -- ^ Omit events than begin at the start.
    -- 'slice_notes' documents why this is necessary.
    -> ScoreTime -> ScoreTime -> Maybe InsertEvent
    -- ^ If given, insert an event at the bottom with the given text and dur.
    -- The created track will have the given track_range, so it can create
    -- a Stack.Region entry.
    -> TrackTree.EventsNode -> TrackTree.EventsNode
slice excludeStart start end insertEvent (Tree.Node track subs) =
    Tree.Node (sliceT track) $ if null subs
        then insert (TrackTree.trackShifted track)
            (TrackTree.trackBlockId track)
        else map (slice excludeStart start end insertEvent) subs
    where
    insert shift blockId = case insertEvent of
        Nothing -> []
        Just insertEvent -> [Tree.Node (make shift blockId insertEvent) []]
    -- The synthesized bottom track.  Since slicing only happens within
    -- a block, I assume the BlockId is the same as the parent.  I need
    -- a BlockId to look up the previous val in 'Derive.Threaded'.
    make shift blockId (InsertEvent dur _ around trackId) = TrackTree.Track
        { trackTitle = ">"
        , trackEvents = Events.singleton (Event.event start dur "")
        , trackId = trackId
        , trackBlockId = blockId
        , trackStart = start
        , trackEnd = end
        , trackSliced = TrackTree.Inversion
        , trackAround = around
        -- Since a note may be inverted and inserted after 'slice_notes'
        -- and its shifting, I have to get the shift from the parent track.
        , trackShifted = shift
        , trackVoice = Nothing
        }
    sliceT track = track
        { TrackTree.trackEvents = within
        , TrackTree.trackStart = start
        , TrackTree.trackEnd = end
        , TrackTree.trackSliced = case TrackTree.trackSliced track of
            TrackTree.Inversion -> TrackTree.Inversion
            -- This might already be Sliced Positive because of orphan
            -- slicing, so make sure to update the orientation for both
            -- Sliced and NotSliced.
            _ -> TrackTree.Sliced
                (maybe Types.Positive eventOrientation insertEvent)
        , TrackTree.trackAround = (before, after)
        }
        where (before, within, after) = extractEvents track
    -- Extract events from an intermediate track.
    extractEvents track
        | ParseTitle.isNoteTrack title =
            extractNoteEvents excludeStart start end events
        | otherwise = extractControlEvents start end events
        where
        events = TrackTree.trackEvents track
        title = TrackTree.trackTitle track

-- | Note tracks don't include pre and post events like control tracks.
extractNoteEvents :: Bool -> ScoreTime -> ScoreTime
    -> Events.Events -> ([Event.Event], Events.Events, [Event.Event])
extractNoteEvents excludeStart start end events =
    (if excludeStart then excludeS else id)
        (Events.descending pre, within, Events.ascending post)
    where
    -- TODO pass Events.Range instead of (start, end) so I can get -0 slices
    -- right.
    range
        | start == end = Events.Point start Types.Positive
        | otherwise = Events.Range start end
    (pre, within, post) = Events.splitRange range events
    excludeS (pre, within, post) =
        case Events.at start Types.Positive within of
            Just event ->
                ( event : pre
                , Events.remove (Events.Point start Types.Positive) within
                , post
                )
            Nothing -> (pre, within, post)

extractControlEvents :: ScoreTime -> ScoreTime
    -> Events.Events -> ([Event.Event], Events.Events, [Event.Event])
    -- ^ (descending_pre, within, ascending_post)
extractControlEvents start end events = (pre, Events.fromList within, post2)
    where
    (pre, post1) = case Events.splitLists start events of
        (at1:pre, at:post) | Event.start at > start -> (pre, at1:at:post)
        (at1:pre, []) -> (pre, [at1])
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
sliceNotes :: Bool -- ^ include a note at the end
    -> ScoreTime -> ScoreTime -> TrackTree.EventsTree
    -> [Track] -- ^ One Track per non-empty sub note track, in TrackNum order.
sliceNotes includeEnd start end tracks
    | null tracks || start > end = []
    | otherwise = map (uncurry Track) $ filter (not . null . snd) $
        map (second (mapMaybe stripNote) . sliceTrack) $
        concatMap noteTracks tracks
    where
    noteTracks :: TrackTree.EventsNode -> [Sliced]
    noteTracks node@(Tree.Node track subs)
        | isNote track =
            [([], track, eventRanges includeEnd start end node, subs)]
        | otherwise =
            [ (track : parents, ntrack, slices, nsubs)
            | (parents, ntrack, slices, nsubs) <- concatMap noteTracks subs
            ]
    -- For each note track, slice out each event.
    sliceTrack :: Sliced -> (Maybe TrackId, [Note])
    sliceTrack (parents, noteTrack, slices, subs) =
        ( TrackTree.trackId noteTrack
        , map (slice1 (makeTree parents)) (Lists.zipPrev slices)
        )
        where
        makeTree (p:ps) = [Tree.Node p (makeTree ps)]
        makeTree [] = [Tree.Node noteTrack subs]
    slice1 tree (prev, (nStart, nEnd, nNext)) =
        ( nStart
        , nEnd - nStart
        , map (fmap (shiftTree nStart nNext)) $
            map (slice prevZero nStart nEnd Nothing) tree
        )
        where
        -- exclude_start if 's' is still the original 'start', or if the
        -- previous slice was zero dur and is the same as 'start', which means
        -- it already consumed any event at 'start'.
        prevZero = case prev of
            Nothing -> False
            Just (s, e, _) -> s == e && s == nStart
    shiftTree shift next track = track
        { TrackTree.trackEvents =
            Events.mapEvents move (TrackTree.trackEvents track)
        , TrackTree.trackStart = TrackTree.trackStart track - shift
        , TrackTree.trackEnd = next - shift
        , TrackTree.trackAround =
            let (prev, next) = TrackTree.trackAround track
            in (map move prev, map move next)
        , TrackTree.trackShifted = TrackTree.trackShifted track + shift
        }
        where move = Event.start_ %= subtract shift

-- | (parents, note_track, 'event_ranges', subs)
type Sliced =
    ( [TrackTree.Track]
    , TrackTree.Track
    , [(ScoreTime, ScoreTime, ScoreTime)]
    , TrackTree.EventsTree
    )

-- | (start, dur, tracks)
type Note = (ScoreTime, ScoreTime, [TrackTree.EventsNode])

data Track = Track
    { _trackId :: Maybe TrackId
    , _notes :: [Note]
    } deriving (Show)

-- | Get slice ranges for a track.  This gets the non-overlapping ranges of all
-- the note tracks events below.
eventRanges :: Bool -> TrackTime -> TrackTime -> TrackTree.EventsNode
    -> [(TrackTime, TrackTime, TrackTime)]
    -- ^ [(start, end, next_start)]
eventRanges includeEnd start end = nonoverlapping . toRanges
    where
    toRanges = Lists.mergeLists (\(s, _, _) -> s) . map trackEvents
        . filter isNote . Tree.flatten
    trackEvents = map range . Lists.zipNext . Events.ascending
        . eventsInRange includeEnd start end
        . TrackTree.trackEvents
    range (event, next) =
        ( Event.min event, Event.max event
        , max (Event.max event) (maybe end Event.min next)
        )
    nonoverlapping [] = []
    nonoverlapping (r:rs) = r : nonoverlapping (dropWhile (overlaps r) rs)
    overlaps (s1, e1, _) (s2, e2, _) = not $ e1 <= s2 || e2 <= s1

-- TODO maybe I can remove include_end if I require the next event to be
-- negative.
eventsInRange :: Bool -> TrackTime -> TrackTime -> Events.Events
    -> Events.Events
eventsInRange includeEnd start end events
    | includeEnd = maybe within (\e -> Events.insert [e] within)
        (Events.at end Types.Positive post)
    | otherwise = within
    where
    (_, within, post) = Events.splitRange range events
    -- TODO since this is Positive I think it doesn't treat -0 events
    -- correctly.  I could pass Events.Range from the caller.
    range = if start == end then Events.Point start Types.Positive
        else Events.Range start end

-- | Remove empty tracks from the Note tree, and the entire Note if it was all
-- empty tracks.
stripNote :: Note -> Maybe Note
stripNote (start, dur, tree)
    | null stripped = Nothing
    | otherwise = Just (start, dur, tree)
    where stripped = concatMap stripEmptyTracks tree

-- | If a branch has no note track children with events, there's no way it can
-- produce any events, so it can be dropped before derivation.
--
-- The branch has to have no notes from top to bottom, because any note in the
-- middle could invert below.
stripEmptyTracks :: TrackTree.EventsNode -> [TrackTree.EventsNode]
stripEmptyTracks (Tree.Node track subs)
    | not (isNote track) || trackEmpty track =
        if null stripped then [] else [Tree.Node track stripped]
    | otherwise = [Tree.Node track subs]
    where stripped = concatMap stripEmptyTracks subs

-- | This is 'slice_notes', but throw an error if 'find_overlapping' complains.
--
-- TODO I think I don't want to allow sub-events larger than their slice, but
-- currently I do.  Actually I think overlap checking needs an overhaul in
-- general.
checkedSliceNotes :: Bool -- ^ TODO change this to Event.Orientation?
    -> ScoreTime -> ScoreTime -> TrackTree.EventsTree -> Either Text [Track]
checkedSliceNotes includeEnd start end tree = case maybeErr of
    Nothing -> Right tracks
    Just err -> Left err
    where
    maybeErr = if start == end
        then checkGreaterThan 0 checkTracks
        else checkOverlapping includeEnd 0 checkTracks
    tracks = sliceNotes includeEnd start end tree
    -- Only check the first note of each slice.  Since the notes are
    -- increasing, this is the one which might start before the slice.  Since
    -- the events have been shifted back by the slice start, an event that
    -- extends over 0 means it overlaps the beginning of the slice.
    checkTracks = map (\(_, _, subs) -> subs) $
        mapMaybe (Lists.head . _notes) tracks

checkGreaterThan :: ScoreTime -> [[TrackTree.EventsNode]] -> Maybe Text
checkGreaterThan start tracks
    | null events = Nothing
    | otherwise = Just $ "zero duration slice has note events >"
        <> pretty start <> ": " <> Text.intercalate ", " (map pretty events)
    where events = mapMaybe (findGreaterThan start) tracks

findGreaterThan :: ScoreTime -> [TrackTree.EventsNode] -> Maybe Event.Event
findGreaterThan start = msum . map (find (hasGt <=< noteTrack))
    where
    noteTrack track
        | isNote track = Just track
        | otherwise = Nothing
    hasGt = List.find ((>start) . Event.start) . Events.ascending
        . TrackTree.trackEvents

checkOverlapping :: Bool -> ScoreTime -> [[TrackTree.EventsNode]]
    -> Maybe Text
checkOverlapping includeEnd start tracks
    | null overlaps = Nothing
    | otherwise = Just $ "slice has overlaps: "
        <> Text.intercalate ", " (map showOverlap overlaps)
    -- TODO 'include_end' is used incorrectly, it becomes 'exclude_start'
    -- need to fix find_overlapping
    where overlaps = mapMaybe (findOverlapping includeEnd start) tracks

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
    zero-duration note parents that take non-zero-duration sub-events.

    This check also requires me to delay stripping out empty tracks until after
    the check has been done, because otherwise @b@ wouldn't notice that @c@
    overlaps.
-}
findOverlapping :: Bool -> ScoreTime -> [TrackTree.EventsNode]
    -> Maybe (Maybe TrackId, (TrackTime, TrackTime))
findOverlapping excludeStart start = msum . map (find hasOverlap)
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
    hasOverlap track = case TrackTree.trackAround track of
        (prev : _, _) | isNote track && edge prev > start ->
            Just (TrackTree.trackId track,
                (shifted (Event.start prev), shifted (Event.end prev)))
        _ -> Nothing
        where shifted = (+ TrackTree.trackShifted track)
    -- This works but I don't know why.  It's probably wrong, and the whole
    -- overlap checking strategy probably needs a redo, but it's defeated
    -- me for the moment so I'm letting it be.
    edge = if excludeStart then Event.min else Event.max

showOverlap :: (Maybe TrackId, (TrackTime, TrackTime)) -> Text
showOverlap (Nothing, (start, end)) =
    pretty start <> "--" <> pretty end
showOverlap (Just trackId, (start, end)) =
    pretty $ Ui.Range Nothing trackId start end

-- * orphans

-- | This is a variant of 'slice' used by note track evaluation to derive
-- orphan events.
sliceOrphans :: Bool -> ScoreTime -> ScoreTime -> [TrackTree.EventsNode]
    -> Either Text [TrackTree.EventsNode]
sliceOrphans excludeStart start end subs =
    maybe (Right slices) Left $ checkOverlapping excludeStart start [slices]
    where
    slices = concatMap stripEmptyTracks $
        map (slice excludeStart start end Nothing) subs

-- * util

isNote :: TrackTree.Track -> Bool
isNote = ParseTitle.isNoteTrack . TrackTree.trackTitle

trackEmpty :: TrackTree.Track -> Bool
trackEmpty = Events.null . TrackTree.trackEvents

-- | Get the first Just from the structure.
find :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
find f = Monoid.getFirst . foldMap (Monoid.First . f)
