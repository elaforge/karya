-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is the basic interface for slicing.  This also includes 'inverting',
-- which is a special case of slicing.
module Derive.Call.Sub (
    -- * inversion
    when_under_inversion, unless_under_inversion
    , inverting, inverting_args, inverting_around
    , invert_call
    -- ** events
    , Event(..), event_end, event_overlaps, map_event, map_events
    , stretch
    , sub_events
    , place, place_at
    -- * reapply
    , reapply, reapply_call
) where
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Slice as Slice
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import Types


-- * inversion

-- | Apply the function (likely a transformer) only when at the bottom of
-- the inversion.  This is useful for transforming an inverted call because
-- otherwise the transformation happens twice: once before inversion and once
-- after.
--
-- Track calls avoid this because they skip applying transformers if
-- 'Derive.info_inverted' is true, so transformers applied directly in haskell
-- need to emulate that.
when_under_inversion :: Derive.PassedArgs d -> (a -> a) -> a -> a
when_under_inversion args transform deriver
    | under_inversion args = transform deriver
    | otherwise = deriver

unless_under_inversion :: Derive.PassedArgs d -> (a -> a) -> a -> a
unless_under_inversion args transform deriver
    | under_inversion args = deriver
    | otherwise = transform deriver

-- | True if the call is under an inversion, which means it's the second time
-- it has been evaluated.
under_inversion :: Derive.PassedArgs d -> Bool
under_inversion = Derive.info_inverted . Derive.passed_info

-- | Convert a call into an inverting call.  Documented in doc/inverting_calls.
--
-- This requires a bit of hackery:
--
-- This requires getting the expression being evaluated so it can be inserted
-- into the inverted track, which is what 'Derive.info_expr' is for.
inverting :: (Derive.PassedArgs d -> Derive.NoteDeriver)
    -> (Derive.PassedArgs d -> Derive.NoteDeriver)
inverting = inverting_around (1, 1)

inverting_args :: Derive.PassedArgs d -> Derive.NoteDeriver
    -> Derive.NoteDeriver
inverting_args args f = inverting (const f) args

inverting_around :: (Int, Int) -- ^ Capture this many control points at+before
    -- and after the slice boundary.  Also documented in 'Slice.slice'.
    -> (Derive.PassedArgs d -> Derive.NoteDeriver)
    -> (Derive.PassedArgs d -> Derive.NoteDeriver)
inverting_around around call args =
    -- If I can invert, the call isn't actually called.  Instead I make a track
    -- with event text that will result in this being called again, and at
    -- that point it actually will be called.
    maybe (call args) BlockUtil.derive_tracks =<< invert_call around args

invert_call :: (Int, Int)
    -> Derive.PassedArgs d -> Derive.Deriver (Maybe TrackTree.EventsTree)
invert_call around args = case Derive.info_sub_tracks info of
    [] -> return Nothing
    subs -> Just <$> invert around subs (Event.start event) (Event.end event)
        (Args.next args) (Derive.info_expr info)
        (Derive.info_prev_events info, Derive.info_next_events info)
    where
    event = Derive.info_event info
    info = Derive.passed_info args

invert :: (Int, Int) -> TrackTree.EventsTree
    -> ScoreTime -> ScoreTime -> ScoreTime -> Event.Text
    -> ([Event.Event], [Event.Event])
    -> Derive.Deriver TrackTree.EventsTree
invert around subs start end next_start text events_around = do
    -- Pick the current TrackId out of the stack, and give that to the track
    -- created by inversion.
    -- TODO I'm not 100% comfortable with this, I don't like putting implicit
    -- dependencies on the stack like this.  Too many of these and someday
    -- I change how the stack works and all sorts of things break.  It would be
    -- more explicit to put TrackId into CallInfo.
    track_id <- stack_track_id
    let sliced = slice track_id
    whenJust (non_bottom_note_track sliced) $ \track -> Derive.throw $
        "inverting below a note track will lead to an endless loop: "
        <> pretty (TrackTree.track_id track)
    return sliced
    where
    slice track_id =
        Slice.slice False around start next_start (Just (insert track_id)) subs
    -- Use 'next_start' instead of track_end because in the absence of a next
    -- note, the track end becomes next note and clips controls.
    insert track_id = Slice.InsertEvent
        { Slice.ins_text = text
        , Slice.ins_duration = end - start
        , Slice.ins_around = events_around
        , Slice.ins_track_id = track_id
        }

stack_track_id :: Derive.Deriver (Maybe TrackId)
stack_track_id = Seq.head . mapMaybe Stack.track_of . Stack.innermost
    <$> Internal.get_stack

-- | An inverting call above another note track will lead to an infinite loop
-- if there are overlapping sub-events that also invert, or confusing results
-- if there are non-overlapping or non-inverting sub-events.  Either way, I
-- don't think I want it.
--
-- An exception is if the note track is empty, since I can be sure there are
-- no inverting calls in that case.
non_bottom_note_track :: TrackTree.EventsTree -> Maybe TrackTree.Track
non_bottom_note_track tree = Seq.head (concatMap go tree)
    where
    go (Tree.Node track subs)
        | ParseTitle.is_note_track (TrackTree.track_title track)
            && not (Events.null (TrackTree.track_events track))
            && not (null subs) = [track]
        | otherwise = concatMap go subs

-- ** note slice

-- | Sliced sub-events are represented as a start, duration, and opaque
-- deriver.  This is a compromise between a plain NoteDeriver, which is fully
-- abstract but also fully opaque, and some kind of note data structure, which
-- is fully concrete (and thus inflexible), but also transparent to
-- modification.
data Event = Event {
    event_start :: !ScoreTime
    , event_duration :: !ScoreTime
    , event_deriver :: !Derive.NoteDeriver
    }

event_end :: Event -> ScoreTime
event_end event = event_start event + event_duration event

event_overlaps :: ScoreTime -> Event -> Bool
event_overlaps pos (Event start dur _)
    | dur == 0 = pos == start
    | otherwise = start <= pos && pos < start + dur

map_event :: (Derive.NoteDeriver -> Derive.NoteDeriver) -> Event -> Event
map_event f event = event { event_deriver = f (event_deriver event) }

map_events :: (Derive.NoteDeriver -> Derive.NoteDeriver) -> [Event] -> [Event]
map_events f = map (map_event f)

stretch :: ScoreTime -> ScoreTime -> Event -> Event
stretch offset factor (Event start dur deriver) =
    Event ((start - offset) * factor + offset) (dur * factor) deriver

instance Show Event where
    show (Event start dur _) = "Event " ++ show start ++ " " ++ show dur
instance Pretty.Pretty Event where pretty = show

-- | Get the Events of subtracks, if any, returning one list of events per sub
-- note track.  This is the top-level utility for note calls that take other
-- note calls as arguments.
sub_events :: Derive.PassedArgs d -> Derive.Deriver [[Event]]
sub_events args = case Derive.info_sub_events (Derive.passed_info args) of
    Nothing -> either Derive.throw (return . (map (map mkevent))) $
        Slice.checked_slice_notes False start end subs
    Just events -> return $ map (map (\(s, d, n) -> Event s d n)) events
    where
    (start, end) = Args.range args
    subs = Derive.info_sub_tracks (Derive.passed_info args)
    -- The events have been shifted back to 0 by 'Slice.slice_notes', but
    -- are still their original lengths.  Stretch them back to 1 so Events
    -- are normalized.
    mkevent (shift, stretch, tree) = Event
        { event_start = shift
        , event_duration = stretch
        , event_deriver = Derive.stretch
            (if stretch == 0 then 1 else recip stretch)
            (BlockUtil.derive_tracks tree)
        }

-- | Place and merge a list of Events.
place :: [Event] -> Derive.NoteDeriver
place = mconcatMap (\(Event s d n) -> Derive.place s d n)

-- | Fit the given events into a time range.  Any leading space (time between
-- the start of the range and the first Event) and trailing space is
-- eliminated.
place_at :: (ScoreTime, ScoreTime) -> [Event] -> Derive.NoteDeriver
place_at (start, end) notes = Derive.place start factor $
    place [note { event_start = event_start note - note_start } | note <- notes]
    where
    factor = (end - start) / (note_end - note_start)
    note_end = fromMaybe 1 $ Seq.maximum (map event_end notes)
    note_start = fromMaybe 1 $ Seq.minimum (map event_start notes)

-- * reapply

-- | Call a note transformer with sub-events.  While you can easily call other
-- kinds of calls with 'Call.reapply', note transformers are more tricky
-- because they expect a track structure in 'Derive.info_sub_tracks'.  This
-- bypasses that and directly passes 'Event's to the note transformer, courtesy
-- of 'Derive.info_sub_events'.
reapply :: Derive.NoteArgs -> TrackLang.Expr -> [[Event]] -> Derive.NoteDeriver
reapply args expr notes = Call.reapply subs expr
    where
    subs = args
        { Derive.passed_info = (Derive.passed_info args)
            { Derive.info_sub_events =
                Just $ map (map (\(Event s d n) -> (s, d, n))) notes
            }
        }

reapply_call :: Derive.NoteArgs -> TrackLang.CallId
    -> [TrackLang.Term] -> [[Event]] -> Derive.NoteDeriver
reapply_call args call_id call_args =
    reapply args (TrackLang.call call_id call_args :| [])
