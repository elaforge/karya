-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | This is the basic interface for slicing.  This also includes 'inverting',
-- which is a special case of slicing.
module Derive.Call.Sub (
    -- * inversion
    under_invert
    , inverting, inverting_args
    -- ** events
    , Event, GenericEvent(..), event_end, event_overlaps
    , stretch
    , sub_events
    , place, fit_to_range
    -- ** RestEvent
    , RestEvent, sub_rest_events
    -- * reapply
    , reapply, reapply_call
) where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Args as Args
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Slice as Slice
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import Global
import Types


-- * inversion

{- | Cause this transformer to apply only after inversion.
    'Derive.Call.Tags.under_invert' documents this, also see
    NOTE [under-invert].

    Normally when a call is inverted, the transformers run outside the
    inversion, while only the generator runs underneath.  However, some
    transformers rely on per-note controls, such as pitch and dyn, and
    therefore need to go under the invert.  So this saves the transformer, and
    applies it only after all the inversion has happened.

    If there are no sub-tracks, then inversion won't happen, and the transform
    is run right here.  However, if there are sub-tracks, but the generator
    doesn't want to run, then the transform will be lost.

    TODO I could probably fix it by making Eval.eval_generator apply the
    transform, but it would have to clear it out too to avoid evaluating more
    than once.  Not sure which way is right.
-}
under_invert :: (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver
under_invert transformer args deriver
    | null $ Derive.info_sub_tracks $ Derive.passed_info args =
        transformer args deriver
    | otherwise = with deriver
    where
    with = Internal.local $ \state -> state
        { Derive.state_under_invert =
            Derive.state_under_invert state . transformer args
        }

-- | Convert a call into an inverting call.  Documented in doc/inverting_calls.
run_invert :: Derive.Taggable d => Derive.PassedArgs d -> Derive.NoteDeriver
    -> Derive.NoteDeriver
run_invert args call = do
    dyn <- Internal.get_dynamic id
    case (Derive.state_inversion dyn, Derive.info_sub_tracks cinfo) of
        (Derive.InversionInProgress {}, _) ->
            Derive.throw "tried to invert while inverting"
        (Derive.NotInverted, subs@(_:_)) -> do
            sliced <- invert subs
                (Event.start event) (Event.end event) (Args.next args)
                (Derive.info_prev_events cinfo, Derive.info_next_events cinfo)
            with_inversion $ BlockUtil.derive_tracks sliced
        (Derive.NotInverted, []) -> call
    where
    with_inversion = Internal.local $ \dyn -> dyn
        { Derive.state_inversion = Derive.InversionInProgress call }
    event = Derive.info_event cinfo
    cinfo = Derive.passed_info args

-- | Convert a call into an inverting call.  This is designed to be convenient
-- to insert after the signature arg in a call definition.  The args passed
-- to the call have been stripped of their sub tracks to avoid another
-- inversion.
inverting :: Derive.Taggable d => (Derive.PassedArgs d -> Derive.NoteDeriver)
    -> Derive.PassedArgs d -> Derive.NoteDeriver
inverting call args = run_invert args (call stripped)
    where
    stripped = args
        { Derive.passed_info = (Derive.passed_info args)
            { Derive.info_sub_tracks = mempty
            , Derive.info_sub_events = Nothing
            }
        }

-- | 'inverting' with its arguments flipped.  This is useful for calls that
-- want to do stuff with the args before inverting.  Make sure to shadow the
-- old 'Derive.PassedArgs' with the ones passed to the call, for the reason
-- documented in 'inverting'.
inverting_args :: Derive.Taggable d => Derive.PassedArgs d
    -> (Derive.PassedArgs d -> Derive.NoteDeriver) -> Derive.NoteDeriver
inverting_args = flip inverting

-- When I invert, I call derive_tracks again, which means the inverted bottom
-- is going to expect to see the current prev val.  TODO but evidently I don't
-- need this?  Try to make a problem without it.
save_prev_val :: Derive.Taggable a => Derive.PassedArgs a -> Derive.Deriver ()
save_prev_val args = case Args.prev_val args of
    Nothing -> return ()
    Just val -> Stack.block_track_of <$> Internal.get_stack >>= \x -> case x of
        Nothing -> return ()
        Just block_track -> modify_threaded $ \th -> th
            { Derive.state_prev_val = Map.insert block_track
                (Derive.to_tagged val) (Derive.state_prev_val th)
            }
    where
    modify_threaded modify = Derive.modify $
        \st -> st { Derive.state_threaded = modify (Derive.state_threaded st) }

invert :: TrackTree.EventsTree -> ScoreTime -> ScoreTime -> ScoreTime
    -> ([Event.Event], [Event.Event])
    -> Derive.Deriver TrackTree.EventsTree
invert subs start end next_start events_around = do
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
        Slice.slice False start next_start (Just (insert track_id)) subs
    -- Use 'next_start' instead of track_end because in the absence of a next
    -- note, the track end becomes next note and clips controls.
    insert track_id = Slice.InsertEvent
        { Slice.ins_duration = end - start
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
type Event = GenericEvent Derive.NoteDeriver

data GenericEvent a = Event {
    event_start :: !ScoreTime
    , event_duration :: !ScoreTime
    , event_note :: !a
    } deriving (Show, Functor)

event_end :: GenericEvent a -> ScoreTime
event_end event = event_start event + event_duration event

event_overlaps :: ScoreTime -> GenericEvent a -> Bool
event_overlaps pos (Event start dur _)
    | dur == 0 = pos == start
    | otherwise = start <= pos && pos < start + dur

stretch :: ScoreTime -> ScoreTime -> GenericEvent a -> GenericEvent a
stretch offset factor (Event start dur note) =
    Event ((start - offset) * factor + offset) (dur * factor) note

instance Pretty.Pretty a => Pretty.Pretty (GenericEvent a) where
    pretty (Event start dur note) =
        "Event " <> show start <> " " <> show dur <> " (" <> pretty note <> ")"

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
        , event_note = Derive.stretch
            (if stretch == 0 then 1 else recip stretch)
            (BlockUtil.derive_tracks tree)
        }

-- | Place and merge a list of Events.
place :: [Event] -> Derive.NoteDeriver
place = mconcatMap (\(Event s d n) -> Derive.place s d n)

-- | Fit the given events into a time range.  Any leading space (time between
-- the start of the range and the first Event) and trailing space is
-- eliminated.
fit_to_range :: (ScoreTime, ScoreTime) -> [Event] -> Derive.NoteDeriver
fit_to_range (start, end) notes = Derive.place start factor $
    place [note { event_start = event_start note - note_start } | note <- notes]
    where
    factor = (end - start) / (note_end - note_start)
    note_end = fromMaybe 1 $ Seq.maximum (map event_end notes)
    note_start = fromMaybe 1 $ Seq.minimum (map event_start notes)

-- ** RestEvent

-- | A Nothing represents a rest.
type RestEvent = GenericEvent (Maybe Derive.NoteDeriver)

-- | This is like 'sub_events', but gaps between the events are returned as
-- explicit rests.
sub_rest_events :: Derive.PassedArgs d -> Derive.Deriver [[RestEvent]]
sub_rest_events args =
    map (uncurry find_gaps (Args.range args)) <$> sub_events args

find_gaps :: ScoreTime -> ScoreTime -> [GenericEvent a]
    -> [GenericEvent (Maybe a)]
find_gaps start end (event : events)
    | gap > 0 = Event start gap Nothing : rest
    | otherwise = rest
    where
    gap = event_start event - start
    rest = (Just <$> event) : find_gaps (event_end event) end events
find_gaps start end []
    | start < end = [Event start (end-start) Nothing]
    | otherwise = []

-- * reapply

-- | Call a note transformer with sub-events.  While you can easily call other
-- kinds of calls with 'Eval.reapply', note transformers are more tricky
-- because they expect a track structure in 'Derive.info_sub_tracks'.  This
-- bypasses that and directly passes 'Event's to the note transformer, courtesy
-- of 'Derive.info_sub_events'.
reapply :: Derive.CallInfo Score.Event -> TrackLang.Expr -> [[Event]]
    -> Derive.NoteDeriver
reapply cinfo expr notes = Eval.reapply subs expr
    where
    subs = cinfo
        { Derive.info_sub_events =
            Just $ map (map (\(Event s d n) -> (s, d, n))) notes
        }

reapply_call :: Derive.CallInfo Score.Event -> TrackLang.CallId
    -> [TrackLang.Term] -> [[Event]] -> Derive.NoteDeriver
reapply_call cinfo call_id call_args =
    reapply cinfo (TrackLang.call call_id call_args :| [])

{- NOTE [under-invert]
    . To make lift to an absolute pitch work outside of inversion, I'd need
      an abstract way (e.g. like a transpose signal) to say "pitch midway to
      (4c)"
    . It's better to have the lift under the pitch.  The only reason it isn't
      is that inversion assumes all transformers go above.  So either make it
      a generator (at which point it can't compose), or have some way to put
      transformers under the inversion, e.g. 'delay | Drop $ lift $ gen' under
      inversion is 'delay' -> 'Drop' 'lift' 'gen'.
    . Another way would be to put that in the call itself, so 'lift' has a flag
      that says it likes to be under the inversion.  Then the invert function
      has to go look all those up.  But that can't work, because invert is
      called by a generator, and that's too late.
    . So call all the transformers pre and post invert.  Normally they check
      if they're under inversion, and if so do nothing, but ones that would
      rather be inverted do the inverse.

    Cons:
      1. Instead of transformers always happening before inversion, they can
      now vary internally, which is one more subtle thing about inversion.
      I'll need to expose it in documentation at least, via a tag.

      2. Call stacks get even messier in the presence of inversion, since
      every transformer appears twice.

      3. Transformers can have their order change, e.g. given
      'below | above | gen', below is actually called below above, if it
      wants to be under inversion.

    . It seems like I could improve these by driving them from a tag.  E.g.
      if the call has a under-inversion tag, Call.eval_transformers will skip
      or not skip, as appropriate.  This solves #1 and #2, but not #3.

    . This is all just to get lift working under inversion.  Is it that
      important?
      . Everything should work under inversion.  It's a hassle to suddenly
        have to rearrange the pitch track, and now 'd' doesn't work.
      . This will come up for every note transformer that wants to know the
        pitch.
-}
