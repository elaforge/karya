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
    , place, stretch, at
    , sub_events, sub_events_negative
    , assert_no_subs
    , modify_notes
    , derive_subs, derive, derive_tracks, derive_pitch, fit
    -- ** RestEvent
    , RestEvent, sub_rest_events
    , fit_rests, strip_rests
    -- * reapply
    , reapply, reapply_call
) where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Slice as Slice
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.TrackTree as TrackTree

import           Global
import           Types


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
    | null $ Derive.ctx_sub_tracks $ Derive.passed_ctx args =
        transformer args deriver
    | otherwise = with deriver
    where
    with = Internal.local $ \state -> state
        { Derive.state_under_invert =
            Derive.state_under_invert state . transformer args
        }

-- | Convert a call into an inverting call.  Documented in
-- @doc/slicing-inverting.md@.
run_invert :: Derive.PassedArgs d -> Derive.NoteDeriver -> Derive.NoteDeriver
run_invert args call = do
    dyn <- Internal.get_dynamic id
    case (Derive.state_inversion dyn, Derive.ctx_sub_tracks ctx) of
        (Derive.InversionInProgress {}, _) ->
            Derive.throw "tried to invert while inverting"
        (Derive.NotInverted, subs@(_:_)) -> do
            sliced <- invert subs event (Args.next args)
                (Derive.ctx_prev_events ctx, Derive.ctx_next_events ctx)
            with_inversion $ BlockUtil.derive_tracks sliced
        (Derive.NotInverted, []) -> call
    where
    with_inversion = Internal.local $ \dyn -> dyn
        { Derive.state_inversion = Derive.InversionInProgress call }
    event = Derive.ctx_event ctx
    ctx = Derive.passed_ctx args

-- | Convert a call into an inverting call.  This is designed to be convenient
-- to insert after the signature arg in a call definition.  The args passed
-- to the call have been stripped of their sub tracks to avoid another
-- inversion.
inverting :: (Derive.PassedArgs d -> Derive.NoteDeriver) -> Derive.PassedArgs d
    -> Derive.NoteDeriver
inverting call args = run_invert args (call stripped)
    where
    stripped = args
        { Derive.passed_ctx = (Derive.passed_ctx args)
            { Derive.ctx_sub_tracks = mempty
            , Derive.ctx_sub_events = Nothing
            }
        }

-- | 'inverting' with its arguments flipped.  This is useful for calls that
-- want to do stuff with the args before inverting.  Make sure to shadow the
-- old 'Derive.PassedArgs' with the ones passed to the call, for the reason
-- documented in 'inverting'.
inverting_args :: Derive.PassedArgs d
    -> (Derive.PassedArgs d -> Derive.NoteDeriver) -> Derive.NoteDeriver
inverting_args = flip inverting

-- When I invert, I call derive_tracks again, which means the inverted bottom
-- is going to expect to see the current prev val.  TODO but evidently I don't
-- need this?  Try to make a problem without it.
save_prev_val :: Derive.Taggable a => Derive.PassedArgs a -> Derive.Deriver ()
save_prev_val args = case Args.prev_val args of
    Nothing -> return ()
    Just val -> Stack.block_track_of <$> Internal.get_stack >>= \case
        Nothing -> return ()
        Just block_track -> modify_threaded $ \th -> th
            { Derive.state_prev_val = Map.insert block_track
                (Derive.to_tagged val) (Derive.state_prev_val th)
            }
    where
    modify_threaded modify = Derive.modify $
        \st -> st { Derive.state_threaded = modify (Derive.state_threaded st) }

invert :: TrackTree.EventsTree -> Event.Event -> ScoreTime
    -> ([Event.Event], [Event.Event]) -> Derive.Deriver TrackTree.EventsTree
invert subs event next_start events_around = do
    -- Pick the current TrackId out of the stack, and give that to the track
    -- created by inversion.
    -- TODO I'm not 100% comfortable with this, I don't like putting implicit
    -- dependencies on the stack like this.  Too many of these and someday
    -- I change how the stack works and all sorts of things break.  It would be
    -- more explicit to put TrackId into Context.
    track_id <- stack_track_id
    let sliced = slice track_id
    whenJust (non_bottom_note_track sliced) $ \track -> Derive.throw $
        "inverting below a note track will lead to an endless loop: "
        <> pretty (TrackTree.track_id track)
    return sliced
    where
    slice track_id =
        map (Slice.slice False (Event.start event) next_start
            (Just (insert track_id))) subs
    -- Use 'next_start' instead of track_end because in the absence of a next
    -- note, the track end becomes next note and clips controls.
    insert track_id = Slice.InsertEvent
        { event_duration = Event.duration event
        , event_orientation = Event.orientation event
        , event_around = events_around
        , event_track_id = track_id
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

place :: ScoreTime -> ScoreTime -> GenericEvent a -> GenericEvent a
place shift factor (Event start dur note) =
    Event ((start - shift) * factor + shift) (dur * factor) note

stretch :: ScoreTime -> GenericEvent a -> GenericEvent a
stretch factor = place 0 factor

at :: ScoreTime -> GenericEvent a -> GenericEvent a
at shift (Event start dur note) = Event (start + shift) dur note

instance Pretty a => Pretty (GenericEvent a) where
    pretty (Event start dur note) =
        "Event " <> showt start <> " " <> showt dur
            <> " (" <> pretty note <> ")"

-- | Get the Events of subtracks, if any, returning one list of events per sub
-- note track.  This is the top-level utility for note calls that take other
-- note calls as arguments.
sub_events :: Derive.PassedArgs d -> Derive.Deriver [[Event]]
sub_events = sub_events_ False

-- | Like 'sub_events', but exclude events at the start time, and include
-- events at the end time.  Presumably suitable for 'Event.Negative' calls.
sub_events_negative :: Derive.PassedArgs d -> Derive.Deriver [[Event]]
sub_events_negative = sub_events_ True

-- | Throw an exception if there are sub-events.
assert_no_subs :: Derive.PassedArgs d -> Derive.Deriver ()
assert_no_subs args = do
    -- Due to laziness, checking null shouldn't require any actual slicing.
    events <- sub_events args
    if null events then return () else case Derive.ctx_sub_events ctx of
        Just subs -> Derive.throw $ "expected no sub events, but got "
            <> pretty (map (map (\(s, d, _) -> (s, d))) subs)
        Nothing -> Derive.throw $ "expected no sub events, but got "
            <> pretty (map (fmap extract_track) (Derive.ctx_sub_tracks ctx))
    where
    ctx = Derive.passed_ctx args
    extract_track t = (TrackTree.track_title t, TrackTree.track_id t)

sub_events_ :: Bool -> Derive.PassedArgs d -> Derive.Deriver [[Event]]
sub_events_ include_end args =
    case Derive.ctx_sub_events (Derive.passed_ctx args) of
        Nothing -> either Derive.throw (return . map (map mkevent)) $
            Slice.checked_slice_notes include_end start end
                (Derive.ctx_sub_tracks (Derive.passed_ctx args))
        Just events -> return $ map (map (\(s, d, n) -> Event s d n)) events
    where
    (start, end) = Args.range args
    -- The events have been shifted back to 0 by 'Slice.checked_slice_notes',
    -- but are still their original lengths.  Stretch them back to 1 so Events
    -- are normalized.
    mkevent (shift, stretch, tree) = Event
        { event_start = shift
        , event_duration = stretch
        , event_note = Derive.stretch
            (if stretch == 0 then 1 else recip stretch)
            (BlockUtil.derive_tracks tree)
        }

-- | Modify the text of sub note tracks before deriving them.  This can be
-- used to implement an ad-hoc new language.
modify_notes :: ([GenericEvent Text] -> Either Text [GenericEvent Text])
    -> Derive.PassedArgs a -> Either Text (Derive.PassedArgs a)
modify_notes modify =
    modify_sub_tracks $ modify_sub_notes (fmap to . modify . from)
    where
    from = map (\e -> Event (Event.start e) (Event.duration e) (Event.text e))
        . Events.ascending
    to = Events.from_list
        . map (\(Event start dur text) -> Event.event start dur text)

modify_sub_notes :: (Events.Events -> Either Text Events.Events)
    -> TrackTree.EventsTree -> Either Text TrackTree.EventsTree
modify_sub_notes modify = traverse $ traverse $ \track ->
    if ParseTitle.is_note_track (TrackTree.track_title track)
        then do
            events <- modify (TrackTree.track_events track)
            Right $ track { TrackTree.track_events = events }
        else Right track

modify_sub_tracks :: (TrackTree.EventsTree -> Either Text TrackTree.EventsTree)
    -> Derive.PassedArgs a -> Either Text (Derive.PassedArgs a)
modify_sub_tracks modify args = do
    tracks <- modify $ Derive.ctx_sub_tracks (Derive.passed_ctx args)
    Right $ args
        { Derive.passed_ctx = (Derive.passed_ctx args)
            { Derive.ctx_sub_tracks = tracks }
        }

derive_subs :: Derive.PassedArgs d -> Derive.NoteDeriver
derive_subs = derive_tracks <=< sub_events

-- | Derive and merge Events.
derive :: [Event] -> Derive.NoteDeriver
derive = mconcatMap (\(Event s d n) -> Derive.place s d n)

derive_tracks :: [[Event]] -> Derive.NoteDeriver
derive_tracks = derive . Seq.merge_lists event_start

-- | Get the pitch of an Event.  Useful for debugging.
derive_pitch :: Event -> Derive.Deriver (GenericEvent (Maybe Pitch.Note))
derive_pitch event = do
    stream <- event_note event
    let note = Score.initial_note =<< Seq.head (Stream.events_of stream)
    return $ event { event_note = note }

-- | Re-fit the events from one range to another.
fit :: (ScoreTime, ScoreTime) -- ^ fit this range
    -> (ScoreTime, ScoreTime) -- ^ into this range
    -> [Event] -> Derive.NoteDeriver
fit (from_start, from_end) (to_start, to_end) events =
    Derive.place to_start factor $ derive
        [e { event_start = event_start e - from_start } | e <- events]
    -- Subtract from_start because Derive.place is going to add the start back
    -- on again in the form of to_start.
    where factor = (to_end - to_start) / (from_end - from_start)

-- ** RestEvent

-- | A Nothing represents a rest.
type RestEvent = GenericEvent (Maybe Derive.NoteDeriver)

-- | This is like 'sub_events', but gaps between the events are returned as
-- explicit rests.
sub_rest_events :: Bool -- ^ end bias
    -> Bool -- ^ if True, include the trailing gap as a rest
    -> Derive.PassedArgs d -> Derive.Deriver [[RestEvent]]
sub_rest_events include_end want_final_rest args =
    map (uncurry (find_gaps want_final_rest) (Args.range args)) <$>
        sub_events_ include_end args

find_gaps :: Bool -> ScoreTime -> ScoreTime -> [GenericEvent a]
    -> [GenericEvent (Maybe a)]
find_gaps want_final_rest start end (event : events)
    | gap > 0 = Event start gap Nothing : rest
    | otherwise = rest
    where
    gap = event_start event - start
    rest = (Just <$> event)
        : find_gaps want_final_rest (event_end event) end events
find_gaps want_final_rest start end []
    | want_final_rest && start < end = [Event start (end-start) Nothing]
    | otherwise = []

-- | 'fit' for 'RestEvent's.
fit_rests :: (ScoreTime, ScoreTime) -> (ScoreTime, ScoreTime)
    -> [RestEvent] -> Derive.NoteDeriver
fit_rests (from_start, from_end) (to_start, to_end) events =
    Derive.place to_start factor $
        derive [e { event_start = event_start e - from_start } |
            e <- strip_rests events]
    where factor = (to_end - to_start) / (from_end - from_start)

strip_rests :: [RestEvent] -> [Event]
strip_rests events = [Event s d n | Event s d (Just n) <- events]

-- * reapply

-- | Call a note parent with sub-events.  While you can easily call other
-- kinds of calls with 'Eval.reapply', note parents are more tricky
-- because they expect a track structure in 'Derive.ctx_sub_tracks'.  This
-- bypasses that and directly passes 'Event's to the note parent, courtesy
-- of 'Derive.ctx_sub_events'.
reapply :: Derive.Context Score.Event -> DeriveT.Expr -> [[Event]]
    -> Derive.NoteDeriver
reapply ctx expr notes = Eval.reapply subs expr
    where
    subs = ctx
        { Derive.ctx_sub_events =
            Just $ map (map (\(Event s d n) -> (s, d, n))) notes
        }

reapply_call :: Derive.Context Score.Event -> Expr.Symbol
    -> [DeriveT.Term] -> [[Event]] -> Derive.NoteDeriver
reapply_call ctx sym call_args =
    reapply ctx $ Expr.generator $ Expr.Call sym call_args

{- NOTE [under-invert]
    . To make 'lift' to an absolute pitch work outside of inversion, I'd need
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
