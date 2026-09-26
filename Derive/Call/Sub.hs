-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | This is the basic interface for slicing.  This also includes 'inverting',
-- which is a special case of slicing.
module Derive.Call.Sub (
    -- * inversion
    underInvert
    , inverting, invertingArgs
    -- ** events
    , subEvents, subEventsNegative
    , subTracks
    , assertNoSubs
    , modifyNotes
    , deriveSubs, derive, deriveTracks, derivePitch, fit
    -- ** RestEvent
    , RestEvent, subRestEvents
    , fitRests, stripRests
    -- * reapply
    , reapply, reapplyCall
) where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.SubT as SubT
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
underInvert :: (Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver)
    -> Derive.NoteArgs -> Derive.NoteDeriver -> Derive.NoteDeriver
underInvert transformer args deriver
    | null $ Derive.ctxSubTracks $ Derive.passedCtx args =
        transformer args deriver
    | otherwise = with deriver
    where
    with = Internal.local $ \state -> state
        { Derive.stateUnderInvert =
            Derive.stateUnderInvert state . transformer args
        }

-- | Convert a call into an inverting call.  Documented in
-- @doc/slicing-inverting.md@.
runInvert :: Derive.PassedArgs d -> Derive.NoteDeriver -> Derive.NoteDeriver
runInvert args call = do
    dyn <- Internal.getDynamic id
    case (Derive.stateInversion dyn, Derive.ctxSubTracks ctx) of
        (Derive.InversionInProgress {}, _) ->
            Derive.throw "tried to invert while inverting"
        (Derive.NotInverted, subs@(_:_)) -> do
            sliced <- invert subs event (Args.next args)
                (Derive.ctxPrevEvents ctx, Derive.ctxNextEvents ctx)
            withInversion $ BlockUtil.deriveTracks sliced
        (Derive.NotInverted, []) -> call
    where
    withInversion = Internal.local $ \dyn -> dyn
        { Derive.stateInversion = Derive.InversionInProgress call }
    event = Derive.ctxEvent ctx
    ctx = Derive.passedCtx args

-- | Convert a call into an inverting call.  This is designed to be convenient
-- to insert after the signature arg in a call definition.  The args passed
-- to the call have been stripped of their sub tracks to avoid another
-- inversion.
inverting :: (Derive.PassedArgs d -> Derive.NoteDeriver) -> Derive.PassedArgs d
    -> Derive.NoteDeriver
inverting call args = runInvert args (call stripped)
    where
    stripped = args
        { Derive.passedCtx = (Derive.passedCtx args)
            { Derive.ctxSubTracks = mempty
            , Derive.ctxSubEvents = Nothing
            }
        }

-- | 'inverting' with its arguments flipped.  This is useful for calls that
-- want to do stuff with the args before inverting.  Make sure to shadow the
-- old 'Derive.PassedArgs' with the ones passed to the call, for the reason
-- documented in 'inverting'.
invertingArgs :: Derive.PassedArgs d
    -> (Derive.PassedArgs d -> Derive.NoteDeriver) -> Derive.NoteDeriver
invertingArgs = flip inverting

-- When I invert, I call derive_tracks again, which means the inverted bottom
-- is going to expect to see the current prev val.  TODO but evidently I don't
-- need this?  Try to make a problem without it.
savePrevVal :: Derive.Taggable a => Derive.PassedArgs a -> Derive.Deriver ()
savePrevVal args = case Args.prevVal args of
    Nothing -> pure ()
    Just val -> Stack.blockTrackOf <$> Internal.getStack >>= \case
        Nothing -> pure ()
        Just blockTrack -> modifyThreaded $ \th -> th
            { Derive.statePrevVal = Map.insert blockTrack
                (Derive.toTagged val) (Derive.statePrevVal th)
            }
    where
    modifyThreaded modify = Derive.modify $
        \st -> st { Derive.stateThreaded = modify (Derive.stateThreaded st) }

invert :: TrackTree.EventsTree -> Event.Event -> ScoreTime
    -> ([Event.Event], [Event.Event]) -> Derive.Deriver TrackTree.EventsTree
invert subs event nextStart eventsAround = do
    -- Pick the current TrackId out of the stack, and give that to the track
    -- created by inversion.
    -- TODO I'm not 100% comfortable with this, I don't like putting implicit
    -- dependencies on the stack like this.  Too many of these and someday
    -- I change how the stack works and all sorts of things break.  It would be
    -- more explicit to put TrackId into Context.
    trackId <- stackTrackId
    let sliced = slice trackId
    whenJust (nonBottomNoteTrack sliced) $ \track -> Derive.throw $
        "inverting below a note track will lead to an endless loop: "
        <> pretty (TrackTree.trackId track)
    pure sliced
    where
    slice trackId =
        map (Slice.slice False (Event.start event) nextStart
            (Just (insert trackId))) subs
    -- Use 'next_start' instead of track_end because in the absence of a next
    -- note, the track end becomes next note and clips controls.
    insert trackId = Slice.InsertEvent
        { eventDuration = Event.duration event
        , eventOrientation = Event.orientation event
        , eventAround = eventsAround
        , eventTrackId = trackId
        }

stackTrackId :: Derive.Deriver (Maybe TrackId)
stackTrackId = Lists.head . mapMaybe Stack.trackOf . Stack.innermost
    <$> Internal.getStack

-- | An inverting call above another note track will lead to an infinite loop
-- if there are overlapping sub-events that also invert, or confusing results
-- if there are non-overlapping or non-inverting sub-events.  Either way, I
-- don't think I want it.
--
-- An exception is if the note track is empty, since I can be sure there are
-- no inverting calls in that case.
nonBottomNoteTrack :: TrackTree.EventsTree -> Maybe TrackTree.Track
nonBottomNoteTrack tree = Lists.head (concatMap go tree)
    where
    go (Tree.Node track subs)
        | ParseTitle.isNoteTrack (TrackTree.trackTitle track)
            && not (Events.null (TrackTree.trackEvents track))
            && not (null subs) = [track]
        | otherwise = concatMap go subs

-- * sub tracks

-- | Get the Events of subtracks, if any, returning one list of events per sub
-- note track.  This is the top-level utility for note calls that take other
-- note calls as arguments.
subEvents :: Derive.PassedArgs d -> Derive.Deriver [[SubT.Event]]
subEvents = subEvents_ False

-- | TODO maybe this should replace 'sub_events'
subTracks :: Derive.PassedArgs d -> Derive.Deriver [SubT.Track]
subTracks = subTracks_ False

-- | Like 'sub_events', but exclude events at the start time, and include
-- events at the end time.  Presumably suitable for 'Event.Negative' calls.
subEventsNegative :: Derive.PassedArgs d -> Derive.Deriver [[SubT.Event]]
subEventsNegative = subEvents_ True

-- | Throw an exception if there are sub-events.
assertNoSubs :: Derive.PassedArgs d -> Derive.Deriver ()
assertNoSubs args = do
    -- Due to laziness, checking null shouldn't require any actual slicing.
    events <- subEvents args
    if null events then pure () else case Derive.ctxSubEvents ctx of
        Just subs -> Derive.throw $ "expected no sub events, but got "
            <> pretty (map (map (\(s, d, _) -> (s, d))) subs)
        Nothing -> Derive.throw $ "expected no sub events, but got "
            <> pretty (map (fmap extractTrack) (Derive.ctxSubTracks ctx))
    where
    ctx = Derive.passedCtx args
    extractTrack t = (TrackTree.trackTitle t, TrackTree.trackId t)

subEvents_ :: Bool -> Derive.PassedArgs d -> Derive.Deriver [[SubT.Event]]
subEvents_ includeEnd args = map SubT._events <$> subTracks_ includeEnd args

subTracks_ :: Bool -> Derive.PassedArgs d -> Derive.Deriver [SubT.Track]
subTracks_ includeEnd args =
    case Derive.ctxSubEvents (Derive.passedCtx args) of
        Nothing -> either Derive.throw (pure . mapMaybe mktrack) $
            Slice.checkedSliceNotes includeEnd start end $
                Derive.ctxSubTracks (Derive.passedCtx args)
        Just tracks -> pure
            [ SubT.Track (Left $ "subevent:" <> showt i) $
                map (\(s, d, n) -> SubT.EventT s d n) track
            | (i, track) <- zip [0..] tracks
            ]
    where
    mktrack (Slice.Track trackId notes) = Just $
        -- 'TrackTree.track_id' can be Nothing.  Presumably this happens for
        -- a constructed block.
        SubT.Track (maybe (Left "no-track-id") Right trackId)
            (map mkevent notes)
    (start, end) = Args.range args
    -- The events have been shifted back to 0 by 'Slice.checked_slice_notes',
    -- but are still their original lengths.  Stretch them back to 1 so Events
    -- are normalized.
    mkevent (shift, stretch, tree) = SubT.EventT
        { _start = shift
        , _duration = stretch
        , _note = Derive.stretch
            (if stretch == 0 then 1 else recip stretch)
            (BlockUtil.deriveTracks tree)
        }

-- | Modify the text of sub note tracks before deriving them.  This can be
-- used to implement an ad-hoc new language.
modifyNotes :: ([SubT.EventT Text] -> Either Text [SubT.EventT Text])
    -> Derive.PassedArgs a -> Either Text (Derive.PassedArgs a)
modifyNotes modify =
    modifySubTracks $ modifySubNotes (fmap to . modify . from)
    where
    from = map make . Events.ascending
    make e = SubT.EventT
        { _start = Event.start e
        , _duration = Event.duration e
        , _note = Event.text e
        }
    to = Events.fromList
        . map (\(SubT.EventT start dur text) -> Event.event start dur text)

modifySubNotes :: (Events.Events -> Either Text Events.Events)
    -> TrackTree.EventsTree -> Either Text TrackTree.EventsTree
modifySubNotes modify = traverse $ traverse $ \track ->
    if ParseTitle.isNoteTrack (TrackTree.trackTitle track)
        then do
            events <- modify (TrackTree.trackEvents track)
            Right $ track { TrackTree.trackEvents = events }
        else Right track

modifySubTracks :: (TrackTree.EventsTree -> Either Text TrackTree.EventsTree)
    -> Derive.PassedArgs a -> Either Text (Derive.PassedArgs a)
modifySubTracks modify args = do
    tracks <- modify $ Derive.ctxSubTracks (Derive.passedCtx args)
    Right $ args
        { Derive.passedCtx = (Derive.passedCtx args)
            { Derive.ctxSubTracks = tracks }
        }

deriveSubs :: Derive.PassedArgs d -> Derive.NoteDeriver
deriveSubs = deriveTracks <=< subEvents

-- | Derive and merge Events.
derive :: [SubT.Event] -> Derive.NoteDeriver
derive = mconcatMap (\(SubT.EventT s d n) -> Derive.place s d n)

deriveTracks :: [[SubT.Event]] -> Derive.NoteDeriver
deriveTracks = derive . Lists.mergeLists SubT._start

-- | Get the pitch of an Event.  Useful for debugging.
derivePitch :: SubT.Event -> Derive.Deriver (SubT.EventT (Maybe Pitch.Note))
derivePitch event = do
    stream <- SubT._note event
    let note = Score.initialNote =<< Lists.head (Stream.eventsOf stream)
    pure $ event { SubT._note = note }

-- | Re-fit the events from one range to another.
fit :: (ScoreTime, ScoreTime) -- ^ fit this range
    -> (ScoreTime, ScoreTime) -- ^ into this range
    -> [SubT.Event] -> Derive.NoteDeriver
fit (fromStart, fromEnd) (toStart, toEnd) events =
    Derive.place toStart factor $ derive
        [e { SubT._start = SubT._start e - fromStart } | e <- events]
    -- Subtract from_start because Derive.place is going to add the start back
    -- on again in the form of to_start.
    where factor = (toEnd - toStart) / (fromEnd - fromStart)

-- ** RestEvent

-- | A Nothing represents a rest.
type RestEvent = SubT.EventT (Maybe Derive.NoteDeriver)

-- | This is like 'sub_events', but gaps between the events are returned as
-- explicit rests.
subRestEvents :: Bool -- ^ end bias
    -> Bool -- ^ if True, include the trailing gap as a rest
    -> Derive.PassedArgs d -> Derive.Deriver [[RestEvent]]
subRestEvents includeEnd wantFinalRest args =
    map (uncurry (findGaps wantFinalRest) (Args.range args)) <$>
        subEvents_ includeEnd args

findGaps :: Bool -> ScoreTime -> ScoreTime -> [SubT.EventT a]
    -> [SubT.EventT (Maybe a)]
findGaps wantFinalRest start end (event : events)
    | gap > 0 = SubT.EventT start gap Nothing : rest
    | otherwise = rest
    where
    gap = SubT._start event - start
    rest = (Just <$> event)
        : findGaps wantFinalRest (SubT.end event) end events
findGaps wantFinalRest start end []
    | wantFinalRest && start < end = [SubT.EventT start (end-start) Nothing]
    | otherwise = []

-- | 'fit' for 'RestEvent's.
fitRests :: (ScoreTime, ScoreTime) -> (ScoreTime, ScoreTime)
    -> [RestEvent] -> Derive.NoteDeriver
fitRests (fromStart, fromEnd) (toStart, toEnd) events =
    Derive.place toStart factor $
        derive [e { SubT._start = SubT._start e - fromStart } |
            e <- stripRests events]
    where factor = (toEnd - toStart) / (fromEnd - fromStart)

stripRests :: [RestEvent] -> [SubT.Event]
stripRests events = [SubT.EventT s d n | SubT.EventT s d (Just n) <- events]

-- * reapply

-- | Call a note parent with sub-events.  While you can easily call other
-- kinds of calls with 'Eval.reapply', note parents are more tricky
-- because they expect a track structure in 'Derive.ctx_sub_tracks'.  This
-- bypasses that and directly passes 'SubT.EventT's to the note parent,
-- courtesy of 'Derive.ctx_sub_events'.
reapply :: Derive.Context Score.Event -> DeriveT.Expr -> [[SubT.Event]]
    -> Derive.NoteDeriver
reapply ctx expr notes = Eval.reapply subs expr
    where
    subs = ctx
        { Derive.ctxSubEvents =
            Just $ map (map (\(SubT.EventT s d n) -> (s, d, n))) notes
        }

reapplyCall :: Derive.Context Score.Event -> Expr.Symbol
    -> [DeriveT.Term] -> [[SubT.Event]] -> Derive.NoteDeriver
reapplyCall ctx sym callArgs =
    reapply ctx $ Expr.generator $ Expr.Call sym callArgs

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
