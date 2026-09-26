-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{- | Derive tracks.

    It should also have Deriver utilities that could go in Derive, but are more
    specific to calls.

    It used to be that events were evaluated in \"normalized time\", which to
    say each one was shifted and stretched into place so that it always
    begins at 0t and ends at 1t.  While elegant, this was awkward in
    practice.  Some calls take ScoreTimes as arguments, and for those to
    be in the track's ScoreTime they have to be warped too.  Calls that
    look at the time of the next event on the track must warp that too.
    The result is that calls have to work in two time references
    simultaneously, which is confusing.  But the main thing is that note
    calls with subtracks need to slice the relevant events out of the
    subtracks, and those events are naturally in track time.  So the slice
    times would have to be unwarped, and then the sliced events warped.
    It was too complicated.

    Now events are evaluated in track time.  Block calls still warp the
    call into place, so blocks are still in normalized time, but other
    calls must keep track of their start and end times.

    The way expression evaluation works is a little irregular.  The toplevel
    expression returns a parameterized deriver, so this part of the type is
    exported to the haskell type system.  The values and non-toplevel calls
    return dynamically typed Vals though.  The difference between a generator
    and a transformer is that the latter takes an extra deriver arg, but since
    the type of the deriver is statically determined at the haskell level, it
    isn't passed as a normal arg but is instead hardcoded into the evaluation
    scheme for the toplevel expression.  So only the toplevel calls can take
    and return derivers.

    I experimented with a system that added a VDeriver type, but there were
    several problems:

    - If I don't parameterize Val I wind up with separate VEventDeriver,
    VPitchDeriver, etc. constructors.  Every call that takes a deriver must
    validate the type and there is no static guarantee that event deriver
    calls won't wind up the pitch deriver symbol table.  It seems nice that
    the CallMap and Environ can all be replaced with a single symbol table,
    but in practice they represent different scopes, so they would need to be
    separated anyway.

    - If I do parameterize Val, I need some complicated typeclass gymnastics
    and a lot of redundant Typecheck instances to make the new VDeriver type
    fit in with the calling scheme.  I have to differentiate PassedVals, which
    include VDeriver, from Vals, which don't, so Environ can remain
    unparameterized.  Otherwise I would need a separate Environ per track, and
    copy over vals which should be shared, like srate.  The implication is
    that Environ should really have dynamically typed deriver vals.

    - Replacing @a | b | c@ with @a (b (c))@ is appealing, but if the deriver
    is the final argument then I have a problem where a required argument wants
    to follow an optional one.  Solutions would be to implement some kind of
    keyword args that allow the required arg to remain at the end, or simply
    put it as the first arg, so that @a 1 | b 2 | c 3@ is sugar for
    @a (b (c 3) 2) 1@.

    - But, most importantly, I don't have a clear use for making derivers first
    class.  Examples would be:

        * A call that takes two derivers: @do-something (block1) (block2)@.
        I can't think of a @do-something@.

        * Derivers in the environment: @default-something = (block1)@.  I
        can't think of a @default-something@.

    I could move more in the direction of a real language by unifying all
    symbols into Environ, looking up Symbols in @eval@, and making a VCall
    type.  That way I could rebind calls with @tr = absolute-trill@ or
    do argument substitution with @d = (block1); transpose 1 | d@.  However,
    I don't have any uses in mind for that, and /haskell/ is supposed to be
    the real language.  I should focus more on making it easy to write your own
    calls in haskell.
-}
module Derive.EvalTrack (
    TrackInfo(..)
    , GetLastVal
    , deriveControlTrack, deriveNoteTrack
    , defragmentTrackSignals, unwarp
    , deriveEvent, context
#ifdef TESTING
    , module Derive.EvalTrack
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Slice as Slice
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Warp as Warp

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import           Global
import           Types


-- | Per-track parameters, to cut down on the number of arguments taken by
-- 'derive_note_track'.
data TrackInfo d = TrackInfo
    { tinfoTrack :: !TrackTree.Track
    , tinfoSubTracks :: !TrackTree.EventsTree
    , tinfoType :: !ParseTitle.Type
    , tinfoGetLastVal :: GetLastVal d
    }

tinfoPrevVal :: TrackInfo d -> Maybe d -> Stream.Stream d -> Maybe d
tinfoPrevVal tinfo prevVal levents =
    tinfoGetLastVal tinfo (Stream.eventsOf levents) <|> prevVal

instance Pretty (TrackInfo d) where
    format (TrackInfo track subs ttype _) = Pretty.record "TrackInfo"
        [ ("track", Pretty.format track)
        , ("sub_tracks", Pretty.format subs)
        , ("type", Pretty.format ttype)
        ]

type GetLastVal d = [d] -> Maybe d
type DeriveResult d = ([Stream.Stream d], Derive.Threaded, Derive.Collect)

-- | This is the toplevel function to derive control tracks.  It's responsible
-- for actually evaluating each event.
{-# SCC deriveControlTrack #-}
deriveControlTrack :: Derive.CallableExpr d => Derive.State -> TrackInfo d
    -> DeriveResult d
deriveControlTrack state tinfo =
    postTrack track $ useSaveVal $
    List.mapAccumL (deriveControlTrackStream tinfo) initialState
        (eventPrevNexts tinfo)
    where
    initialState = (recordTrackDynamic track state, val, val)
        where val = lookupPrevVal track state
    track = tinfoTrack tinfo
    useSaveVal ((state, _, saveVal), result) = ((state, saveVal), result)

{- | This is the note track version of 'derive_control_track'.  The main
    difference is that it evaluates orphans.

    Orphans are uncovered events in note tracks in the sub-tracks.  They are
    extracted with 'Slice.checked_slice_notes' and evaluated as-is.  The effect
    is that note parents can be stacked horizontally, and tracks left empty
    have no effect, except whatever transformers they may have in their titles.

    This is all very complicated and unsatisfactory, but it's still less
    complicated and somewhat more satisfactory than it used to be, if you can
    imagine that.
-}
{-# SCC deriveNoteTrack #-}
deriveNoteTrack :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Derive.State -> TrackInfo Score.Event -> DeriveResult Score.Event
deriveNoteTrack deriveTracks state tinfo
    | TrackTree.trackSliced (tinfoTrack tinfo) == TrackTree.Inversion =
        deriveInverted state tinfo $
        Derive.stateInversion (Derive.stateDynamic state)
    | otherwise = deriveNoteTrack_ deriveTracks state tinfo

-- This function is split out from 'derive_note_track' to emphasize the shared
-- structure with 'derive_control_track'.
deriveNoteTrack_ :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Derive.State -> TrackInfo Score.Event
    -> ([Stream.Stream Score.Event], Derive.Threaded, Derive.Collect)
deriveNoteTrack_ deriveTracks state tinfo =
    postTrack track $
    List.mapAccumL (deriveNoteTrackStream deriveTracks tinfo) initialState
        (eventPrevNexts tinfo)
    where
    initialState = (recordTrackDynamic track state, val)
        where val = lookupPrevVal track state
    track = tinfoTrack tinfo

-- I used to use the same function to derive note and control tracks.  However,
-- over time control and note tracks gradually gained separate features, and
-- the function became more complicated to accommodate the differences.
-- Eventually I just split them into separate functions.  The result is
-- simpler, but there is a certain amount of duplicated code between them.
-- So if you modify one, make sure you also modify the other if applicable.

deriveInverted :: Derive.State -> TrackInfo Score.Event
    -> Derive.Inversion -> DeriveResult Score.Event
deriveInverted state_ tinfo inversion =
    ([levents], threaded, Derive.stateCollect nextState)
    where
    (levents, nextState) = runDerive state $ case inversion of
        Derive.NotInverted ->
            Derive.throw "inverted track didn't set state_inversion"
        Derive.InversionInProgress generator -> withInverted tinfo $
            Derive.stateUnderInvert (Derive.stateDynamic state) generator
    threaded = stashPrevVal track nextVal $ Derive.stateThreaded nextState
    prevVal = lookupPrevVal track state_
    nextVal = tinfoPrevVal tinfo prevVal levents
    track = tinfoTrack tinfo
    state = recordTrackDynamic track state_

-- | Update Dynamic before evaluating the inverted generator.
withInverted :: TrackInfo d -> Derive.Deriver a -> Derive.Deriver a
withInverted tinfo = Internal.local $ \state ->
    maybe id Internal.addStackFrame frame $ state
        { Derive.stateInversion = Derive.NotInverted }
    where
    -- The region is redundant, since the uninverted call has already put it on
    -- the stack, but inversion causes the tracks to go on the stack again, and
    -- if I don't put the region on then the [block, track, region] order is
    -- messed up.
    -- TODO should I put the call name on again?  I could stash it in the event
    -- text.
    frame = (\e -> Stack.Region (Event.min e + shifted) (Event.max e + shifted))
        <$> maybeEvent
    shifted = TrackTree.trackShifted (tinfoTrack tinfo)
    maybeEvent = Events.head $ TrackTree.trackEvents $ tinfoTrack tinfo

-- | Extract the final state at the end of a track derivation.
postTrack :: Derive.Taggable d => TrackTree.Track
    -> ((Derive.State, Maybe d), a) -> (a, Derive.Threaded, Derive.Collect)
postTrack track ((state, saveVal), result) =
    ( result
    , stashPrevVal track saveVal $ Derive.stateThreaded state
    , Derive.stateCollect state
    )

-- | Get all event prefixes and suffixes.
eventPrevNexts :: TrackInfo d -> [([Event.Event], [Event.Event])]
eventPrevNexts =
    Lists.zipper [] . Events.ascending . TrackTree.trackEvents . tinfoTrack

-- | Derive one event on a control track.  Carrying previous values forward
-- on a control track is a bit more complicated, because there is a separate
-- next_val and save_val.  The next_val should be the next event's prev_val,
-- and the save_val should be saved as the final next_val at the end of the
-- track.  The reason is that I only save a prev val if the event won't be
-- derived again, e.g.  there's a future event <= the start of the next slice.
-- Otherwise, a sliced event will see its own output as its previous val.
deriveControlTrackStream :: Derive.CallableExpr d
    => TrackInfo d
    -> (Derive.State, Maybe d, Maybe d)
    -> ([Event.Event], [Event.Event])
    -> ((Derive.State, Maybe d, Maybe d), Stream.Stream d)
deriveControlTrackStream tinfo (prevState, prevVal, prevSaveVal)
        (prevEvents, curEvents) =
    ((state, nextVal, saveVal), stream)
    where
    (stream, state) = case derivers of
        Just deriver -> runDerive (resetEventSerial prevState) deriver
        Nothing -> (Stream.empty, prevState)
    derivers = case curEvents of
        event : nextEvents -> Just $
            deriveEvent (context tinfo prevVal prevEvents event nextEvents)
                event
        [] -> Nothing
    nextVal = tinfoPrevVal tinfo prevVal stream
    saveVal = if shouldSaveVal then nextVal else prevSaveVal
    shouldSaveVal = case curEvents of
        _ : next : _ ->
            Event.start next <= TrackTree.trackEnd (tinfoTrack tinfo)
        _ -> False

-- | Derive one event on a note track.  This also derives orphan events
-- before the event, or after the last event.
{-# SCC deriveNoteTrackStream #-}
deriveNoteTrackStream :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> TrackInfo Score.Event
    -> (Derive.State, Maybe Score.Event)
    -> ([Event.Event], [Event.Event])
    -> ((Derive.State, Maybe Score.Event), Stream.Stream Score.Event)
deriveNoteTrackStream deriveTracks tinfo (prevState, prevVal)
        (prevEvents, curEvents) =
    ((nextState, nextVal), stream)
    where
    (stream, nextState)
        | null derivers = (Stream.empty, prevState)
        | otherwise = runDerive (resetEventSerial prevState)
            (mconcat derivers)
    derivers = Maybe.catMaybes $ case curEvents of
        event : nextEvents ->
            [ deriveEmpty (Lists.head prevEvents) (Just event)
            , Just $ deriveNote event nextEvents
            ]
        [] -> [deriveEmpty (Lists.head prevEvents) Nothing]
    deriveNote event nextEvents = deriveEvent ctx event
        where ctx = context tinfo prevVal prevEvents event nextEvents
    nextVal = tinfoPrevVal tinfo prevVal stream
    -- Look for orphans in the gap between events.
    deriveEmpty prev next =
        deriveOrphans deriveTracks prev end (tinfoSubTracks tinfo)
        where
        end = maybe (TrackTree.trackEnd (tinfoTrack tinfo)) Event.start next

-- | See 'Derive.state_event_serial' for what this is doing.
resetEventSerial :: Derive.State -> Derive.State
resetEventSerial state =
    case Derive.stateInversion (Derive.stateDynamic state) of
        Derive.NotInverted -> state
            { Derive.stateThreaded = (Derive.stateThreaded state)
                { Derive.stateEventSerial = 0 }
            }
        _ -> state

lookupPrevVal :: Derive.Taggable a => TrackTree.Track -> Derive.State
    -> Maybe a
lookupPrevVal track state = do
    blockTrack <- TrackTree.blockTrackId track
    tagged <- Map.lookup blockTrack $
        Derive.statePrevVal (Derive.stateThreaded state)
    Derive.fromTagged tagged

stashPrevVal :: Derive.Taggable a => TrackTree.Track -> Maybe a
    -> Derive.Threaded -> Derive.Threaded
stashPrevVal track prevVal threaded = fromMaybe threaded $ do
    val <- prevVal
    blockTrack <- TrackTree.blockTrackId track
    pure $ threaded
        { Derive.statePrevVal = Map.insert blockTrack (Derive.toTagged val)
            (Derive.statePrevVal threaded)
        }

-- | Run a derivation.  If the deriver throws an exception, it will be caught
-- and turned into a log msg, and any state changes rolled back.
-- 'Internal.local' relies on this, since it doesn't revert the state after an
-- exception.
runDerive :: Derive.State -> Derive.Deriver (Stream.Stream d)
    -> (Stream.Stream d, Derive.State)
runDerive state deriver = case result of
    Right stream -> (Stream.mergeLogs logs stream, outState)
    Left err -> (Stream.mergeLogs logs (errorToStream err), state)
    where
    errorToStream = Stream.fromLogs . (:[]) . Derive.errorToWarn
    (result, outState, logs) = Derive.run state deriver

deriveOrphans :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Maybe Event.Event -> TrackTime -> TrackTree.EventsTree
    -> Maybe Derive.NoteDeriver
    -- ^ The Maybe is a micro-optimization to avoid returning 'mempty'.  This
    -- is because 'Derive.d_merge' doesn't know that one of its operands is
    -- empty, and does all the splitting of and restoring collect bother.
    -- I expect lots of empties here so maybe it makes a difference.
deriveOrphans deriveTracks prev end subs
    | start >= end = Nothing
    | otherwise = case checked of
        Left err -> Just $ Log.warn err >> pure Stream.empty
        Right [] -> Nothing
        Right slices -> Just $ deriveTracks slices
    where
    checked = Slice.sliceOrphans excludeStart start end subs
    excludeStart = maybe False ((==0) . Event.duration) prev
    start = maybe 0 Event.end prev

-- Notes on recording TrackDynamic at NOTE [record-track-dynamics].
--
-- I want controls from the first uninverted version (might be sliced because
-- a child note track will only ever be evaluated sliced), and the rest from
-- the first inverted version.
recordTrackDynamic :: TrackTree.Track -> Derive.State -> Derive.State
recordTrackDynamic track state =
    -- TODO I feel like I should merge this with the existing collect, but
    -- profiling shows that it kills performance.  Perhaps I wind up
    -- duplicating the collect and doing a bunch of extra merging every
    -- (inverted) event.  I'm not sure at the moment, but it should be safe to
    -- start with an empty collect anyway.
    state { Derive.stateCollect = collect }
    where
    collect = case Internal.recordTrackDynamic (Derive.stateDynamic state) of
        Nothing -> mempty
        Just trackDyn
            | TrackTree.trackSliced track == TrackTree.Inversion -> mempty
                { Derive.collectTrackDynamicInverted = trackDyn }
            | otherwise -> mempty { Derive.collectTrackDynamic = trackDyn }

defragmentTrackSignals :: Warp.Warp -> Derive.Collect -> Derive.Collect
defragmentTrackSignals warp collect
    | Map.null fragments = collect
    | otherwise = collect
        { Derive.collectTrackSignals = Derive.collectTrackSignals collect
            <> Map.map defragment fragments
        , Derive.collectSignalFragments = mempty
        }
    where
    fragments = Derive.collectSignalFragments collect
    defragment = unwarp warp . mconcat . Map.elems

unwarp :: Warp.Warp -> Signal.Control -> Track.TrackSignal
unwarp warp control = case Warp.isLinear warp of
    Just linear -> Track.TrackSignal
        { tsSignal = Signal.coerce control
        , tsShift = RealTime.toScore (Warp._shift linear)
        , tsStretch = RealTime.toScore (Warp._stretch linear)
        }
    Nothing -> Track.TrackSignal
        { tsSignal = Warp.unwarpSignal warp control
        , tsShift = 0
        , tsStretch = 1
        }

{-# SCC deriveEvent #-}
deriveEvent :: Derive.CallableExpr d => Derive.Context d -> Event.Event
    -> Derive.Deriver (Stream.Stream d)
deriveEvent ctx event
    | Parse.commentRemove `Text.isPrefixOf` Text.stripStart text =
        pure Stream.empty
    | otherwise = withEventRegion (Derive.ctxTrackShifted ctx) event $
        case Parse.parseExpr text of
            Left err -> Log.warn err >> pure Stream.empty
            Right expr -> withNoteStartEnd $ Eval.evalToplevel ctx expr
    where
    text = Event.text event
    withNoteStartEnd = case Derive.ctxTrackType ctx of
        Just ParseTitle.NoteTrack -> Derive.withVals
                [ (EnvKey.noteStart, Event.start event)
                , (EnvKey.noteEnd, Event.end event)
                ]
            where event = Derive.ctxEvent ctx
        _ -> id

withEventRegion :: ScoreTime -> Event.Event -> Derive.Deriver a
    -> Derive.Deriver a
withEventRegion trackShifted event =
    Internal.withStackRegion (Event.min event + trackShifted)
        (Event.max event + trackShifted)

context :: TrackInfo a -> Maybe val
    -> [Event.Event] -- ^ previous events, in reverse order
    -> Event.Event -- ^ cur event
    -> [Event.Event] -- ^ following events
    -> Derive.Context val
context tinfo prevVal prev event next = Derive.Context
    { Derive.ctxPrevVal = prevVal
    , Derive.ctxEvent = event
    -- Augment prev and next with the unevaluated "around" notes from
    -- 'State.track_around'.
    , Derive.ctxPrevEvents = tprev ++ prev
    , Derive.ctxNextEvents = next ++ tnext
    , Derive.ctxEventEnd = case next ++ tnext of
        [] -> TrackTree.trackEnd track
        event : _ -> Event.start event
    , Derive.ctxTrackShifted = TrackTree.trackShifted track
    , Derive.ctxSubTracks = subs
    , Derive.ctxSubEvents = Nothing
    , Derive.ctxTrackType = Just ttype
    }
    where
    TrackInfo track subs ttype _ = tinfo
    (tprev, tnext) = TrackTree.trackAround track
