-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
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
    , derive_control_track, derive_note_track
    , defragment_track_signals, unwarp
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.Slice as Slice

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- | Per-track parameters, to cut down on the number of arguments taken by
-- 'derive_track'.
data TrackInfo = TrackInfo {
    tinfo_track :: !TrackTree.Track
    , tinfo_sub_tracks :: !TrackTree.EventsTree
    , tinfo_type :: !ParseTitle.Type
    } deriving (Show)

instance Pretty.Pretty TrackInfo where
    format (TrackInfo track subs ttype) = Pretty.record "TrackInfo"
        [ ("track", Pretty.format track)
        , ("sub_tracks", Pretty.format subs)
        , ("type", Pretty.format ttype)
        ]

type GetLastVal d = [d] -> Maybe d

type DeriveResult d = ([LEvent.LEvents d], Derive.Threaded, Derive.Collect)
type DeriveEmpty d = TrackInfo -> Maybe Event.Event -> Maybe Event.Event
    -> Maybe (Derive.LogsDeriver d) -> Maybe (Derive.LogsDeriver d)

-- | This is the toplevel function to derive control tracks.  It's responsible
-- for actually evaluating each event.
derive_control_track :: Derive.Callable d =>
    Derive.State -> TrackInfo -> GetLastVal d -> [Event.Event] -> DeriveResult d
derive_control_track = derive_track derive_empty
    where derive_empty _ _ _ deriver = deriver

-- | Derive a note track.  This is different from 'derive_control_track' in
-- two ways: it doesn't keep track of the previous sample, and it evaluates
-- orphans.
--
-- Orphans are uncovered events in note tracks in the sub-tracks.  They are
-- extracted with 'Slice.checked_slice_notes' and evaluated as-is.  The effect
-- is that note transformers can be stacked horizontally, and tracks left empty
-- have no effect, except whatever transformers they may have in their titles.
derive_note_track :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Derive.State -> TrackInfo -> [Event.Event] -> DeriveResult Score.Event
derive_note_track derive_tracks state tinfo =
    derive_track derive_empty state tinfo get_last_val
    where
    get_last_val = Seq.last
    derive_empty tinfo prev next deriver =
        case Maybe.catMaybes [orphans, deriver] of
            [] -> Nothing
            ds -> Just (mconcat ds)
        where
        orphans = derive_orphans derive_tracks prev end (tinfo_sub_tracks tinfo)
        end = maybe (TrackTree.track_end (tinfo_track tinfo)) Event.start next

derive_track :: forall d. Derive.Callable d =>
    DeriveEmpty d -> Derive.State -> TrackInfo -> GetLastVal d
    -> [Event.Event] -> DeriveResult d
derive_track derive_empty initial_state tinfo get_last_val =
    track_end . List.mapAccumL event1 accum_state . Seq.zipper []
    where
    accum_state = (record_track_dynamic track initial_state,
        lookup_prev_val track initial_state)
    track = tinfo_track tinfo
    event1 (prev_state, prev_val) (prev_events, cur_events) =
        ((state, next_val), levents)
        where
        (levents, state) = maybe ([], prev_state) (run_derive prev_state) $
            case cur_events of
                event : _ -> derive_empty tinfo (Seq.head prev_events)
                    (Just event) d_event
                [] -> derive_empty tinfo (Seq.head prev_events) Nothing d_event
        d_event = case cur_events of
            event : next_events ->
                Just $ derive_event tinfo prev_val prev_events event next_events
            [] -> Nothing

        -- I stop propagating the previous value when the event goes past the
        -- end of the track.  This happens because 'Slice.slice' includes one
        -- event past the end of the track, since control calls like to emit
        -- samples before.  But the result is that the event is evaluated
        -- twice, and I don't want its second evaluation to see its first
        -- evaluation as the "previous" value.  This does mean that if there
        -- are >1 events after track_end, the later ones won't get the proper
        -- prev_val, but I don't think that should happen, and if it does,
        -- I don't think I care what they emit.
        save_val = maybe False ((< TrackTree.track_end track) . Event.start)
            (Seq.head cur_events)
        next_val :: Maybe d
        next_val = if save_val
            then mplus (get_last_val (LEvent.events_of levents)) prev_val
            else prev_val
    track_end ((state, val), result) =
        (result, stash_prev_val track val $ Derive.state_threaded state,
            Derive.state_collect state)

lookup_prev_val :: Derive.Taggable a => TrackTree.Track -> Derive.State
    -> Maybe a
lookup_prev_val track state = do
    block_track <- TrackTree.block_track_id track
    tagged <- Map.lookup block_track $
        Derive.state_prev_val (Derive.state_threaded state)
    Derive.from_tagged tagged

stash_prev_val :: Derive.Taggable a => TrackTree.Track -> Maybe a
    -> Derive.Threaded -> Derive.Threaded
stash_prev_val track prev_val threaded = fromMaybe threaded $ do
    val <- prev_val
    block_track <- TrackTree.block_track_id track
    return $ threaded
        { Derive.state_prev_val = Map.insert block_track (Derive.to_tagged val)
            (Derive.state_prev_val threaded)
        }

run_derive :: Derive.State -> Derive.LogsDeriver d
    -> ([LEvent.LEvent d], Derive.State)
run_derive state deriver = (levents, out_state)
    where
    levents = map LEvent.Log logs ++ case result of
        Right stream -> stream
        Left err -> [LEvent.Log (Derive.error_to_warn err)]
    (result, out_state, logs) = Derive.run state deriver

derive_orphans :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Maybe Event.Event -> TrackTime -> TrackTree.EventsTree
    -> Maybe Derive.NoteDeriver
    -- ^ The Maybe is a micro-optimization to avoid returning 'mempty'.  This
    -- is because 'Derive.d_merge' doesn't know that one of its operands is
    -- empty, and does all the splitting of and restoring collect bother.
    -- I expect lots of empties here so maybe it makes a difference.
derive_orphans derive_tracks prev end subs
    | start >= end = Nothing
    | otherwise = case checked of
        Left err -> Just $ Log.warn err >> return []
        Right [] -> Nothing
        Right slices -> Just $ derive_tracks slices
    where
    checked = Slice.slice_orphans exclude_start start end subs
    exclude_start = maybe False ((==0) . Event.duration) prev
    start = maybe 0 Event.end prev

-- Notes on recording TrackDynamic at NOTE [record-track-dynamics].
--
-- I want controls from the first uninverted version (might be sliced because
-- a child note track will only ever be evaluated sliced), and the rest from
-- the first inverted version.
record_track_dynamic :: TrackTree.Track -> Derive.State -> Derive.State
record_track_dynamic track state = state { Derive.state_collect = collect }
    where
    collect = case Internal.record_track_dynamic (Derive.state_dynamic state) of
        Nothing -> mempty
        Just track_dyn
            | TrackTree.track_inverted track -> mempty
                { Derive.collect_track_dynamic_inverted = track_dyn }
            | otherwise -> mempty { Derive.collect_track_dynamic = track_dyn }

defragment_track_signals :: Score.Warp -> Derive.Collect -> Derive.Collect
defragment_track_signals warp collect
    | Map.null fragments = collect
    | otherwise = collect
        { Derive.collect_track_signals = Derive.collect_track_signals collect
            <> Map.map defragment fragments
        , Derive.collect_signal_fragments = mempty
        }
    where
    fragments = Derive.collect_signal_fragments collect
    defragment = unwarp warp . Signal.merge

unwarp :: Score.Warp -> Signal.Control -> Track.TrackSignal
unwarp warp control = case is_linear_warp warp of
    Just (shift, stretch) ->
        Track.TrackSignal (Signal.coerce control) shift stretch
    Nothing -> Track.TrackSignal unwarped 0 1
        where
        Score.Warp warp_sig shift stretch = warp
        unwarped = Signal.unwarp_fused warp_sig (RealTime.score shift)
            (RealTime.score stretch) control

-- | Return (shift, stretch) if the tempo is linear.  This relies on an
-- optimization in 'Derive.d_tempo' to notice when the tempo is constant and
-- give it 'Score.id_warp_signal'.
is_linear_warp :: Score.Warp -> Maybe (ScoreTime, ScoreTime)
is_linear_warp warp
    | Score.warp_signal warp == Score.id_warp_signal =
        Just (Score.warp_shift warp, Score.warp_stretch warp)
    | otherwise = Nothing

derive_event :: Derive.Callable d => TrackInfo -> Maybe d
    -> [Event.Event] -- ^ previous events, in reverse order
    -> Event.Event -- ^ cur event
    -> [Event.Event] -- ^ following events
    -> Derive.LogsDeriver d
derive_event tinfo prev_val prev event next
    | "--" `Text.isPrefixOf` Text.dropWhile (==' ') text = return []
    | otherwise = case Parse.parse_expr text of
        Left err -> Log.warn err >> return []
        Right expr -> Internal.with_stack_region (Event.min event + shifted)
            (Event.max event + shifted) $ Eval.apply_toplevel cinfo expr
    where
    shifted = TrackTree.track_shifted track
    text = Event.event_text event
    cinfo = Derive.CallInfo
        { Derive.info_expr = text
        , Derive.info_prev_val = prev_val
        , Derive.info_event = event
        -- Augment prev and next with the unevaluated "around" notes from
        -- 'State.track_around'.
        , Derive.info_prev_events = tprev ++ prev
        , Derive.info_next_events = next ++ tnext
        , Derive.info_event_end = case next ++ tnext of
            [] -> TrackTree.track_end track
            event : _ -> Event.start event
        , Derive.info_track_shifted = TrackTree.track_shifted track
        , Derive.info_inverted = TrackTree.track_inverted track
        , Derive.info_sub_tracks = subs
        , Derive.info_sub_events = Nothing
        , Derive.info_track_type = Just ttype
        }
    TrackInfo track subs ttype = tinfo
    (tprev, tnext) = TrackTree.track_around track
