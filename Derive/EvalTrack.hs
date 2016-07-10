-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
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
    , derive_event, context
#ifdef TESTING
    , module Derive.EvalTrack
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

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

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


-- | Per-track parameters, to cut down on the number of arguments taken by
-- 'derive_track'.
data TrackInfo d = TrackInfo {
    tinfo_track :: !TrackTree.Track
    , tinfo_sub_tracks :: !TrackTree.EventsTree
    , tinfo_type :: !ParseTitle.Type
    , tinfo_get_last_val :: GetLastVal d
    }

tinfo_prev_val :: TrackInfo d -> Maybe d -> Stream.Stream d -> Maybe d
tinfo_prev_val tinfo prev_val levents =
    tinfo_get_last_val tinfo (Stream.events_of levents) <|> prev_val

instance Pretty.Pretty (TrackInfo d) where
    format (TrackInfo track subs ttype _) = Pretty.record "TrackInfo"
        [ ("track", Pretty.format track)
        , ("sub_tracks", Pretty.format subs)
        , ("type", Pretty.format ttype)
        ]

type GetLastVal d = [d] -> Maybe d
type DeriveResult d = ([Stream.Stream d], Derive.Threaded, Derive.Collect)

-- | This is the toplevel function to derive control tracks.  It's responsible
-- for actually evaluating each event.
derive_control_track :: Derive.Callable d => Derive.State -> TrackInfo d
    -> DeriveResult d
derive_control_track state tinfo =
    post_track track $ use_save_val $
    List.mapAccumL (derive_control_track_stream tinfo) initial_state
        (event_prev_nexts tinfo)
    where
    initial_state = (record_track_dynamic track state, val, val)
        where val = lookup_prev_val track state
    track = tinfo_track tinfo
    use_save_val ((state, _, save_val), result) = ((state, save_val), result)

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
derive_note_track :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Derive.State -> TrackInfo Score.Event -> DeriveResult Score.Event
derive_note_track derive_tracks state tinfo
    | TrackTree.track_inverted (tinfo_track tinfo) =
        derive_inverted state tinfo $
        Derive.state_inversion (Derive.state_dynamic state)
    | otherwise = derive_note_track_ derive_tracks state tinfo

-- This function is split out from 'derive_note_track' to emphasize the shared
-- structure with 'derive_control_track'.
derive_note_track_ :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> Derive.State -> TrackInfo Score.Event
    -> ([Stream.Stream Score.Event], Derive.Threaded, Derive.Collect)
derive_note_track_ derive_tracks state tinfo =
    post_track track $
    List.mapAccumL (derive_note_track_stream derive_tracks tinfo) initial_state
        (event_prev_nexts tinfo)
    where
    initial_state = (record_track_dynamic track state, val)
        where val = lookup_prev_val track state
    track = tinfo_track tinfo

-- I used to use the same function to derive note and control tracks.  However,
-- over time control and note tracks gradually gained separate features, and
-- the function became more complicated to accommodate the differences.
-- Eventually I just split them all into separate functions.  The result is
-- simpler, but there is a certain amount of duplicated code between them.
-- So if you modify one, make sure you also modify the other if applicable.

derive_inverted :: Derive.State -> TrackInfo Score.Event
    -> Derive.Inversion -> DeriveResult Score.Event
derive_inverted state_ tinfo inversion =
    ([levents], threaded, Derive.state_collect next_state)
    where
    (levents, next_state) = run_derive state $ case inversion of
        Derive.NotInverted ->
            Derive.throw "inverted track didn't set state_inversion"
        Derive.InversionInProgress generator -> with_inverted tinfo $
            Derive.state_under_invert (Derive.state_dynamic state) generator
    threaded = stash_prev_val track next_val $ Derive.state_threaded next_state
    prev_val = lookup_prev_val track state_
    next_val = tinfo_prev_val tinfo prev_val levents
    track = tinfo_track tinfo
    state = record_track_dynamic track state_

-- | Update Dynamic before evaluating the inverted generator.
with_inverted :: TrackInfo d -> Derive.Deriver a -> Derive.Deriver a
with_inverted tinfo = Internal.local $ \state ->
    maybe id Internal.add_stack_frame frame $ state
        { Derive.state_inversion = Derive.NotInverted }
    where
    -- The region is redundant, since the uninverted call has already put it on
    -- the stack, but inversion causes the tracks to go on the stack again, and
    -- if I don't put the region on then the [block, track, region] order is
    -- messed up.
    -- TODO should I put the call name on again?  I could stash it in the event
    -- text.
    frame = (\e -> Stack.Region (Event.min e + shifted) (Event.max e + shifted))
        <$> maybe_event
    shifted = TrackTree.track_shifted (tinfo_track tinfo)
    maybe_event = Events.head $ TrackTree.track_events $ tinfo_track tinfo

-- | Extract the final state at the end of a track derivation.
post_track :: Derive.Taggable d => TrackTree.Track
    -> ((Derive.State, Maybe d), a) -> (a, Derive.Threaded, Derive.Collect)
post_track track ((state, save_val), result) =
    ( result
    , stash_prev_val track save_val $ Derive.state_threaded state
    , Derive.state_collect state
    )

-- | Get all event prefixes and suffixes.
event_prev_nexts :: TrackInfo d -> [([Event.Event], [Event.Event])]
event_prev_nexts =
    Seq.zipper [] . Events.ascending . TrackTree.track_events . tinfo_track

-- | Derive one event on a control track.  Carrying previous values forward
-- on a control track is a bit more complicated, because there is a separate
-- next_val and save_val.  The next_val should be the next event's prev_val,
-- and the save_val should be saved as the final next_val at the end of the
-- track.  The reason is that I only save a prev val if the event won't be
-- derived again, e.g.  there's a future event <= the start of the next slice.
-- Otherwise, a sliced event will see its own output as its previous val.
derive_control_track_stream :: Derive.Callable d => TrackInfo d
    -> (Derive.State, Maybe d, Maybe d)
    -> ([Event.Event], [Event.Event])
    -> ((Derive.State, Maybe d, Maybe d), Stream.Stream d)
derive_control_track_stream tinfo (prev_state, prev_val, prev_save_val)
        (prev_events, cur_events) =
    ((state, next_val, save_val), stream)
    where
    (stream, state) = case derivers of
        Just deriver -> run_derive (reset_event_serial prev_state) deriver
        Nothing -> (Stream.empty, prev_state)
    derivers = case cur_events of
        event : next_events -> Just $
            derive_event (context tinfo prev_val prev_events event next_events)
                event
        [] -> Nothing
    next_val = tinfo_prev_val tinfo prev_val stream
    save_val = if should_save_val then next_val else prev_save_val
    should_save_val = case cur_events of
        _ : next : _ ->
            Event.start next <= TrackTree.track_end (tinfo_track tinfo)
        _ -> False

-- | Derive one event on a note track.  This also derives orphan events
-- before the event, or after the last event.
derive_note_track_stream :: (TrackTree.EventsTree -> Derive.NoteDeriver)
    -> TrackInfo Score.Event
    -> (Derive.State, Maybe Score.Event)
    -> ([Event.Event], [Event.Event])
    -> ((Derive.State, Maybe Score.Event), Stream.Stream Score.Event)
derive_note_track_stream derive_tracks tinfo (prev_state, prev_val)
        (prev_events, cur_events) =
    ((next_state, next_val), stream)
    where
    (stream, next_state)
        | null derivers = (Stream.empty, prev_state)
        | otherwise = run_derive (reset_event_serial prev_state)
            (mconcat derivers)
    derivers = Maybe.catMaybes $ case cur_events of
        event : next_events ->
            [ derive_empty (Seq.head prev_events) (Just event)
            , Just $ derive_note event next_events
            ]
        [] -> [derive_empty (Seq.head prev_events) Nothing]
    derive_note event next_events = derive_event ctx event
        where ctx = context tinfo prev_val prev_events event next_events
    next_val = tinfo_prev_val tinfo prev_val stream
    -- Look for orphans in the gap between events.
    derive_empty prev next =
        derive_orphans derive_tracks prev end (tinfo_sub_tracks tinfo)
        where
        end = maybe (TrackTree.track_end (tinfo_track tinfo)) Event.start next

-- | See 'Derive.state_event_serial' for what this is doing.
reset_event_serial :: Derive.State -> Derive.State
reset_event_serial state =
    case Derive.state_inversion (Derive.state_dynamic state) of
        Derive.NotInverted -> state
            { Derive.state_threaded = (Derive.state_threaded state)
                { Derive.state_event_serial = 0 }
            }
        _ -> state

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

-- | Run a derivation.  If the deriver throws an exception, it will be caught
-- and turned into a log msg, and any state changes rolled back.
-- 'Internal.local' relies on this, since it doesn't revert the state after an
-- exception.
run_derive :: Derive.State -> Derive.Deriver (Stream.Stream d)
    -> (Stream.Stream d, Derive.State)
run_derive state deriver = case result of
    Right stream -> (Stream.merge_logs logs stream, out_state)
    Left err -> (Stream.merge_logs logs (error_to_stream err), state)
    where
    error_to_stream = Stream.from_logs . (:[]) . Derive.error_to_warn
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
        Left err -> Just $ Log.warn err >> return Stream.empty
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
record_track_dynamic track state =
    -- TODO I feel like I should merge this with the existing collect, but
    -- profiling shows that it kills performance.  Perhaps I wind up
    -- duplicating the collect and doing a bunch of extra merging every
    -- (inverted) event.  I'm not sure at the moment, but it should be safe to
    -- start with an empty collect anyway.
    state { Derive.state_collect = collect }
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
    defragment = unwarp warp . Signal.merge . Map.elems

unwarp :: Score.Warp -> Signal.Control -> Track.TrackSignal
unwarp warp control = case is_linear_warp warp of
    Just (shift, stretch) ->
        Track.TrackSignal (Signal.coerce control) (RealTime.to_score shift)
            (RealTime.to_score stretch)
    Nothing -> Track.TrackSignal unwarped 0 1
        where
        Score.Warp warp_sig shift stretch = warp
        unwarped = Signal.unwarp_fused warp_sig shift stretch control

-- | Return (shift, stretch) if the tempo is linear.  This relies on an
-- optimization in 'Tempo.with_tempo' to notice when the tempo is constant and
-- give it 'Score.id_warp_signal'.
is_linear_warp :: Score.Warp -> Maybe (RealTime, RealTime)
is_linear_warp warp
    | Score.warp_signal warp == Score.id_warp_signal =
        Just (Score.warp_shift warp, Score.warp_stretch warp)
    | otherwise = Nothing

derive_event :: Derive.Callable d => Derive.Context d -> Event.Event
    -> Derive.Deriver (Stream.Stream d)
derive_event ctx event
    | "--" `Text.isPrefixOf` Text.dropWhile (==' ') text = return Stream.empty
    | otherwise = with_event_region (Derive.ctx_track_shifted ctx) event $
        case Parse.parse_expr text of
            Left err -> Log.warn err >> return Stream.empty
            Right expr -> with_note_start_end $ Eval.eval_toplevel ctx expr
    where
    text = Event.text event
    with_note_start_end = case Derive.ctx_track_type ctx of
        Just ParseTitle.NoteTrack -> Derive.with_vals
                [ (EnvKey.note_start, Event.start event)
                , (EnvKey.note_end, Event.end event)
                ]
            where event = Derive.ctx_event ctx
        _ -> id

with_event_region :: ScoreTime -> Event.Event -> Derive.Deriver a
    -> Derive.Deriver a
with_event_region track_shifted event =
    Internal.with_stack_region (Event.min event + track_shifted)
        (Event.max event + track_shifted)

context :: TrackInfo a -> Maybe val
    -> [Event.Event] -- ^ previous events, in reverse order
    -> Event.Event -- ^ cur event
    -> [Event.Event] -- ^ following events
    -> Derive.Context val
context tinfo prev_val prev event next = Derive.Context
    { Derive.ctx_prev_val = prev_val
    , Derive.ctx_event = event
    -- Augment prev and next with the unevaluated "around" notes from
    -- 'State.track_around'.
    , Derive.ctx_prev_events = tprev ++ prev
    , Derive.ctx_next_events = next ++ tnext
    , Derive.ctx_event_end = case next ++ tnext of
        [] -> TrackTree.track_end track
        event : _ -> Event.start event
    , Derive.ctx_track_shifted = TrackTree.track_shifted track
    , Derive.ctx_sub_tracks = subs
    , Derive.ctx_sub_events = Nothing
    , Derive.ctx_track_type = Just ttype
    }
    where
    TrackInfo track subs ttype _ = tinfo
    (tprev, tnext) = TrackTree.track_around track
