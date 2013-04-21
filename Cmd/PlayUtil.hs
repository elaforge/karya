-- | Functions to do with performance.  This is split off from "Cmd.Play",
-- which contains play Cmds and their direct support.
module Cmd.PlayUtil where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Derive.Cache as Cache
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Instrument.MidiDb as MidiDb
import Types


-- | There are a few environ values that almost everything relies on.
initial_environ :: Pitch.ScaleId -> Maybe Score.Instrument -> TrackLang.Environ
initial_environ scale_id maybe_inst = TrackLang.make_environ $
    inst ++
    -- Control interpolators rely on this.
    [ (TrackLang.v_srate, TrackLang.num 0.02)
    -- Looking up any val call relies on having a scale in scope.
    , (TrackLang.v_scale, TrackLang.VScaleId scale_id)
    , (TrackLang.v_attributes, TrackLang.VAttributes Score.no_attrs)
    , (TrackLang.v_seed, TrackLang.num 0)
    ]
    where
    inst = case maybe_inst of
        Just inst -> [(TrackLang.v_instrument, TrackLang.VInstrument inst)]
        Nothing -> []

-- | Derive with the cache.
cached_derive :: (Cmd.M m) => BlockId -> m Derive.Result
cached_derive block_id = do
    (cache, damage) <- get_derive_cache block_id <$>
        Cmd.lookup_performance block_id
    -- Log.debug $ "rederiving with score damage: " ++ show damage
    derive cache damage block_id

uncached_derive :: (Cmd.M m) => BlockId -> m Derive.Result
uncached_derive = derive mempty mempty

clear_cache :: (Cmd.M m) => BlockId -> m ()
clear_cache block_id = Cmd.modify_play_state $ \st -> st
    { Cmd.state_performance = Map.delete block_id (Cmd.state_performance st)
    -- Must remove this too or it won't want to rederive.
    , Cmd.state_performance_threads =
        Map.delete block_id (Cmd.state_performance_threads st)
    }

-- | Derive the contents of the given block to score events.
derive :: (Cmd.M m) => Derive.Cache -> Derive.ScoreDamage -> BlockId
    -> m Derive.Result
derive cache damage block_id = do
    global_transform <- State.config#State.global_transform <#> State.get
    Derive.extract_result <$> run cache damage
        (Call.Block.eval_root_block global_transform block_id)

run :: (Cmd.M m) => Derive.Cache -> Derive.ScoreDamage
    -> Derive.Deriver a -> m (Derive.RunResult a)
run cache damage deriver = do
    ui_state <- State.get
    scope <- Cmd.gets (Cmd.state_global_scope . Cmd.state_config)
    constant <- make_constant ui_state cache damage
    env <- make_environ
    return $ Derive.derive constant scope env deriver

make_environ :: (State.M m) => m TrackLang.Environ
make_environ = do
    deflt <- State.get_default id
    return $ initial_environ (State.default_scale deflt)
        (State.default_instrument deflt)

make_constant :: (Cmd.M m) => State.State -> Derive.Cache -> Derive.ScoreDamage
    -> m Derive.Constant
make_constant ui_state cache damage = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- get_lookup_inst
    return $ Derive.initial_constant
        ui_state lookup_scale lookup_inst cache damage

-- | Run a derivation when you already know the Dynamic.  This is the case when
-- deriving at a certain point in the score via the TrackDynamic.
run_with_dynamic :: (Cmd.M m) => Derive.Dynamic -> Derive.Deriver a
    -> m (Derive.RunResult a)
run_with_dynamic dynamic deriver = do
    ui_state <- State.get
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- get_lookup_inst
    let constant = Derive.initial_constant ui_state lookup_scale lookup_inst
            mempty mempty
    let state = Derive.State dynamic mempty constant
    return $ Derive.run state deriver

get_lookup_inst :: (Cmd.M m) => m (Score.Instrument -> Maybe Derive.Instrument)
get_lookup_inst =
    (fmap Cmd.derive_instrument .) <$> Cmd.get_lookup_instrument

perform_from :: (Cmd.M m) => RealTime -> Cmd.Performance
    -> m Perform.MidiEvents
perform_from start = perform_events . events_from start . Cmd.perf_events

shift_messages :: RealTime -> RealTime -> Perform.MidiEvents
    -> Perform.MidiEvents
shift_messages multiplier start events = shift start events
    where
    shift offset = map $ fmap $
        Midi.modify_timestamp ((* multiplier) . subtract offset)

first_time :: LEvent.LEvents Midi.WriteMessage -> RealTime
first_time msgs = case LEvent.events_of msgs of
    event : _ -> Midi.wmsg_ts event
    [] -> 0

-- | As a special case, a start <= 0 will get all events, including negative
-- ones.  This is so notes pushed before 0 won't be clipped on a play from 0.
--
-- Cache log msgs are emitted even if they are before @start@ because otherwise
-- you never see the cache status unless you play from the beginning.  Cache
-- msgs tend to show up first because 'Derive.d_merge' puts logs before events
-- and all the events on a track are merged.
events_from :: RealTime -> Derive.Events -> Derive.Events
events_from start_ events
    | start <= 0 = events
    | otherwise = go [] events
    where
    start = start_ - RealTime.eta
    go _ [] = []
    go logs (e@(LEvent.Event event) : es)
        | Score.event_start event >= start = reverse logs ++ e : es
        | otherwise = go [] es
    go logs (e@(LEvent.Log msg) : es)
        | Cache.is_cache_log msg = e : go logs es
        | otherwise = go (e:logs) es

-- | Filter events according to the Solo and Mute flags in the tracks of the
-- given blocks.
--
-- Solo only applies to the block on which the track is soloed.  So if you solo
-- a track on one block, other blocks will still play.
--
-- Solo always takes priority over Mute.
filter_muted :: [(BlockId, Block.Block)] -> Derive.Events -> Derive.Events
filter_muted blocks
    | not (Set.null soloed) = filter (LEvent.log_or track_soloed)
    | not (Set.null muted) =
        filter (LEvent.log_or $ not . stack_contains muted)
    | otherwise = id
    where
    stack_contains track_ids = any (`Set.member` track_ids) . stack_tracks
    stack_tracks = mapMaybe Stack.track_of . Stack.innermost . Score.event_stack
    stack_blocks = mapMaybe Stack.block_of . Stack.innermost . Score.event_stack
    soloed = with_flag Block.Solo
    muted = with_flag Block.Mute
    with_flag flag = Set.fromList
        [ track_id
        | (_, block) <- blocks
        , track <- Block.block_tracks block
        , Just track_id <- [Block.track_id_of (Block.tracklike_id track)]
        , flag `Set.member` Block.track_flags track
        ]

    -- Solo is trickier than it seems.  Firstly, it doesn't work to just mute
    -- non-soloed tracks, because that winds up muting the parent track.
    -- Secondly, if I just filter out non-soloed events then other blocks are
    -- totally muted, and I'd like to leave them alone.  So always emit events
    -- on blocks that don't have any solo tracks.
    track_soloed e = block_not_soloed e || stack_contains soloed e
    block_not_soloed = maybe True (`Set.notMember` soloed_blocks)
        . Seq.head . stack_blocks
    soloed_blocks = Set.fromList
        [ block_id
        | (block_id, block) <- blocks
        , any ((Block.Solo `Set.member`) . Block.track_flags)
            (Block.block_tracks block)
        ]

perform_events :: (Cmd.M m) => Derive.Events -> m Perform.MidiEvents
perform_events events = do
    midi_config <- State.get_midi_config
    lookup <- get_convert_lookup
    blocks <- State.gets (Map.toList . State.state_blocks)
    return $ fst $ Perform.perform Perform.initial_state midi_config $
        Convert.convert lookup (filter_muted blocks events)

get_convert_lookup :: (Cmd.M m) => m Convert.Lookup
get_convert_lookup = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    lookup_info <- Cmd.get_lookup_instrument
    return $ Convert.Lookup lookup_scale lookup_inst
        (fmap MidiDb.info_patch . lookup_info)

-- * util

get_derive_cache :: BlockId -> Maybe Cmd.Performance
    -> (Derive.Cache, Derive.ScoreDamage)
get_derive_cache block_id Nothing =
    -- If a block is being derived the first time, its considered damaged.
    -- Hack needed by 'Derive.Call.Integrate.only_destinations_damaged '.
    (mempty, mempty { Derive.sdamage_blocks = Set.singleton block_id })
get_derive_cache _ (Just perf) =
    (Cmd.perf_derive_cache perf, Cmd.perf_score_damage perf)
