-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to do with performance.  This is split off from "Cmd.Play",
-- which contains play Cmds and their direct support.
module Cmd.PlayUtil where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.Vector as Vector

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.MidiDb as MidiDb
import qualified App.Config as Config
import Types


-- | There are a few environ values that almost everything relies on.
initial_environ :: TrackLang.Environ
initial_environ = TrackLang.make_environ $
    -- Control interpolators rely on this.
    [ (Environ.srate, TrackLang.num 0.02)
    -- Looking up any val call relies on having a scale in scope.
    , (Environ.scale, TrackLang.VSymbol
        (TrackLang.Symbol Config.default_scale_id))
    , (Environ.attributes, TrackLang.VAttributes Score.no_attrs)
    , (Environ.seed, TrackLang.num 0)
    ]

-- | Derive with the cache.
cached_derive :: (Cmd.M m) => BlockId -> m Derive.Result
cached_derive block_id = do
    (cache, damage) <- get_derive_cache block_id <$>
        Cmd.lookup_performance block_id
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

clear_all_caches :: (Cmd.M m) => m ()
clear_all_caches = Cmd.modify_play_state $ \st -> st
    { Cmd.state_performance = mempty
    , Cmd.state_performance_threads = mempty
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
    return $ Derive.derive constant scope initial_environ deriver

make_constant :: (Cmd.M m) => State.State -> Derive.Cache -> Derive.ScoreDamage
    -> m Derive.Constant
make_constant ui_state cache damage = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- get_lookup_inst
    wants_signal <- get_wants_track_signal
    return $ Derive.initial_constant
        ui_state wants_signal lookup_scale lookup_inst cache damage

-- | Get the set of tracks that want to render a signal.
get_wants_track_signal :: (Cmd.M m) => m (Set.Set (BlockId, TrackId))
get_wants_track_signal = do
    block_ids <- State.all_block_ids
    mconcat <$> mapM get block_ids
    where
    get block_id = do
        track_flags <- map (second Block.track_flags)
            . Seq.map_filter (Block.track_id_of . Block.tracklike_id)
            . Block.block_tracks <$> State.get_block block_id
        tracks <- mapM (State.get_track . fst) track_flags
        return $ Set.fromList
            [ (block_id, track_id)
            | ((track_id, flags), track) <- zip track_flags tracks
            , Block.wants_track_signal flags track
            ]

-- | Run a derivation when you already know the Dynamic.  This is the case when
-- deriving at a certain point in the score via the TrackDynamic.
run_with_dynamic :: (Cmd.M m) => Derive.Dynamic -> Derive.Deriver a
    -> m (Derive.RunResult a)
run_with_dynamic dynamic deriver = do
    ui_state <- State.get
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- get_lookup_inst
    let constant = Derive.initial_constant ui_state mempty
            lookup_scale lookup_inst mempty mempty
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
events_from :: RealTime -> Cmd.Events -> Cmd.Events
events_from start events
    | start <= 0 = events
    | otherwise = Vector.drop i events
    where
    i = Vector.lowest_index Score.event_start (start - RealTime.eta) events

-- | How to know how far back to go?  Impossible to know!  Well, I could look
-- up overlapping ui events, then map the earliest time to RealTime, and start
-- searching there.  But for now scanning from the beginning should be fast
-- enough.
overlapping_events :: RealTime -> Cmd.Events -> [Score.Event]
overlapping_events pos = Vector.foldl' collect []
    where
    collect overlap event
        | Score.event_end event <= pos || Score.event_start event > pos =
            overlap
        | otherwise = event : overlap

-- | Filter events according to the Solo and Mute flags in the tracks of the
-- given blocks.
--
-- Solo only applies to the block on which the track is soloed.  So if you solo
-- a track on one block, other blocks will still play.
--
-- Solo takes priority over Mute.
filter_track_muted :: [(BlockId, Block.Block)] -> [Score.Event] -> [Score.Event]
filter_track_muted blocks
    | not (Set.null soloed) = filter track_soloed
    | not (Set.null muted) = filter (not . stack_contains muted)
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

-- | Similar to the Solo and Mute track flags, individual instruments can be
-- soloed or muted.
filter_instrument_muted :: Instrument.Configs -> [Score.Event] -> [Score.Event]
filter_instrument_muted configs
    | not (Set.null soloed) = filter $
        (`Set.member` soloed) . Score.event_instrument
    | not (Set.null muted) = filter $
        (`Set.notMember` muted) . Score.event_instrument
    | otherwise = id
    where
    soloed = Set.fromList $ map fst $ filter (Instrument.config_solo . snd) $
        Map.toList configs
    muted = Set.fromList $ map fst $ filter (Instrument.config_mute . snd) $
        Map.toList configs

perform_events :: (Cmd.M m) => Cmd.Events -> m Perform.MidiEvents
perform_events events = do
    midi_config <- State.get_midi_config
    lookup <- get_convert_lookup
    blocks <- State.gets (Map.toList . State.state_blocks)
    return $ fst $ Perform.perform Perform.initial_state midi_config $
        Convert.convert lookup $ filter_track_muted blocks $
        filter_instrument_muted midi_config $
        -- Performance should be lazy, so converting to a list here means I can
        -- avoid doing work for the notes that never get played.
        Vector.toList events

get_convert_lookup :: (Cmd.M m) => m Convert.Lookup
get_convert_lookup = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    lookup_info <- Cmd.get_lookup_instrument
    configs <- State.get_midi_config
    let defaults = Map.map (Map.map (Score.untyped . Signal.constant)
            . Instrument.config_controls) configs
    return $ Convert.Lookup
        { Convert.lookup_scale = lookup_scale
        , Convert.lookup_inst = lookup_inst
        , Convert.lookup_patch = fmap MidiDb.info_patch . lookup_info
        , Convert.lookup_default_controls = \inst ->
            Map.findWithDefault mempty inst defaults
        }

-- * util

get_derive_cache :: BlockId -> Maybe Cmd.Performance
    -> (Derive.Cache, Derive.ScoreDamage)
get_derive_cache block_id Nothing =
    -- If a block is being derived the first time, its considered damaged.
    -- Hack needed by 'Derive.Call.Integrate.only_destinations_damaged '.
    (mempty, mempty { Derive.sdamage_blocks = Set.singleton block_id })
get_derive_cache _ (Just perf) =
    (Cmd.perf_derive_cache perf, Cmd.perf_score_damage perf)
