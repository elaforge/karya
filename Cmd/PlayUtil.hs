-- | Functions to do with performance.  This is split off from the lower level
-- "Cmd.Play" because Play uses Sync, which eventually imports C, which makes
-- it a pain to use from ghci.
--
-- TODO needs a better name
module Cmd.PlayUtil where
import qualified Data.Map as Map
import Util.Control

import Ui
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd

import qualified Derive.Call as Call
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Cache as Midi.Cache
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform

import qualified Derive.Derive as Derive
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb


-- | There are a few environ values that almost everything relies on.
initial_environ :: Pitch.ScaleId -> Maybe Score.Instrument -> TrackLang.Environ
initial_environ scale_id maybe_inst = Map.fromList $
    inst ++
    -- Control interpolators rely on this.
    [ (TrackLang.v_srate, TrackLang.VNum 0.05)
    -- Looking up any val call relies on having a scale in scope.
    , (TrackLang.v_scale, TrackLang.VScaleId scale_id)
    , (TrackLang.v_attributes, TrackLang.VAttributes Score.no_attrs)
    ]
    where
    inst = case maybe_inst of
        Just inst -> [(TrackLang.v_instrument, TrackLang.VInstrument inst)]
        Nothing -> []

-- | Derive with the cache.
cached_derive :: (Cmd.M m) => BlockId -> m Derive.Result
cached_derive block_id = do
    (cache, damage) <- get_derive_cache <$> Cmd.lookup_performance block_id
    -- Log.debug $ "rederiving with score damage: " ++ show damage
    derive cache damage block_id

uncached_derive :: (Cmd.M m) => BlockId -> m Derive.Result
uncached_derive block_id = derive mempty mempty block_id

cached_perform :: (Cmd.M m) => BlockId -> Derive.Result -> m Cmd.Performance
cached_perform block_id result = do
    midi_config <- State.get_midi_config
    midi_cache <- get_midi_cache midi_config <$> Cmd.lookup_performance block_id
    perform block_id result midi_cache

uncached_perform :: (Cmd.M m) => BlockId -> Derive.Result -> m Cmd.Performance
uncached_perform block_id result = do
    midi_config <- State.get_midi_config
    perform block_id result (Midi.Cache.cache midi_config)

clear_cache :: (Cmd.M m) => BlockId -> m ()
clear_cache block_id =
    Cmd.modify_state $ \st -> st { Cmd.state_performance_threads =
        Map.delete block_id (Cmd.state_performance_threads st) }

-- | Derive the contents of the given block to score events.
derive :: (Cmd.M m) => Derive.Cache -> Derive.ScoreDamage -> BlockId
    -> m Derive.Result
derive derive_cache damage block_id = do
    schema_map <- Cmd.get_schema_map
    ui_state <- State.get
    lookup_scale <- Cmd.get_lookup_scale
    inst_calls <- get_lookup_inst_calls
    let constant = Derive.initial_constant ui_state
            (Schema.lookup_deriver schema_map ui_state) lookup_scale inst_calls
    scopes <- Cmd.gets Cmd.state_global_scopes
    let env = initial_environ (State.state_project_scale ui_state)
            (State.state_default_inst ui_state)
    return $ Derive.derive constant scopes derive_cache damage env
        (Call.eval_root_block block_id)

get_lookup_inst_calls :: (Cmd.M m) =>
    m (Score.Instrument -> Maybe Derive.InstrumentCalls)
get_lookup_inst_calls = do
    inst_db <- Cmd.gets Cmd.state_instrument_db
    return $ fmap MidiDb.info_inst_calls . Instrument.Db.db_lookup inst_db

-- | Convert a block ID into MIDI msgs and log msgs.  The logs are not
-- immediately written to preserve laziness on the returned MIDI msgs.
-- This is actually called from ResponderSync, when it kicks off background
-- derivation.  By the time 'cmd_play' pulls out the Performance, it should be
-- at least partially evaluated.
perform :: (Cmd.M m) => BlockId -> Derive.Result -> Midi.Cache.Cache
    -> m Cmd.Performance
perform block_id result cache = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    let midi_events = Convert.convert lookup_scale lookup_inst
            (Derive.r_events result)

    let Derive.EventDamage event_damage = Derive.r_event_damage result
        new_cache = Midi.Cache.perform (Stack.block block_id) cache
                (Midi.Cache.EventDamage event_damage) midi_events
    return $ Cmd.Performance
        (Derive.r_cache result) new_cache (Derive.r_track_environ result) mempty
        (Derive.r_tempo result) (Derive.r_closest_warp result)
        (Derive.r_inv_tempo result) (Derive.r_track_signals result)

-- | Perform some events with no caching or anything.  For interactive
-- debugging.
perform_events :: (Cmd.M m) => Derive.Events -> m Perform.MidiEvents
perform_events events = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    midi_config <- State.get_midi_config
    return $ fst $ Perform.perform Perform.initial_state midi_config $
        Convert.convert lookup_scale lookup_inst events

-- * util

-- | The MIDI cache depends on the inst config.  If it changes, the cache must
-- be cleared.
get_midi_cache :: Instrument.Config -> Maybe Cmd.Performance -> Midi.Cache.Cache
get_midi_cache config maybe_perf = case maybe_perf of
    Nothing -> empty
    Just perf ->
        let cache = Cmd.perf_midi_cache perf
        in if config == Midi.Cache.cache_config cache then cache else empty
    where empty = Midi.Cache.cache config

get_derive_cache :: Maybe Cmd.Performance -> (Derive.Cache, Derive.ScoreDamage)
get_derive_cache Nothing = (mempty, mempty)
get_derive_cache (Just perf) =
    (Cmd.perf_derive_cache perf, Cmd.perf_score_damage perf)
