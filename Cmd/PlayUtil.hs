-- | Functions to do with performance.  This is split off from the lower level
-- "Cmd.Play" because Play uses Sync, which eventually imports C, which makes
-- it a pain to use from ghci.
--
-- TODO needs a better name
module Cmd.PlayUtil where
import qualified Data.Monoid as Monoid
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

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
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Warning as Warning

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb


-- | There are a few environ values that almost everything relies on.
initial_environ :: TrackLang.Environ
initial_environ = Map.fromList
    -- Control interpolators rely on this.
    [ (TrackLang.v_srate, TrackLang.VNum 0.05)
    -- Looking up any val call relies on having a scale in scope.
    , (TrackLang.v_scale, TrackLang.VScaleId Twelve.scale_id)
    , (TrackLang.v_attributes, TrackLang.VAttributes Score.no_attrs)
    ]

-- | Derive with the cache.
cached_derive :: (Monad m) =>
    BlockId -> Cmd.CmdT m (Derive.Result [Score.Event])
cached_derive block_id = do
    (cache, damage) <- get_derive_cache <$> Cmd.lookup_performance block_id
    -- Log.debug $ "rederiving with score damage: " ++ show damage
    derive cache damage block_id

uncached_derive :: (Monad m) =>
    BlockId -> Cmd.CmdT m (Derive.Result [Score.Event])
uncached_derive block_id = derive Derive.empty_cache Monoid.mempty block_id

cached_perform :: (Monad m) =>
    BlockId -> Derive.Result Derive.Events -> Cmd.CmdT m Cmd.Performance
cached_perform block_id result = do
    midi_config <- State.get_midi_config
    midi_cache <- get_midi_cache midi_config <$> Cmd.lookup_performance block_id
    perform result midi_cache

uncached_perform :: (Monad m) => Derive.Result Derive.Events
    -> Cmd.CmdT m Cmd.Performance
uncached_perform result = do
    midi_config <- State.get_midi_config
    perform result (Midi.Cache.cache midi_config)

clear_cache :: (Monad m) => BlockId -> Cmd.CmdT m ()
clear_cache block_id =
    Cmd.modify_state $ \st -> st { Cmd.state_performance_threads =
        Map.delete block_id (Cmd.state_performance_threads st) }

-- | Derive the contents of the given block to score events.
derive :: (Monad m) => Derive.Cache -> Derive.ScoreDamage
    -> BlockId -> Cmd.CmdT m (Derive.Result [Score.Event])
derive derive_cache damage block_id = do
    schema_map <- Cmd.get_schema_map
    ui_state <- State.get
    lookup_scale <- Cmd.get_lookup_scale
    inst_calls <- get_lookup_inst_calls
    let constant = Derive.initial_constant ui_state
            (Schema.lookup_deriver schema_map ui_state) lookup_scale
            inst_calls False
    scopes <- Cmd.gets Cmd.state_global_scopes
    return $ Derive.derive constant scopes derive_cache damage
        initial_environ (Call.eval_root_block block_id)

get_lookup_inst_calls :: (Monad m) =>
    Cmd.CmdT m (Score.Instrument -> Maybe Derive.InstrumentCalls)
get_lookup_inst_calls = do
    inst_db <- Cmd.gets Cmd.state_instrument_db
    return $ fmap MidiDb.info_inst_calls . Instrument.Db.db_lookup inst_db

-- | Convert a block ID into MIDI msgs and log msgs.  The logs are not
-- immediately written to preserve laziness on the returned MIDI msgs.
-- This is actually called from ResponderSync, when it kicks off background
-- derivation.  By the time 'cmd_play' pulls out the Performance, it should be
-- at least partially evaluated.
perform :: (Monad m) => Derive.Result Derive.Events -> Midi.Cache.Cache
    -> Cmd.CmdT m Cmd.Performance
perform result cache = do
    events <- case Derive.r_result result of
        Left err -> do
            Log.write (Derive.error_to_warn err)
            Cmd.abort
        Right events -> return events

    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    let (midi_events, convert_warnings) =
            Convert.convert lookup_scale lookup_inst events
    -- TODO call Convert.verify for more warnings

    let Derive.EventDamage event_damage = Derive.r_event_damage result
        new_midi_cache = Midi.Cache.perform cache
                (Midi.Cache.EventDamage event_damage) midi_events
    warn_logs <- mapM (warn_to_log "convert") convert_warnings
    let logs = Derive.r_logs result ++ warn_logs
    return $ Cmd.Performance
        (Derive.r_cache result) new_midi_cache Monoid.mempty
        logs (Derive.r_tempo result) (Derive.r_closest_warp result)
        (Derive.r_inv_tempo result) (Derive.r_track_signals result)

-- | Perform some events with no caching or anything.  For interactive
-- debugging.
perform_events :: (Monad m) => Derive.Events
    -> Cmd.CmdT m (Perform.Messages, [String])
perform_events events = do
    lookup_scale <- Cmd.get_lookup_scale
    lookup_inst <- Cmd.get_lookup_midi_instrument
    midi_config <- State.get_midi_config
    let (midi_events, convert_warnings) =
            Convert.convert lookup_scale lookup_inst events
        (msgs, perf_warns, _) =
            Perform.perform Perform.initial_state midi_config midi_events
    convert_logs <- mapM (warn_to_log "convert") convert_warnings
    perf_logs <- mapM (warn_to_log "perform") perf_warns
    return (msgs, map Pretty.pretty (convert_logs ++ perf_logs))

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
get_derive_cache Nothing = (Derive.empty_cache, Monoid.mempty)
get_derive_cache (Just perf) =
    (Cmd.perf_derive_cache perf, Cmd.perf_score_damage perf)

-- | Convert a Warning into an appropriate log msg.
warn_to_log :: (Log.LogMonad m) => String -> Warning.Warning -> m Log.Msg
warn_to_log context (Warning.Warning msg event_stack maybe_range) = do
    log <- Log.msg Log.Warn $ context ++ ": " ++ msg
        -- TODO It would be more useful to append this to the stack, but I have
        -- to convert real -> score.
        ++ maybe "" ((" range: " ++) . show) maybe_range
    return $ log { Log.msg_stack = Just event_stack }
