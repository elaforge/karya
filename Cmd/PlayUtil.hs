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

import Ui
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Cache as Midi.Cache
import qualified Perform.Midi.Instrument as Instrument
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Warning as Warning

import qualified Instrument.MidiDb as MidiDb


-- | There are a few environ values that almost everything relies on.
initial_environ :: TrackLang.Environ
initial_environ = Map.fromList
    -- Control interpolators rely on this.
    [ (TrackLang.v_srate, TrackLang.VNum 0.05)
    -- Looking up any val call relies on this.
    , (TrackLang.v_scale, TrackLang.VScale Twelve.scale)
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
    lookup_inst <- Cmd.get_lookup_midi_instrument
    midi_cache <- get_midi_cache lookup_inst midi_config <$>
        Cmd.lookup_performance block_id
    perform result midi_cache

uncached_perform :: (Monad m) => Derive.Result Derive.Events
    -> Cmd.CmdT m Cmd.Performance
uncached_perform result = do
    midi_config <- State.get_midi_config
    lookup_inst <- Cmd.get_lookup_midi_instrument
    perform result (Midi.Cache.cache lookup_inst midi_config)

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
    call_map <- Cmd.gets Cmd.state_call_map
    return $ Derive.derive derive_cache damage
        (Schema.lookup_deriver schema_map ui_state)
        ui_state call_map initial_environ False
        (Call.eval_root_block block_id)

-- | Convert a block ID into MIDI msgs and log msgs.  The logs are not
-- immediately written to preserve laziness on the returned MIDI msgs.
-- This is actually called from ResponderSync, when it kicks off background
-- derivation.  By the time 'cmd_play' pulls out the Performance, it should be
-- at least partially evaluated.
perform :: (Monad m) => Derive.Result Derive.Events -> Midi.Cache.Cache
    -> Cmd.CmdT m Cmd.Performance
perform result cache = do
    events <- case Derive.r_result result of
        Left (Derive.DeriveError srcpos stack msg) -> do
            msg <- Log.msg_srcpos srcpos Log.Warn ("deriving: " ++ msg)
            Log.write $ msg { Log.msg_stack = Just stack }
            Cmd.abort
        Right events -> return events

    let (midi_events, convert_warnings) =
            Convert.convert (Midi.Cache.cache_lookup cache) events
    -- TODO call Convert.verify for more warnings

    let Derive.EventDamage event_damage = Derive.r_event_damage result
        new_midi_cache = Midi.Cache.perform cache
                (Midi.Cache.EventDamage event_damage) midi_events
    warn_logs <- mapM (warn_to_log "convert") convert_warnings
    let logs = Derive.r_logs result ++ warn_logs
    return $ Cmd.Performance
        (Derive.r_cache result) new_midi_cache Monoid.mempty
        logs (Derive.r_tempo result) (Derive.r_inv_tempo result)
        (Derive.r_track_signals result)


-- * util

-- | The MIDI cache depends on the inst lookup function and inst config.  If
-- either of those change, it has to be cleared.
--
-- I can't compare functions so I just have to make sure to reinitialize the
-- MIDI cache when the function changes (which should be rare if ever).  The
-- instrument config /can/ be compared, so I just compare on play, and clear
-- the cache if it's changed.
get_midi_cache :: MidiDb.LookupMidiInstrument -> Instrument.Config
    -> Maybe Cmd.Performance -> Midi.Cache.Cache
get_midi_cache lookup_inst config maybe_perf = case maybe_perf of
    Nothing -> empty
    Just perf ->
        let cache = Cmd.perf_midi_cache perf
        in if config == Midi.Cache.cache_config cache then cache else empty
    where
    empty = Midi.Cache.cache lookup_inst config

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
