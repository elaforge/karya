-- | Functions to do with performance.  This is split off from the lower level
-- "Cmd.Play" because Play uses Sync, which eventually imports C, which makes
-- it a pain to use from ghci.
--
-- TODO needs a better name
module Cmd.PlayUtil where
import qualified Data.Map as Map
import Util.Control
import qualified Util.Seq as Seq

import Ui
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd

import qualified Derive.Call.Block as Block
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Perform as Perform

import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent

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
uncached_derive = derive mempty mempty

clear_cache :: (Cmd.M m) => BlockId -> m ()
clear_cache block_id =
    Cmd.modify_state $ \st -> st { Cmd.state_performance_threads =
        Map.delete block_id (Cmd.state_performance_threads st) }

-- | Derive the contents of the given block to score events.
derive :: (Cmd.M m) => Derive.Cache -> Derive.ScoreDamage -> BlockId
    -> m Derive.Result
derive cache damage block_id =
    Derive.extract_result <$> run cache damage (Block.eval_root_block block_id)

run :: (Cmd.M m) => Derive.Cache -> Derive.ScoreDamage
    -> Derive.Deriver a -> m (Derive.RunResult a)
run cache damage deriver = do
    schema_map <- Cmd.get_schema_map
    ui_state <- State.get
    lookup_scale <- Cmd.get_lookup_scale
    inst_calls <- get_lookup_inst_calls
    let constant = Derive.initial_constant ui_state
            (Schema.lookup_deriver schema_map ui_state) lookup_scale inst_calls
            cache damage
    scope <- Cmd.gets Cmd.state_global_scope
    let deflt = State.state_default ui_state
        env = initial_environ (State.default_scale deflt)
            (State.default_instrument deflt)
    return $ Derive.derive constant scope env deriver

get_lookup_inst_calls :: (Cmd.M m) =>
    m (Score.Instrument -> Maybe Derive.InstrumentCalls)
get_lookup_inst_calls = do
    inst_db <- Cmd.gets Cmd.state_instrument_db
    return $ fmap (Cmd.inst_calls . MidiDb.info_code)
        . Instrument.Db.db_lookup inst_db

perform_from :: (Cmd.M m) => Cmd.Performance -> RealTime
    -> m Perform.MidiEvents
perform_from perf start =
    perform_events (events_from start (Cmd.perf_events perf))

events_from :: RealTime -> Derive.Events -> Derive.Events
events_from start =
    map (fmap (\e -> e { Score.event_start = Score.event_start e - start }))
    . Seq.drop_unknown -- keep log msgs before an event after 'start'
        (LEvent.either (Just . (<start) . Score.event_start) (const Nothing))

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

get_derive_cache :: Maybe Cmd.Performance -> (Derive.Cache, Derive.ScoreDamage)
get_derive_cache Nothing = (mempty, mempty)
get_derive_cache (Just perf) =
    (Cmd.perf_derive_cache perf, Cmd.perf_score_damage perf)
