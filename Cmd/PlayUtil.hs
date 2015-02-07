-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to do with performance.  This is split off from "Cmd.Play",
-- which contains play Cmds and their direct support.
module Cmd.PlayUtil (
    initial_environ
    , cached_derive, uncached_derive
    , clear_cache, clear_caches
    , derive_block, run, run_with_dynamic
    , is_score_damage_log
    , get_constant
    -- * perform
    , perform_from, shift_messages, first_time
    , events_from, overlapping_events
    , perform_events, get_convert_lookup
    -- * definition file
    , update_definition_cache
    , load_ky
    , compile_library
) where
import qualified Control.Monad.Error as Error
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Util.Tree as Tree
import qualified Util.Vector as Vector

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.Library as Library
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.MidiDb as MidiDb
import qualified App.Config as Config
import Global
import Types


-- | There are a few environ values that almost everything relies on.
initial_environ :: TrackLang.Environ
initial_environ = TrackLang.make_environ $
    -- Control interpolators rely on this.
    [ (Environ.srate, TrackLang.num 0.015)
    -- Looking up any val call relies on having a scale in scope.
    , (Environ.scale, TrackLang.VSymbol
        (TrackLang.Symbol Config.default_scale_id))
    , (Environ.attributes, TrackLang.VAttributes Score.no_attrs)
    , (Environ.seed, TrackLang.num 0)
    ]

-- | Derive with the cache.
cached_derive :: Cmd.M m => BlockId -> m Derive.Result
cached_derive block_id = do
    maybe_perf <- Cmd.lookup_performance block_id
    case maybe_perf of
        Nothing -> uncached_derive block_id
        Just perf -> derive_block True (Cmd.perf_derive_cache perf)
            (Cmd.perf_damage perf) block_id

uncached_derive :: Cmd.M m => BlockId -> m Derive.Result
uncached_derive = derive_block True mempty mempty

clear_cache :: Cmd.M m => BlockId -> m ()
clear_cache block_id = Cmd.modify_play_state $ \st -> st
    { Cmd.state_performance = delete (Cmd.state_performance st)
    , Cmd.state_current_performance = delete (Cmd.state_current_performance st)
    -- Must remove this too or it won't want to rederive.
    , Cmd.state_performance_threads = delete (Cmd.state_performance_threads st)
    }
    where delete = Map.delete block_id

clear_caches :: Cmd.M m => m ()
clear_caches = Cmd.modify_play_state $ \st -> st
    { Cmd.state_performance = mempty
    , Cmd.state_current_performance = mempty
    , Cmd.state_performance_threads = mempty
    }

-- | Derive the contents of the given block to score events.
derive_block :: Cmd.M m => Bool -> Derive.Cache -> Derive.ScoreDamage
    -> BlockId -> m Derive.Result
derive_block sort cache damage block_id = do
    global_transform <- State.config#State.global_transform <#> State.get
    fmap (Derive.extract_result sort) $ run cache damage $ do
        unless (damage == mempty) $
            Log.debug $ "score damage for " <> showt block_id <> ": "
                <> pretty damage
        Call.Block.eval_root_block global_transform block_id

is_score_damage_log :: Log.Msg -> Bool
is_score_damage_log = ("score damage for " `Text.isPrefixOf`) . Log.msg_text

run :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage
    -> Derive.Deriver a -> m (Derive.RunResult a)
run cache damage deriver = do
    constant <- get_constant cache damage
    return $ Derive.derive constant initial_environ deriver

-- | Run a derivation when you already know the Dynamic.  This is the case when
-- deriving at a certain point in the score via the TrackDynamic.
run_with_dynamic :: Cmd.M m => Derive.Dynamic -> Derive.Deriver a
    -> m (Derive.RunResult a)
run_with_dynamic dynamic deriver = do
    constant <- get_constant mempty mempty
    let state = Derive.State
            { state_threaded = Derive.initial_threaded
            , state_dynamic = dynamic
            , state_collect = mempty
            , state_constant = constant
            }
    return $ Derive.run state deriver

get_constant :: Cmd.M m => Derive.Cache -> Derive.ScoreDamage
    -> m Derive.Constant
get_constant cache damage = do
    ui_state <- State.get
    lookup_scale <- Cmd.gets $ Cmd.state_lookup_scale . Cmd.state_config
    lookup_inst <- get_lookup_inst
    library <- Cmd.gets $ Cmd.state_library . Cmd.state_config
    defs_library <- get_library
    return $ Derive.initial_constant ui_state (defs_library <> library)
        lookup_scale lookup_inst cache damage

get_lookup_inst :: Cmd.M m => m (Score.Instrument -> Maybe Derive.Instrument)
get_lookup_inst = (fmap Cmd.derive_instrument .) <$> Cmd.get_lookup_instrument

perform_from :: Cmd.M m => RealTime -> Cmd.Performance -> m Perform.MidiEvents
perform_from start = perform_events . events_from start . Cmd.perf_events

shift_messages :: RealTime -> RealTime -> Perform.MidiEvents
    -> Perform.MidiEvents
shift_messages multiplier start events = shift start events
    where
    shift offset = map $ fmap $
        Midi.modify_timestamp ((* multiplier) . subtract offset)

-- | The first timestamp from the msgs.
first_time :: [LEvent.LEvent Midi.WriteMessage] -> RealTime
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
filter_track_muted :: TrackTree.TrackTree -> [(BlockId, Block.Block)]
    -> [Score.Event] -> [Score.Event]
filter_track_muted tree blocks
    | not (Set.null soloed) = filter (not . stack_contains solo_muted)
    | not (Set.null muted) = filter (not . stack_contains muted)
    | otherwise = id
    where
    stack_contains track_ids = any (`Set.member` track_ids) . stack_tracks
    stack_tracks = mapMaybe Stack.track_of . Stack.innermost . Score.event_stack
    soloed = with_flag Block.Solo
    muted = with_flag Block.Mute
    solo_muted = solo_to_mute tree blocks soloed
    with_flag flag = Set.fromList
        [ track_id
        | (_, block) <- blocks
        , track <- Block.block_tracks block
        , Just track_id <- [Block.track_id track]
        , flag `Set.member` Block.track_flags track
        ]

-- | Solo is surprisingly tricky.  Solo means non soloed-tracks are muted,
-- unless there is no solo on the block, or the track is the parent or child of
-- a soloed track.
--
-- I've already rewritten this a bunch of times, hopefully this is the last
-- time.
solo_to_mute :: TrackTree.TrackTree -- ^ All the trees of the whole score,
    -- concatenated.  This is because I just need to know who is a child of
    -- who, and I don't care what block they're in.
    -> [(BlockId, Block.Block)]
    -> Set.Set TrackId -> Set.Set TrackId
solo_to_mute tree blocks soloed = Set.fromList
    [ track_id
    | (block_id, block) <- blocks
    , track <- Block.block_tracks block
    , Just track_id <- [Block.track_id track]
    , track_id `Set.notMember` soloed
    , block_id `Set.member` soloed_blocks
    , track_id `Set.notMember` has_soloed_relatives
    ]
    where
    has_soloed_relatives = Set.fromList (mapMaybe get (Tree.flat_paths tree))
        where
        get (track, parents, children)
            | any (`Set.member` soloed) (map State.track_id children)
                    || any (`Set.member` soloed) (map State.track_id parents) =
                Just (State.track_id track)
            | otherwise = Nothing
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

perform_events :: Cmd.M m => Cmd.Events -> m Perform.MidiEvents
perform_events events = do
    configs <- State.get_midi_config
    lookup <- get_convert_lookup
    blocks <- State.gets (Map.toList . State.state_blocks)
    tree <- concat <$> mapM (TrackTree.track_tree_of . fst) blocks
    return $ fst $ Perform.perform Perform.initial_state configs $
        Convert.convert lookup $ filter_track_muted tree blocks $
        filter_instrument_muted configs $
        -- Performance should be lazy, so converting to a list here means I can
        -- avoid doing work for the notes that never get played.
        Vector.toList events

get_convert_lookup :: Cmd.M m => m Convert.Lookup
get_convert_lookup = do
    lookup_scale <- Cmd.gets $ Cmd.state_lookup_scale . Cmd.state_config
    lookup_inst <- Cmd.get_lookup_midi_instrument
    lookup_info <- Cmd.get_lookup_instrument
    configs <- State.get_midi_config
    let defaults = Map.map (Map.map (Score.untyped . Signal.constant)
            . Instrument.config_controls) configs
    return $ Convert.Lookup
        { lookup_scale = lookup_scale
        , lookup_inst = lookup_inst
        , lookup_patch = fmap extract . lookup_info
        , lookup_default_controls = \inst ->
            Map.findWithDefault mempty inst defaults
        }
    where
    extract info = (MidiDb.info_patch info,
        Cmd.inst_postproc $ MidiDb.info_code info)


-- * definition file

-- | Get Library from the cache.
get_library :: Cmd.M m => m Derive.Library
get_library = do
    cache <- Cmd.gets Cmd.state_ky_cache
    case cache of
        Nothing -> return mempty
        Just (_, Left err) -> Cmd.throw $ "get_library: " <> err
        Just (_, Right library) -> return library

-- | Update the definition cache by reading the per-score definition file.
update_definition_cache :: State.State -> Cmd.State -> IO Cmd.State
update_definition_cache ui_state cmd_state = case ky_file of
    Nothing
        | Maybe.isNothing $ Cmd.state_ky_cache cmd_state ->
            return cmd_state
        | otherwise -> return $ cmd_state { Cmd.state_ky_cache = Nothing }
    Just fname -> cached_load cmd_state fname >>= \x -> return $ case x of
        Nothing -> cmd_state
        Just lib -> cmd_state
            { Cmd.state_ky_cache = Just lib
            , Cmd.state_play = (Cmd.state_play cmd_state)
                { Cmd.state_performance = mempty
                , Cmd.state_current_performance = mempty
                , Cmd.state_performance_threads = mempty
                }
            }
    where ky_file = State.config#State.ky_file #$ ui_state

-- | Load a definition file if the cache is out of date.  Nothing if the cache
-- is up to date.
--
-- This only checks the timestamp of the file given.  If that file imports
-- other files they won't get checked.  Otherwise I have to essentially
-- recreate shake by tracking all the dependencies and last timestamps and
-- that's just way too complicated.
cached_load :: Cmd.State -> FilePath
    -> IO (Maybe (Time.UTCTime, Either Text Derive.Library))
cached_load state fname = run $ do
    dir <- require ("need a SaveFile to find " <> showt fname) $
        Cmd.state_save_dir state
    let paths = dir : Cmd.state_ky_paths (Cmd.state_config state)
    time <- either Error.throwError return
        =<< liftIO (Parse.find_mtime paths fname)
    if last_time == Just time then return Nothing else do
        lib <- liftIO $ load_ky paths fname
        return $ Just (time, lib)
    where
    run = fmap extract . Error.runErrorT
    extract (Left msg)
        -- If I failed to load last time too, then don't clear
        -- 'state_performance_threads' or I'll get an endless loop.
        | last_time == Just day0 = Nothing
        | otherwise = Just (day0, Left msg)
    extract (Right val) = val
    require msg = maybe (Error.throwError msg) return
    day0 = Time.UTCTime (Time.ModifiedJulianDay 0) 0
    last_time = fst <$> Cmd.state_ky_cache state

load_ky :: [FilePath] -> FilePath -> IO (Either Text Derive.Library)
load_ky paths fname =
    Parse.load_ky paths fname >>= \result -> case result of
        Left err -> return $ Left err
        Right (defs, imported) -> do
            Log.notice $ "imported definitions from "
                <> Text.intercalate ", " (map txt imported)
            forM_ (Library.shadowed lib) $ \((name, _), calls) ->
                Log.warn $ "definitions in " <> showt fname
                    <> " " <> name <> " shadowed: " <> pretty calls
            return $ Right lib
            where lib = compile_library defs

compile_library :: Parse.Definitions -> Derive.Library
compile_library (Parse.Definitions note control pitch val) = Derive.Library
    { lib_note = call_maps note
    , lib_control = call_maps control
    , lib_pitch = call_maps pitch
    , lib_val = Derive.call_map $ compile make_val_call val
    }
    where
    call_maps (gen, trans) = Derive.call_maps
        (compile make_generator gen) (compile make_transformer trans)
    compile make = map $ \(call_id, expr) -> (call_id, make call_id expr)

make_generator :: Derive.Callable d => TrackLang.Symbol -> TrackLang.Expr
    -> Derive.Generator d
make_generator (TrackLang.Symbol name) expr =
    Derive.make_call Module.local name mempty ("Local definition: " <> name) $
    case assign_symbol expr of
        Nothing -> Sig.call0 generator
        Just call_id -> Sig.parsed_manually "Args parsed by reapplied call." $
            \args -> Eval.reapply_generator args call_id
    where generator args = Eval.eval_toplevel (Derive.passed_info args) expr

make_transformer :: Derive.Callable d => TrackLang.Symbol -> TrackLang.Expr
    -> Derive.Transformer d
make_transformer (TrackLang.Symbol name) expr =
    Derive.make_call Module.local name mempty ("Local definition: " <> name) $
    case assign_symbol expr of
        Nothing -> Sig.call0t transformer
        Just call_id -> Sig.parsed_manually "Args parsed by reapplied call." $
            reapply call_id
    where
    transformer args deriver =
        Eval.eval_transformers (Derive.passed_info args)
            (NonEmpty.toList expr) deriver
    reapply call_id args deriver =
        Eval.apply_transformer (Derive.passed_info args) call_id
            (Derive.passed_vals args) deriver

make_val_call :: TrackLang.CallId -> TrackLang.Expr -> Derive.ValCall
make_val_call (TrackLang.Symbol name) expr =
    Derive.val_call Module.local name mempty ("Local definiton: " <> name) $
    case assign_symbol expr of
        Nothing -> Sig.call0 $ \args -> case expr of
            call :| [] ->
                Eval.eval (Derive.passed_info args) (TrackLang.ValCall call)
            _ -> Derive.throw "val calls don't support pipeline syntax"
        Just call_id -> Sig.parsed_manually "Args parsed by reapplied call."
            (call_args call_id)
    where
    call_args call_id args = do
        call <- Eval.get_val_call call_id
        Derive.vcall_call call $ args
            { Derive.passed_call_name = Derive.vcall_name call }

-- | If there are arguments in the definition, then don't accept any in the
-- score.  I could do partial application, but it seems confusing, so
-- I won't add it unless I need it.
assign_symbol :: TrackLang.Expr -> Maybe TrackLang.CallId
assign_symbol (TrackLang.Call call_id [] :| []) = Just call_id
assign_symbol _ = Nothing
