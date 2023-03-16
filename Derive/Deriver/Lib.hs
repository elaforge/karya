-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This has the higher level parts of the deriver library.  That is,
    functions where are considered basic but can be defined outside of
    "Derive.Deriver.Monad".
-}
module Derive.Deriver.Lib (
    Result(..)
    , derive
    , extract_result
    , with_default_imported
    -- * errors
    , require, require_right
    , catch

    -- * state access
    , get_stack
    , real_function, score_function
    -- ** import
    , with_imported, with_imported_symbols
    , with_scopes
    -- ** scale
    , get_scale, lookup_scale

    -- ** environment
    , lookup_val, is_val_set, get_val
    , with_val, with_vals
    , with_environ
    , with_val_raw
    , remove_val, remove_vals
    , modify_val
    , with_scale, with_instrument
    , with_instrument_alias, with_instrument_aliases
    , instrument_exists
    , get_instrument, lookup_instrument

    -- ** control
    , lookup_signal
    , lookup_function, get_function
    , is_control_set
    , get_control_map
    , get_function_map
    , state_signals, state_functions -- TODO remove junk
    , control_at, untyped_control_at, controls_at
    , modify_signals

    -- *** control signal
    , with_control, with_constant_control
    , with_controls
    , remove_controls
    , with_merged_control, with_merged_controls
    , resolve_merge
    , get_control_merge
    , get_default_merger

    -- *** ControlMod
    , modify_control
    , eval_control_mods
    , with_control_mods

    -- ** pitch
    , pitch_at, named_pitch_at
    , resolve_pitch
    , nn_at
    , get_pitch
    , named_nn_at
    , lookup_pitch_signal
    , logged_pitch_nn

    -- *** with signal
    , with_pitch, with_named_pitch, with_constant_pitch
    , remove_pitch

    -- * run monad
    , run_logs

    -- * Mode
    , get_mode
    , is_lilypond_mode
    , lookup_lilypond_config
    , get_score_duration, get_real_duration

    -- * postproc
    , with_event
    , with_event_stack
    , shift_controls

    -- * call
    , val_call
    , set_module
) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Seq as Seq

import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Symbols as Symbols
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Typecheck as Typecheck
import qualified Derive.Warp as Warp

import qualified Perform.Lilypond.Types as Lilypond.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import           Derive.Deriver.Monad
import           Global
import           Types


-- * derive

-- This should probably be in Internal, but can't due to a circular dependency
-- with 'real'.  TODO except not any more.  But Internal is lower level, so
-- maybe it should have less stuff in it anyway?

-- | Package up the results of a derivation.
data Result = Result {
    r_events :: !(Stream.Stream Score.Event)
    , r_cache :: !Cache
    , r_track_warps :: ![TrackWarp.TrackWarp]
    , r_track_signals :: !Track.TrackSignals
    , r_track_dynamic :: !TrackDynamic
    , r_integrated :: ![Integrated]

    -- | The relevant parts of the final state should be extracted into the
    -- above fields, but returning the whole state can be useful for testing.
    , r_state :: !State
    }

-- | Kick off a derivation.
derive :: Constant -> Dynamic -> Deriver a -> RunResult a
derive constant dynamic = run (initial_state constant dynamic)
    . with_initial_scope (state_environ dynamic)
    . with_default_imported

extract_result :: RunResult (Stream.Stream Score.Event) -> Result
extract_result (result, state, logs) = Result
    { r_events = merge_logs result (more_logs ++ logs)
    , r_cache = collect_cache collect <> state_cache (state_constant state)
    , r_track_warps =
        TrackWarp.collect_track_warps blocks (collect_warp_map collect)
    , r_track_signals = collect_track_signals collect
    , r_track_dynamic = extract_track_dynamic collect
    , r_integrated = collect_integrated collect
    , r_state = state
    }
    where
    (more_logs, blocks) =
        case Ui.run_id ui_state TrackWarp.get_track_trees of
            Left err -> ([Log.msg Log.Warn Nothing msg], [])
                where msg = "error collecting TrackWarps: " <> pretty err
            Right (blocks, _, _) -> ([], blocks)
    ui_state = state_ui $ state_constant state
    collect = state_collect state

-- | Extract the merged TrackDynamic from the Collect.
--
-- 'EnvKey.scale' comes from the inverted Collect because the scale track is
-- often inverted below the note track.  However, the others come from the
-- non-inverted Collect because if the note track sets an instrument, I want to
-- use its instrument, instead of any instrument on individual events.  E.g.
-- @>kendang-pasang@ has events that are @>kendang-wadon@ or @>kendang-lanang@.
--
-- See 'Collect' and 'TrackDynamic' for why.
extract_track_dynamic :: Collect -> TrackDynamic
extract_track_dynamic collect =
    strip_dynamic . extract <$> Maps.paired
        (collect_track_dynamic collect) (collect_track_dynamic_inverted collect)
    where
    extract = \case
        Seq.First dyn -> dyn
        Seq.Second dyn -> dyn
        Seq.Both normal inverted -> merge normal inverted
    merge normal inverted = normal
        { state_environ = keep (state_environ inverted) <> state_environ normal
        }
    keep env = maybe mempty (Env.from_list . (:[]) . (,) EnvKey.scale) $
        Env.lookup EnvKey.scale env

-- | Given an environ, bring instrument and scale calls into scope.
with_initial_scope :: Env.Environ -> Deriver d -> Deriver d
with_initial_scope env deriver = set_inst (set_scale deriver)
    where
    set_inst = case Env.get_val EnvKey.instrument env of
        Right inst -> with_instrument inst
        _ -> id
    set_scale = case Env.get_val EnvKey.scale env of
        Right str -> \deriver -> do
            scale <- get_scale (Expr.str_to_scale_id str)
            with_scale scale deriver
        _ -> id

with_default_imported :: Deriver a -> Deriver a
with_default_imported deriver =
    foldr (with_imported True) deriver
        [Module.internal, Module.prelude, Module.local]
    -- Calls from Module.local should shadow the others.


-- * errors

-- | If the deriver throws, log the error and return Nothing.
catch :: Bool -- ^ If True, incorporate the evaluated 'state_collect'.
    -- This is False for eval which is disconnected from track evaluation, and
    -- shouldn't be accumulating things like 'ControlMod's.
    -> Deriver a -> Deriver (Maybe a)
catch collect deriver = do
    state <- get
    -- It's critical to clear the collect, because if I merge it again later
    -- I can't go duplicating the whole thing.
    let (result, state2, logs) = run (state { state_collect = mempty }) deriver
    mapM_ Log.write logs
    case result of
        Left err -> do
            Log.write $ error_to_warn err
            return Nothing
        Right val -> do
            when collect $ Internal.merge_collect (state_collect state2)
            Internal.set_threaded (state_threaded state2)
            return $ Just val


-- * state access

get_stack :: Deriver Stack.Stack
get_stack = gets (state_stack . state_dynamic)

real_function :: Deriver (ScoreTime -> RealTime)
real_function = Warp.warp <$> Internal.get_dynamic state_warp

score_function :: Deriver (RealTime -> ScoreTime)
score_function = Warp.unwarp <$> Internal.get_dynamic state_warp

-- ** import

-- | Merge calls from the given module into scope.
with_imported :: Bool -> Module.Module -> Deriver a -> Deriver a
with_imported empty_ok module_ deriver = do
    builtins <- Internal.get_constant state_builtins
    scopes <- case extract_module module_ builtins of
        Scopes gen trans track val
            | not empty_ok && scope_empty gen && scope_empty trans
                    && scope_empty track && empty val ->
                throw $ "no calls in the imported module: " <> pretty module_
        extracted -> return extracted
    with_scopes (scopes<>) deriver
    where
    scope_empty (Scope a b c) = empty a && empty b && empty c
    empty (ScopePriority m) = Map.null m

-- | Import only the given symbols from the module.
with_imported_symbols :: Module.Module -> Set Expr.Symbol -> Deriver a
    -> Deriver a
with_imported_symbols module_ syms deriver = do
    scopes <- extract_symbols (`Set.member` syms) . extract_module module_ <$>
        Internal.get_constant state_builtins
    let missing = syms `Set.difference` Set.fromList (scope_symbols scopes)
    unless (Set.null missing) $
        throw $ "symbols not in module " <> pretty module_ <> ": "
            <> pretty (Set.toList missing)
    with_scopes (scopes<>) deriver

-- | Run the derivation with a modified scope.
with_scopes :: (Scopes -> Scopes) -> Deriver a -> Deriver a
with_scopes modify = Internal.local $ \state ->
    state { state_scopes = modify (state_scopes state) }

-- | Filter out any calls that aren't in the given module.
extract_module :: Module.Module -> Builtins -> Scopes
extract_module module_ (Scopes gen trans track val) = Scopes
    { scopes_generator = extract_scope gen
    , scopes_transformer = extract_scope trans
    , scopes_track = extract_scope track
    , scopes_val = extract val
    }
    where
    extract_scope (Scope note control pitch) = Scope
        { scope_note = extract note
        , scope_control = extract control
        , scope_pitch = extract pitch
        }
    extract = make . Map.findWithDefault mempty module_
    make :: CallMap a -> ScopePriority a
    make cmap@(CallMap calls patterns)
        | Map.null calls && null patterns = scope_priority []
        | null prio_block = scope_priority [(PrioBuiltin, cmap)]
        | otherwise = scope_priority
            [ (PrioBuiltin, cmap { call_patterns = normal })
            , (PrioBlock, mempty { call_patterns = prio_block })
            ]
        where
        (prio_block, normal) =
            List.partition has_prio_block (call_patterns cmap)
    has_prio_block pattern =
        cdoc_tags (pat_call_doc pattern) `Tags.contains` Tags.prio_block

-- | Filter out calls that don't match the predicate.  LookupCalls are also
-- filtered out.  This might be confusing since you might not even know a
-- call comes from a LookupPattern, but then you can't import it by name.
extract_symbols :: (Expr.Symbol -> Bool) -> Scopes -> Scopes
extract_symbols wanted (Scopes gen trans track val) = Scopes
    { scopes_generator = extract_scope gen
    , scopes_transformer = extract_scope trans
    , scopes_track = extract_scope track
    , scopes_val = extract val
    }
    where
    extract_scope (Scope note control pitch) = Scope
        { scope_note = extract note
        , scope_control = extract control
        , scope_pitch = extract pitch
        }
    extract = map_cmap $ \cmap -> mempty
        { call_map = Maps.filterKey wanted (call_map cmap) }
    map_cmap f (ScopePriority m) = ScopePriority $ f <$> m

scope_symbols :: Scopes -> [Expr.Symbol]
scope_symbols (Scopes gen trans track val) = mconcat
    [extract_scope gen, extract_scope trans, extract_scope track, extract val]
    where
    extract_scope (Scope note control pitch) =
        extract note <> extract control <> extract pitch
    extract (ScopePriority m) = concatMap (Map.keys . call_map) (Map.elems m)

-- ** scale

-- | Lookup a scale_id or throw.
get_scale :: Pitch.ScaleId -> Deriver Scale
get_scale scale_id =
    maybe (throw $ "get_scale: unknown scale: " <> pretty scale_id) return
    =<< lookup_scale scale_id

lookup_scale :: Pitch.ScaleId -> Deriver (Maybe Scale)
lookup_scale scale_id = do
    LookupScale lookup <- gets (state_lookup_scale . state_constant)
    env <- Internal.get_environ
    case lookup env scale_id of
        Nothing -> return Nothing
        Just (Left err) -> throw $ "lookup " <> pretty scale_id <> ": "
            <> pretty err
        Just (Right scale) -> return $ Just scale


-- ** environment

lookup_val :: Typecheck.Typecheck a => Env.Key -> Deriver (Maybe a)
lookup_val key =
    either throw return . Env.checked_val key =<< Internal.get_environ

is_val_set :: Env.Key -> Deriver Bool
is_val_set key = Maybe.isJust . Env.lookup key <$> Internal.get_environ

-- | Like 'lookup_val', but throw if the value isn't present.
get_val :: Typecheck.Typecheck a => Env.Key -> Deriver a
get_val key = do
    val <- lookup_val key
    maybe (throw $ "environ val not found: " <> pretty key) return val

{- | Set the given val dynamically within the given computation.  This is
    analogous to a dynamic let.

    Remove on VNotGiven, which is what any Nothing will become.  There's no use
    for a VNotGiven in the environ.  The main way this is used is the @val=_@
    c_equal syntax.

    There is intentionally no way to modify the environment via assignment.
    It would introduce an order of execution dependency that would complicate
    caching as well as have a confusing non-local effect.

    This dispatches to 'with_scale' or 'with_instrument' if it's setting the
    scale or instrument, so scale or instrument scopes are always set when scale
    and instrument are.
-}
with_val :: Typecheck.ToVal val => Env.Key -> val -> Deriver a -> Deriver a
with_val key val deriver
    | key == EnvKey.scale, Just scale_id <- DeriveT.to_scale_id v = do
        scale <- get_scale scale_id
        with_scale scale deriver
    | key == EnvKey.instrument, Just inst <- Typecheck.from_val_simple v =
        with_instrument inst deriver
    | otherwise = do
        env <- Internal.get_environ
        case Env.put_val key v env of
            Left err -> throw err
            Right env -> Internal.local
                (\state -> state { state_environ = env }) deriver
    where v = Typecheck.to_val val

-- | Like 'with_val', but should be slightly more efficient for setting
-- multiple values at once.
with_vals :: Typecheck.ToVal val => [(Env.Key, val)] -> Deriver a -> Deriver a
with_vals vals deriver
    | null vals = deriver
    | any (`elem` [EnvKey.scale, EnvKey.instrument]) (map fst vals) =
        foldr (uncurry with_val) deriver vals
    | otherwise = Internal.localm with deriver
    where
    with state = do
        environ <- either throw return $
            foldr (\(k, v) env -> Env.put_val k v =<< env)
                (return $ state_environ state) vals
        environ `seq` return $! state { state_environ = environ }

-- | Merge the given environ into the environ in effect.  Unlike 'with_val' or
-- 'with_vals', this won't set scopes for 'EnvKey.scale' and
-- 'EnvKey.instrument'.
with_environ :: Env.Environ -> Deriver a -> Deriver a
with_environ environ
    | Env.null environ = id
    | otherwise = Internal.local $ \state -> state
        { state_environ = environ <> state_environ state }

-- | Like 'with_val', but don't set scopes for instrument and scale.  Also
-- don't check for types, so you can replace a val with one of a different
-- type.  Due to this it's also more efficient.
with_val_raw :: Typecheck.ToVal val => Env.Key -> val -> Deriver a -> Deriver a
with_val_raw key val = Internal.local $ \state -> state
    { state_environ = Env.insert_val key val (state_environ state) }

with_vals_raw :: Typecheck.ToVal val => [(EnvKey.Key, val)] -> Deriver a
    -> Deriver a
with_vals_raw vals =
    with_environ $ Env.from_list (map (second Typecheck.to_val) vals)

remove_val :: Env.Key -> Deriver a -> Deriver a
remove_val = remove_vals . (:[])
-- remove_val key = Internal.local $ \state ->
--     state { state_environ = Env.delete key $ state_environ state }

remove_vals :: [Env.Key] -> Deriver a -> Deriver a
remove_vals keys = Internal.local $ \state ->
    state { state_environ = foldr Env.delete (state_environ state) keys }

modify_val :: (Typecheck.Typecheck val, Typecheck.ToVal val) => Env.Key
    -> (Maybe val -> val) -> Deriver a -> Deriver a
modify_val key modify = Internal.localm $ \state -> do
    val <- modify <$>
        require_right id (Env.checked_val key (state_environ state))
    return $! insert_env key val state

insert_env :: Typecheck.ToVal val => Env.Key -> val -> Dynamic -> Dynamic
insert_env key val state = state
    { state_environ =
        Env.insert_val key (Typecheck.to_val val) (state_environ state)
    }

-- | Replace the scale's calls.
--
-- Previously this used 'add_priority' instead of 'replace_priority', which
-- meant you could overlay scales and use both at the same time.  Maybe that's
-- actually a useful feature?  In any case, I don't need it at the moment, so
-- it seems more likely to be confusing than useful.
with_scale :: Scale -> Deriver d -> Deriver d
with_scale scale deriver =
    with_val_raw EnvKey.scale (Expr.scale_id_to_str (scale_id scale)) $
        with_scopes (val . pitch) deriver
    where
    pitch = s_generator#s_pitch %= replace (scale_to_call scale val_to_pitch)
    val = s_val %= replace (scale_to_call scale id)
    replace pattern =
        replace_priority PrioScale (mempty { call_patterns = [pattern] })

scale_to_call :: Scale -> (ValCall -> call) -> PatternCall call
scale_to_call scale convert = PatternCall
    { pat_description = description
    , pat_doc = scale_call_doc scale
    , pat_function = \sym ->
        return $ convert <$> scale_note_to_call scale (to_note sym)
    }
    where
    description = pretty (scale_id scale) <> ": " <> scale_pattern scale
    to_note (Expr.Symbol sym) = Pitch.Note sym

-- | Convert a val call to a pitch call.  This is used so scales can export
-- their ValCalls to pitch generators.
val_to_pitch :: ValCall -> Generator Pitch
val_to_pitch (ValCall name doc vcall) = Call
    { call_name = name
    , call_doc = doc
    , call_func = generator_func pitch_call
    }
    where
    convert_args args = args { passed_ctx = tag_context (passed_ctx args) }
    pitch_call args = vcall (convert_args args) >>= \val -> case val of
        DeriveT.VPitch pitch -> lookup_call Symbols.default_pitch >>= \case
            Nothing -> default_pitch_call args pitch
            Just pcall -> gfunc_f (call_func pcall) $ PassedArgs
                { passed_vals = [DeriveT.VPitch pitch]
                , passed_call_name = call_name pcall
                , passed_ctx = passed_ctx args
                }
        _ -> throw $ "scale call " <> pretty name
            <> " returned non-pitch: " <> ShowVal.show_val val

-- | This is the default pitch call for bare scale degrees if
-- 'Symbols.default_pitch' isn't set.
default_pitch_call :: PassedArgs val -> PSignal.Pitch
    -> Deriver (Stream.Stream PSignal.PSignal)
default_pitch_call args pitch = do
    -- This is Args.real, but I can't import it here.
    pos <- Internal.real $ Event.start $ ctx_event $ passed_ctx args
    return $ Stream.from_event $ PSignal.from_sample pos pitch

-- | Run the a deriver with the given instrument in scope.  In addition to
-- assigning the instrument to the 'EnvKey.instrument' field where note calls
-- can inherit it, this also brings the 'Instrument' fields into scope, which
-- is the per-instrument calls and per-instrument environ.
with_instrument :: ScoreT.Instrument -> Deriver d -> Deriver d
with_instrument inst deriver = do
    -- Previously, I would just substitute an empty instrument instead of
    -- throwing, but it turned out to be error prone, since a misspelled
    -- instrument would derive anyway, only without the right calls and
    -- environ.
    (inst, derive_inst) <- get_instrument inst
    let with_inst = with_val_raw EnvKey.instrument inst
    let with_scale = maybe id (with_val EnvKey.scale) $
            Env.lookup EnvKey.scale (inst_environ derive_inst)
    with_inst $ with_scopes (set_scopes (inst_calls derive_inst)) $
        with_scale $ with_environ (inst_environ derive_inst) deriver
    where
    -- Replace the calls in the instrument scope type.
    set_scopes (Scopes inst_gen inst_trans inst_track inst_val)
            (Scopes gen trans track val) =
        Scopes
            { scopes_generator = (s_note %= replace inst_gen) gen
            , scopes_transformer = (s_note %= replace inst_trans) trans
            , scopes_track = (s_note %= replace inst_track) track
            , scopes_val = replace inst_val val
            }
    replace = replace_priority PrioInstrument

with_instrument_alias :: ScoreT.Instrument -> ScoreT.Instrument
    -> Deriver a -> Deriver a
with_instrument_alias alias inst =
    with_instrument_aliases (Map.singleton alias inst)

with_instrument_aliases :: Map ScoreT.Instrument ScoreT.Instrument
    -> Deriver a -> Deriver a
with_instrument_aliases aliases deriver
    | Map.null aliases = deriver
    -- I used to verify that the rhs insts exist, but verification can be
    -- annoying if there is a library ky that creates some general purpose
    -- aliases, e.g. >r{1..4} = >r.
    | otherwise = Internal.local with deriver
    where
    with state = state
        { state_instrument_aliases = (resolve <$> aliases) <> old_aliases }
        where
        old_aliases = state_instrument_aliases state
        resolve inst = Map.findWithDefault inst inst old_aliases

instrument_exists :: ScoreT.Instrument -> Deriver Bool
instrument_exists = (Either.isRight . snd <$>) . lookup_instrument

get_instrument :: ScoreT.Instrument -> Deriver (ScoreT.Instrument, Instrument)
get_instrument score_inst = do
    (real_inst, result) <- lookup_instrument score_inst
    case result of
        Left err -> throw $ "instrument "
            <> "'" <> ShowVal.show_val real_inst <> "': " <> err
            <> if real_inst == score_inst then ""
                else " (aliased from " <> ShowVal.show_val score_inst <> ")"
        Right inst -> return (real_inst, inst)

-- | Look up the instrument.  Also return the instrument name after resolving
-- any alias.  This is what goes in 'Score.event_instrument', since it's what
-- the performer understands.
lookup_instrument :: ScoreT.Instrument
    -> Deriver (ScoreT.Instrument, Either Text Instrument)
lookup_instrument inst = do
    aliases <- Internal.get_dynamic state_instrument_aliases
    let real_inst = Map.findWithDefault inst inst aliases
    lookup_inst <- gets $ state_lookup_instrument . state_constant
    return (real_inst, lookup_inst real_inst)


-- ** control

lookup_signal :: ScoreT.Control -> Deriver (Maybe (ScoreT.Typed Signal.Control))
lookup_signal = Typecheck.lookup_signal

lookup_function :: ScoreT.Control -> Deriver (Maybe DeriveT.TypedFunction)
lookup_function = Typecheck.lookup_function . flip DeriveT.Ref Nothing

get_function :: ScoreT.Control -> Deriver DeriveT.TypedFunction
get_function = Typecheck.resolve_function . flip DeriveT.Ref Nothing

-- | Get the control value at the given time.
control_at :: ScoreT.Control -> RealTime
    -> Deriver (Maybe (ScoreT.Typed Signal.Y))
control_at control pos = fmap (fmap ($ pos)) <$> lookup_function control

untyped_control_at :: ScoreT.Control -> RealTime -> Deriver (Maybe Signal.Y)
untyped_control_at control = fmap (fmap ScoreT.typed_val) . control_at control

is_control_set :: ScoreT.Control -> Deriver Bool
is_control_set = is_val_set . ScoreT.control_name

-- *** all signals / functions

-- TODO for notes, to get just signals, no cfs.  Keeping with function / signal
-- naming, should be get_signals and SignalMap?
get_control_map :: Deriver DeriveT.ControlMap
get_control_map =
    fmap Map.fromList $ mapMaybeM convert . Env.to_list =<< Internal.get_environ
    where
    convert (key, val) = fmap (ScoreT.Control key,) <$> to_signal val
    to_signal val = case Typecheck.val_to_signal val of
        Nothing -> return Nothing
        Just (Right sig) -> return $ Just sig
        Just (Left dsig) -> Just <$> dsig

-- | Like 'get_function', but get them all.
get_function_map :: Deriver DeriveT.FunctionMap
get_function_map = do
    cf_dyn <- Internal.get_control_function_dynamic
    let to_function val = case Typecheck.val_to_function_dyn cf_dyn val of
            Nothing -> return Nothing
            Just (Right tf) -> return $ Just tf
            Just (Left dtf) -> Just <$> dtf
    let resolve (key, val) = fmap (ScoreT.Control key,) <$> to_function val
    Map.fromAscList <$>
        (mapMaybeM resolve . Env.to_list =<< Internal.get_environ)

-- | Get a ControlValMap at the given time.
{-# SCC controls_at #-}
controls_at :: RealTime -> Deriver ScoreT.ControlValMap
controls_at = fmap (fmap ScoreT.typed_val) . typed_controls_at

typed_controls_at :: RealTime -> Deriver ScoreT.TypedControlValMap
typed_controls_at pos = fmap (fmap ($ pos)) <$> get_function_map

-- | Get all signals in the environ.  This is like 'get_control_map', but
-- doesn't resolve ControlRefs.
-- TODO remove it, only used by LPerf and Derive_test
state_signals :: Dynamic -> DeriveT.ControlMap
state_signals =
    Map.mapKeys ScoreT.Control . Map.mapMaybe is_signal
        . Env.to_map . state_environ
    where
    is_signal val = case Typecheck.val_to_signal val of
        Just (Right sig) -> Just sig
        _ -> Nothing

-- TODO remove it, only used by LPerf
-- maybe generalize 'get_function_map' to take cf_dyn and environ
state_functions :: Dynamic -> Ruler.Marklists -> Stack.Serial
    -> DeriveT.FunctionMap
state_functions dyn mlists serial = Map.fromList
    [ (ScoreT.Control name, f)
    | (name, val) <- Env.to_list env
    , Just f <- [val_to_function_dyn cf_dyn val]
    ]
    where
    cf_dyn = Internal.convert_dynamic mlists dyn serial
    env = state_environ dyn
    val_to_function_dyn :: DeriveT.Dynamic -> DeriveT.Val
        -> Maybe DeriveT.TypedFunction
    val_to_function_dyn dyn val =
        case Typecheck.val_to_function_dyn dyn val of
            Just (Right f) -> Just f
            _ -> Nothing

-- *** control signal

with_control :: ScoreT.Control -> ScoreT.Typed Signal.Control -> Deriver a
    -> Deriver a
with_control control signal = with_val_raw (ScoreT.control_name control) signal

with_constant_control :: ScoreT.Control -> Signal.Y -> Deriver a -> Deriver a
with_constant_control control val =
    with_control control (ScoreT.untyped (Signal.constant val))

with_controls :: [(ScoreT.Control, ScoreT.Typed Signal.Control)] -> Deriver a
    -> Deriver a
with_controls controls =
    with_vals_raw (map (first ScoreT.control_name) controls)

-- TODO replace doesn't make so much sense with unify-env
-- | Like 'with_controls', but delete all other signals at the same time.
-- A signal is anything that can be coerced into one via
-- 'Typecheck.val_to_signal'.  Does not do anything with ControlFunctions!
-- replace_controls :: [(ScoreT.Control, ScoreT.Typed Signal.Control)]
--     -> Deriver a -> Deriver a
-- replace_controls controls = Internal.local $ \state ->
--     state { state_environ = replace (state_environ state) }
--     where
--     replace = Env.from_map . (Map.fromList converted <>)
--         . Map.filter (Either.isRight . Typecheck.val_to_signal) . Env.to_map
--     converted = map (bimap ScoreT.control_name Typecheck.to_val) controls

-- | Remove both controls and control functions.  Use this when a control has
-- already been applied, and you don't want it to affect further derivation.
remove_controls :: [ScoreT.Control] -> Deriver a -> Deriver a
remove_controls controls = Internal.local $ \state -> state
    { state_environ = foldr Env.delete (state_environ state) keys }
    where keys = map ScoreT.control_name controls

-- | Modify the given control according to the Merger.
--
-- If both signals are typed, the existing type wins over the relative
-- signal's type.  If one is untyped, the typed one wins.
--
-- As documented in 'merge', this acts like a Set if there is no existing
-- control.
with_merged_control :: Merger -> ScoreT.Control -> ScoreT.Typed Signal.Control
    -> Deriver a -> Deriver a
with_merged_control merger control signal =
    modify_signal control (\mb_sig -> merge merger mb_sig signal)

-- | This is not just 'with_control', because I have to merge a control signal
-- into a possible ControlFunction.
modify_signal :: ScoreT.Control
    -> (Maybe DeriveT.TypedSignal -> DeriveT.TypedSignal) -> Deriver a
    -> Deriver a
modify_signal (ScoreT.Control control) modify = Internal.localm $ \state -> do
    val <- require_right ((control <> ": ")<>) $
        Env.modify_signal modify control (state_environ state)
    return $! insert_env control val state

-- | Like 'with_controls', but merge them with their respective default
-- 'Merger's.
with_merged_controls :: [(ScoreT.Control, ScoreT.Typed Signal.Control)]
    -> Deriver a -> Deriver a
with_merged_controls control_vals deriver
    | null control_vals = deriver
    | otherwise = do
        let (controls, new_sigs) = unzip control_vals
        mergers <- mapM get_default_merger controls
        old_sigs <- mapM lookup_signal controls
        let merged = zipWith3 merge mergers old_sigs new_sigs
        with_controls (zip controls merged) deriver

resolve_merge :: Merge -> ScoreT.Control -> Deriver Merger
resolve_merge DefaultMerge control = get_default_merger control
resolve_merge (Merge merger) _ = return merger

get_control_merge :: Expr.Symbol -> Deriver Merger
get_control_merge name = do
    mergers <- gets (state_mergers . state_constant)
    require ("unknown control merger: " <> ShowVal.show_val name)
        (Map.lookup name mergers)

-- | Get the default merger for this control, or 'merge_mul' if there is none.
get_default_merger :: ScoreT.Control -> Deriver Merger
get_default_merger control = do
    defaults <- Internal.get_dynamic state_control_merge_defaults
    return $ Map.findWithDefault default_merge control defaults
    where
    default_merge = merge_mul

-- | Combine two signals with a Merger.  If there was no old signal, use
-- merger-defined identity value.
--
-- Since the default merge for control tracks is multiplication, whose identity
-- is 1, this means the first control track will set the value, instead of
-- being multiplied to 0.
merge :: Merger -> Maybe (ScoreT.Typed Signal.Control)
    -> ScoreT.Typed Signal.Control -> ScoreT.Typed Signal.Control
merge Set _ new = new
merge Unset (Just old) _ = old
merge Unset Nothing new = new
merge (Merger _ merger ident) maybe_old new =
    ScoreT.Typed (ScoreT.type_of old <> ScoreT.type_of new)
        (merger (ScoreT.typed_val old) (ScoreT.typed_val new))
    where old = fromMaybe (ScoreT.untyped (Signal.constant ident)) maybe_old
    -- Using ident is *not* the same as just emitting the 'new' signal for
    -- subtraction!

-- *** ControlMod

-- | Emit a 'ControlMod'.
modify_control :: Merger -> ScoreT.Control -> Signal.Control -> Deriver ()
modify_control merger control signal = Internal.modify_collect $ \collect ->
    collect { collect_control_mods =
        ControlMod control signal merger : collect_control_mods collect }

-- | Modify all VSignal and VPSignal types in environ.
modify_signals :: (Signal.Control -> Signal.Control)
    -> (PSignal.PSignal -> PSignal.PSignal) -> Deriver a -> Deriver a
modify_signals modify_control modify_pitch = Internal.local $ \state -> state
    { state_environ = Env.map update (state_environ state)
    , state_pitch = modify_pitch (state_pitch state)
    }
    where
    update = \case
        DeriveT.VSignal sig -> DeriveT.VSignal (modify_control <$> sig)
        DeriveT.VPSignal sig -> DeriveT.VPSignal (modify_pitch sig)
        DeriveT.VControlFunction
                (DeriveT.ControlFunction name (DeriveT.CFBacked sig f)) ->
            DeriveT.VControlFunction $ DeriveT.ControlFunction name $
                DeriveT.CFBacked (modify_control <$> sig) f
        val -> val

-- | Apply the collected control mods to the given deriver and clear them out.
eval_control_mods :: RealTime -- ^ Trim controls to end at this time.
    -- If a ControlMod is local to a slice it should end when the slice ends,
    -- and since it bypasses 'Derive.Control.trim_signal', I have to trim
    -- it explicitly.
    -> Deriver a -> Deriver a
eval_control_mods end deriver = do
    mods <- gets (collect_control_mods . state_collect)
    if null mods then deriver else do
        -- TODO Wait, is this really legit?
        Internal.modify_collect $ \collect ->
            collect { collect_control_mods = [] }
        with_control_mods mods end deriver

with_control_mods :: [ControlMod] -> RealTime -> Deriver a -> Deriver a
with_control_mods mods end deriver = foldr ($) deriver (map apply mods)
    where
    apply (ControlMod control signal merger) =
        with_merged_control merger control $ ScoreT.untyped $
            Signal.clip_after end signal
            -- TODO is clip_after necessary?  Document end better, with
            -- a reference to a test which demonstrates the issue.

-- ** pitch

-- | The pitch at the given time.  The transposition controls have not been
-- applied since that is supposed to be done once only when the event is
-- generated.
--
-- The scenario is a call that generates a note based on the current pitch.
-- If 'pitch_at' applied the transposition, the new note would have to remove
-- the transposition signals so they don't get applied again at performance
-- conversion.
pitch_at :: RealTime -> Deriver (Maybe PSignal.Pitch)
pitch_at pos = PSignal.at pos <$> get_pitch

-- | Like 'pitch_at', this is a raw pitch.
named_pitch_at :: ScoreT.PControl -> RealTime -> Deriver (Maybe PSignal.Pitch)
named_pitch_at name pos = PSignal.at pos <$> get_pitch_signal name

-- | Resolve the raw pitch returned from 'pitch_at' to the final transposed
-- pitch.
resolve_pitch :: RealTime -> PSignal.Pitch -> Deriver PSignal.Transposed
resolve_pitch pos pitch = do
    controls <- controls_at pos
    return $ PSignal.apply controls pitch

-- | Unlike 'pitch_at', the transposition has already been applied, because you
-- can't transpose any further once you have a NoteNumber.
nn_at :: RealTime -> Deriver (Maybe Pitch.NoteNumber)
nn_at pos = justm (pitch_at pos) $ \pitch ->
    logged_pitch_nn ("nn " <> pretty pos) =<< resolve_pitch pos pitch

get_pitch :: Deriver PSignal.PSignal
get_pitch = Internal.get_dynamic state_pitch

-- | Re-export 'Typecheck.lookup_pitch_signal', defined there to avoid
-- circular import.
lookup_pitch_signal :: ScoreT.PControl -> Deriver (Maybe PSignal.PSignal)
lookup_pitch_signal = Typecheck.lookup_pitch_signal

get_pitch_signal :: ScoreT.PControl -> Deriver PSignal.PSignal
get_pitch_signal pcontrol =
    -- The PControl itself doesn't add the # because that's the ref syntax,
    -- but let's add the # to remind that it's a PControl.
    require ("no named pitch #" <> ShowVal.show_val pcontrol)
        =<< lookup_pitch_signal pcontrol

named_nn_at :: ScoreT.PControl -> RealTime -> Deriver (Maybe Pitch.NoteNumber)
named_nn_at name pos = do
    controls <- controls_at pos
    justm (named_pitch_at name pos) $ \pitch ->
        logged_pitch_nn ("named_nn " <> pretty (name, pos)) $
            PSignal.apply controls pitch

-- | Version of 'PSignal.pitch_nn' that logs errors.
logged_pitch_nn :: Text -> PSignal.Transposed
    -> Deriver (Maybe Pitch.NoteNumber)
logged_pitch_nn msg pitch = case PSignal.pitch_nn pitch of
    Left err -> do
        Log.warn $ "pitch_nn " <> msg <> ": " <> pretty err
        return Nothing
    Right nn -> return $ Just nn

-- *** with signal

with_pitch :: PSignal.PSignal -> Deriver a -> Deriver a
with_pitch = modify_pitch ScoreT.default_pitch . const

-- TODO now that named pitch is just env VPSignals, do I really need this?
with_named_pitch :: ScoreT.PControl -> PSignal.PSignal -> Deriver a -> Deriver a
with_named_pitch pcontrol = modify_pitch pcontrol . const

with_constant_pitch :: PSignal.Pitch -> Deriver a -> Deriver a
with_constant_pitch = with_pitch . PSignal.constant

remove_pitch :: Deriver a -> Deriver a
remove_pitch = modify_pitch ScoreT.default_pitch (const mempty)

modify_pitch :: ScoreT.PControl -> (Maybe PSignal.PSignal -> PSignal.PSignal)
    -> Deriver a -> Deriver a
modify_pitch pcontrol modify deriver
    | pcontrol == ScoreT.default_pitch = Internal.local
        (\state -> state { state_pitch = modify (Just (state_pitch state)) })
        deriver
    | otherwise = do
        mb_sig <- lookup_val (ScoreT.pcontrol_name pcontrol)
        with_val_raw (ScoreT.pcontrol_name pcontrol) (modify mb_sig) deriver

-- * run monad

-- | Embed a LogId into Deriver.  This is for computations that need logging
-- but not a full Deriver.
run_logs :: Log.LogId a -> Deriver a
run_logs action = do
    let (val, logs) = Log.run_id action
    mapM_ Log.write logs
    return val

run_try :: (State -> State) -> Deriver a -> Deriver (Either Error a, State)
run_try with_state deriver = do
    state <- get
    let (val, state2, logs) = run (with_state state) deriver
    mapM_ Log.write logs
    return (val, state2)

-- * 'Mode'

get_mode :: Deriver Mode
get_mode = gets (state_mode . state_dynamic)

is_lilypond_mode :: Deriver Bool
is_lilypond_mode = Maybe.isJust <$> lookup_lilypond_config

lookup_lilypond_config :: Deriver (Maybe Lilypond.Types.Config)
lookup_lilypond_config = get_mode >>= \mode -> return $ case mode of
    Lilypond config -> Just config
    _ -> Nothing

-- | Get the 'CallDuration' of the given deriver.
get_score_duration :: Deriver a
    -> Deriver (Either Error (CallDuration ScoreTime))
get_score_duration deriver = do
    (val, out) <- run_try set_mode deriver
    return $ case val of
        Left err -> Left err
        Right _ -> Right $ collect_score_duration $ state_collect out
    where
    set_mode state = state
        { state_collect = mempty
        , state_dynamic = (state_dynamic state)
            { state_mode = ScoreDurationQuery }
        }

get_real_duration :: Deriver a -> Deriver (Either Error (CallDuration RealTime))
get_real_duration deriver = do
    (val, out) <- run_try set_mode deriver
    return $ case val of
        Left err -> Left err
        Right _ -> Right $ collect_real_duration $ state_collect out
    where
    set_mode state = state
        { state_collect = mempty
        , state_dynamic = (state_dynamic state)
            { state_mode = RealDurationQuery }
        }

-- * postproc

-- | Replace the 'state_stack' with the one from the event.  This is useful
-- for transformers, so they can show a stack trace to the event they are
-- processing.
with_event_stack :: Score.Event -> Deriver a -> Deriver a
with_event_stack event =
    maybe id with_stack (Stack.block_track_region_of (Score.event_stack event))
    where
    with_stack (block_id, track_id, (s, e)) = Internal.with_stack_block block_id
        . Internal.with_stack_track track_id . Internal.with_stack_region s e

-- | A combination of 'catch' and 'with_event_stack'.
with_event :: Score.Event -> Deriver a -> Deriver (Maybe a)
with_event event = catch False . with_event_stack event

-- | Shift the controls of a deriver.  You're supposed to apply the warp
-- before deriving the controls, but I don't have a good solution for how to
-- do this yet, so I can leave these here for the moment.
shift_controls :: ScoreTime -> Deriver a -> Deriver a
shift_controls shift deriver = do
    real <- Internal.real shift
    modify_signals (Signal.shift real) (PSignal.shift real) deriver

-- * call

-- | Wrap 'make_val_call' with a 'Typecheck.to_val' to automatically convert
-- to a 'DeriveT.Val'.  This is not in "Derive.Deriver.Monad" to avoid
-- a circular import with "Derive.DeriveT".
val_call :: Typecheck.ToVal a => Module.Module -> CallName -> Tags.Tags
    -> Doc.Doc -> WithArgDoc (PassedArgs Tagged -> Deriver a) -> ValCall
val_call module_ name tags doc (call, arg_docs) =
    make_val_call module_ name tags doc (fmap Typecheck.to_val . call, arg_docs)

set_module :: Module.Module -> Call f -> Call f
set_module module_ call = call
    { call_doc = (call_doc call) { cdoc_module = module_ } }
