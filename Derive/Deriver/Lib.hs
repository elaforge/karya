-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | This has the higher level parts of the deriver library.  That is,
    functions where are considered basic but can be defined outside of
    "Derive.Deriver.Monad".
-}
module Derive.Deriver.Lib where
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.CallStack as CallStack
import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Util.Map
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Deriver.Internal as Internal
import Derive.Deriver.Monad
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Types as Lilypond.Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Global
import Types


-- * derive

-- This should probably be in Internal, but can't due to a circular dependency
-- with 'real'.

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
    . with_initial_instrument_aliases
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
    Map.fromList $ map (second strip_dynamic . extract) $ Util.Map.pairs
        (collect_track_dynamic collect) (collect_track_dynamic_inverted collect)
    where
    extract (k, Seq.First dyn) = (k, dyn)
    extract (k, Seq.Second dyn) = (k, dyn)
    extract (k, Seq.Both normal inverted) = (k, merge normal inverted)
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

-- | Apply the instrument aliases loaded from the ky file.  This should only
-- happen when starting a derivation.
with_initial_instrument_aliases :: Deriver a -> Deriver a
with_initial_instrument_aliases deriver = do
    aliases <- lib_instrument_aliases <$> Internal.get_constant state_library
    with_instrument_aliases aliases deriver

with_default_imported :: Deriver a -> Deriver a
with_default_imported deriver =
    foldr (with_imported True) deriver
        [Module.internal, Module.prelude, Module.local]
    -- Calls from Module.local should shadow the others.


-- * errors

require :: CallStack.Stack => Text -> Maybe a -> Deriver a
require msg = maybe (throw msg) return

require_right :: CallStack.Stack => (err -> Text) -> Either err a -> Deriver a
require_right fmt_err = either (throw . fmt_err) return


-- * state access

get_stack :: Deriver Stack.Stack
get_stack = gets (state_stack . state_dynamic)

real_function :: Deriver (ScoreTime -> RealTime)
real_function = Score.warp_pos <$> Internal.get_dynamic state_warp

score_function :: Deriver (RealTime -> ScoreTime)
score_function = Score.unwarp_pos <$> Internal.get_dynamic state_warp

-- ** import

-- | Merge calls from the given module into scope.
with_imported :: Bool -> Module.Module -> Deriver a -> Deriver a
with_imported empty_ok module_ deriver = do
    lib <- Internal.get_constant state_library
    lib <- case extract_module module_ lib of
        Library note control pitch val _aliases
            | and [is_empty note, is_empty control, is_empty pitch, null val]
                    && not empty_ok ->
                throw $ "no calls in the imported module: " <> pretty module_
        extracted -> return extracted
    with_scopes (import_library lib) deriver
    where
    is_empty (Scopes [] [] [] ()) = True
    is_empty _ = False

-- | Import only the given symbols from the module.
with_imported_symbols :: Module.Module -> Set Expr.Symbol -> Deriver a
    -> Deriver a
with_imported_symbols module_ syms deriver = do
    lib <- extract_symbols (`Set.member` syms) . extract_module module_ <$>
        Internal.get_constant state_library
    let missing = syms `Set.difference` Set.fromList (library_symbols lib)
    unless (Set.null missing) $
        throw $ "symbols not in module " <> pretty module_ <> ": "
            <> pretty (Set.toList missing)
    with_scopes (import_library lib) deriver

-- | Run the derivation with a modified scope.
with_scopes :: (Scopes -> Scopes) -> Deriver a -> Deriver a
with_scopes modify = Internal.local $ \state ->
    state { state_scopes = modify (state_scopes state) }

-- | Filter out any calls that aren't in the given modules.
extract_module :: Module.Module -> Library -> Library
extract_module module_ (Library note control pitch val _aliases) = Library
    { lib_note = extract2 note
    , lib_control = extract2 control
    , lib_pitch = extract2 pitch
    , lib_val = extract vcall_doc val
    , lib_instrument_aliases = mempty
    }
    where
    extract2 (Scopes gen trans track ()) = Scopes
        { scopes_generator = extract call_doc gen
        , scopes_transformer = extract call_doc trans
        , scopes_track = extract tcall_doc track
        , scopes_val = ()
        }
    extract get_doc = mapMaybe (has_module get_doc)
    has_module get_doc (LookupMap calls)
        | Map.null include = Nothing
        | otherwise = Just (LookupMap include)
        where include = Map.filter (wanted . get_doc) calls
    has_module _ lookup@(LookupPattern _ (DocumentedCall _ doc) _)
        | wanted doc = Just lookup
        | otherwise = Nothing
    wanted = (== module_) . cdoc_module

-- | Filter out calls that don't match the predicate.  LookupCalls are also
-- filtered out.  This might be confusing since you might not even know a
-- call comes from a LookupPattern, but then you can't import it by name.
extract_symbols :: (Expr.Symbol -> Bool) -> Library -> Library
extract_symbols wanted (Library note control pitch val _aliases) = Library
    { lib_note = extract2 note
    , lib_control = extract2 control
    , lib_pitch = extract2 pitch
    , lib_val = extract val
    , lib_instrument_aliases = mempty
    }
    where
    extract2 (Scopes gen trans track ()) = Scopes
        { scopes_generator = extract gen
        , scopes_transformer = extract trans
        , scopes_track = extract track
        , scopes_val = ()
        }
    extract = mapMaybe has_name
    has_name (LookupMap calls)
        | Map.null include = Nothing
        | otherwise = Just (LookupMap include)
        where include = Util.Map.filter_key wanted calls
    has_name (LookupPattern {}) = Nothing

library_symbols :: Library -> [Expr.Symbol]
library_symbols (Library note control pitch val _aliases) =
    extract2 note <> extract2 control <> extract2 pitch <> extract val
    where
    extract2 (Scopes gen trans track ()) =
        extract gen <> extract trans <> extract track
    extract = concatMap names_of
    names_of (LookupMap calls) = Map.keys calls
    names_of (LookupPattern {}) = []

import_library :: Library -> Scopes -> Scopes
import_library (Library lib_note lib_control lib_pitch lib_val _aliases)
        (Scopes gen trans track val) =
    -- It seems like I should be able to refactor this, but it's hard to get
    -- the types to work out.
    Scopes
        { scopes_generator = Scope
            { scope_note =
                insert (scopes_generator lib_note) (scope_note gen)
            , scope_control =
                insert (scopes_generator lib_control) (scope_control gen)
            , scope_pitch =
                insert (scopes_generator lib_pitch) (scope_pitch gen)
            }
        , scopes_transformer = Scope
            { scope_note =
                insert (scopes_transformer lib_note) (scope_note trans)
            , scope_control =
                insert (scopes_transformer lib_control) (scope_control trans)
            , scope_pitch =
                insert (scopes_transformer lib_pitch) (scope_pitch trans)
            }
        , scopes_track = Scope
            { scope_note =
                insert (scopes_track lib_note) (scope_note track)
            , scope_control =
                insert (scopes_track lib_control) (scope_control track)
            , scope_pitch =
                insert (scopes_track lib_pitch) (scope_pitch track)
            }
        , scopes_val = insert lib_val val
        }
    where
    insert lookups = (imported (merge_lookups lookups) <>)
    imported lookups = scope_priority
        [ (PrioBlock, prio_block)
        , (PrioLibrary, normal)
        ]
        where
        (prio_block, normal) = List.partition
            (lookup_pattern_has_tag Tags.prio_block) lookups

-- | Merge 'LookupMap's into one LookupMap, with any LookupPatterns afterwards.
-- If there are collisions, the first one wins.
merge_lookups :: [LookupCall call] -> [LookupCall call]
merge_lookups lookups = LookupMap calls : [p | p@(LookupPattern {}) <- lookups]
    where calls = Map.unions [calls | LookupMap calls <- lookups]

-- | Check if the call has a tag.  This doesn't support LookupMap, but doesn't
-- need to because I only use it for the block lookup call.
lookup_pattern_has_tag :: Tags.Tags -> LookupCall call -> Bool
lookup_pattern_has_tag tag lookup = case lookup of
    LookupPattern _ (DocumentedCall _ doc) _ ->
        cdoc_tags doc `Tags.contains` tag
    LookupMap {} -> False

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
    case lookup env (LookupScale lookup) scale_id of
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

-- | Set the given val dynamically within the given computation.  This is
-- analogous to a dynamic let.
--
-- There is intentionally no way to modify the environment via assignment.
-- It would introduce an order of execution dependency that would complicate
-- caching as well as have a confusing non-local effect.
--
-- This dispatches to 'with_scale' or 'with_instrument' if it's setting the
-- scale or instrument, so scale or instrument scopes are always set when scale
-- and instrument are.
with_val :: Typecheck.ToVal val => Env.Key -> val -> Deriver a -> Deriver a
with_val key val deriver
    | key == EnvKey.scale, Just scale_id <- BaseTypes.to_scale_id v = do
        scale <- get_scale scale_id
        with_scale scale deriver
    | key == EnvKey.instrument, Just inst <- Typecheck.from_val_simple v =
        with_instrument inst deriver
    | otherwise = with_val_raw key val deriver
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
            foldr (\(k, v) env -> Env.put_val_error k v =<< env)
                (return $ state_environ state) vals
        environ `seq` return $! state { state_environ = environ }

-- | Like 'with_val', but don't set scopes for instrument and scale.
with_val_raw :: Typecheck.ToVal val => Env.Key -> val -> Deriver a -> Deriver a
with_val_raw key val = Internal.localm $ \state -> do
    environ <- insert_environ key val (state_environ state)
    environ `seq` return $! state { state_environ = environ }
    where insert_environ key val = require_right id . Env.put_val_error key val

delete_val :: Env.Key -> Deriver a -> Deriver a
delete_val key = Internal.local $ \state ->
    state { state_environ = Env.delete key $ state_environ state }

-- | This is the Env version of with_merged_control.  It only works on numeric
-- env vals.
with_merged_numeric_val :: Merger Signal.Control -> Env.Key
    -> Signal.Y -> Deriver a -> Deriver a
with_merged_numeric_val Set key val = with_val key val
with_merged_numeric_val (Merger name merge ident) key val = Internal.localm $
    \state -> do
        (typ, old) <- case Env.checked_val2 key (state_environ state) of
            Nothing -> return (Score.Untyped, ident)
            Just (Right (Score.Typed typ old)) ->
                return (typ, Signal.constant old)
            Just (Left err) -> throw err
        -- This is a hack to reuse Merger, which is defined on Signal, not Y.
        -- TODO it could be defined on Y, but I'd lose interleave... but I only
        -- use that for pitches and it's kind of sketchy anyway.
        new <- require ("merger " <> name <> " produced an empty signal") $
            Signal.constant_val (merge old (Signal.constant val))
        return $! insert_env key (Score.Typed typ new) state

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
with_scale scale =
    with_val_raw EnvKey.scale (Expr.scale_id_to_str (scale_id scale))
        . with_scopes (val . pitch)
    where
    pitch = s_generator#s_pitch %= replace [scale_to_lookup scale val_to_pitch]
    val = s_val %= replace [scale_to_lookup scale id]
    replace = replace_priority PrioScale

scale_to_lookup :: Scale -> (ValCall -> call) -> LookupCall call
scale_to_lookup scale convert =
    LookupPattern name (scale_call_doc scale) $ \sym ->
        return $ convert <$> scale_note_to_call scale (to_note sym)
    where
    name = pretty (scale_id scale) <> ": " <> scale_pattern scale
    to_note (Expr.Symbol sym) = Pitch.Note sym

-- | Convert a val call to a pitch call.  This is used so scales can export
-- their ValCalls to pitch generators.
val_to_pitch :: ValCall -> Generator Pitch
val_to_pitch (ValCall name doc vcall) = Call
    { call_name = name
    , call_doc = doc
    , call_func = generator_func $ pitch_call . convert_args
    }
    where
    convert_args args = args { passed_ctx = tag_context (passed_ctx args) }
    pitch_call args = vcall args >>= \val -> case val of
        BaseTypes.VPitch pitch -> do
            -- Previously I dispatched to '', which is normally
            -- 'Derive.Call.Pitch.c_set'.  That would be more flexible since
            -- you can then override '', but is also less efficient.
            pos <- Internal.real $ Event.start $ ctx_event $ passed_ctx args
            return $ Stream.from_event $ PSignal.set (prev_pitch args) pos pitch
        _ -> throw $ "scale call " <> pretty name
            <> " returned non-pitch: " <> ShowVal.show_val val
    -- This is like Args.prev_pitch, but importing Args would make a circular
    -- import.
    prev_pitch = fmap snd . PSignal.last <=< from_tagged
        <=< ctx_prev_val . passed_ctx

-- | Run the a deriver with the given instrument in scope.  In addition to
-- assigning the instrument to the 'EnvKey.instrument' field where note calls
-- can inherit it, this also brings the 'Instrument' fields into scope, which
-- is the per-instrument calls and per-instrument environ.
with_instrument :: Score.Instrument -> Deriver d -> Deriver d
with_instrument inst deriver = do
    -- Previously, I would just substitute an empty instrument instead of
    -- throwing, but it turned out to be error prone, since a misspelled
    -- instrument would derive anyway, only without the right calls and
    -- environ.
    (inst, derive_inst) <- get_instrument inst
    let with_inst = with_val_raw EnvKey.instrument inst
    with_inst $ with_scopes (set_scopes (inst_calls derive_inst)) $
        with_environ (inst_environ derive_inst) deriver
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

with_instrument_alias :: Score.Instrument -> Score.Instrument
    -> Deriver a -> Deriver a
with_instrument_alias alias inst =
    with_instrument_aliases (Map.singleton alias inst)

with_instrument_aliases :: Map Score.Instrument Score.Instrument
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

instrument_exists :: Score.Instrument -> Deriver Bool
instrument_exists = (Maybe.isJust . snd <$>) . lookup_instrument

get_instrument :: Score.Instrument -> Deriver (Score.Instrument, Instrument)
get_instrument score_inst = do
    (real_inst, result) <- lookup_instrument score_inst
    case result of
        Nothing -> throw $ "no instrument named "
            <> "'" <> ShowVal.show_val real_inst <> "'"
            <> if real_inst == score_inst then ""
                else " (aliased from " <> ShowVal.show_val score_inst <> ")"
        Just inst -> return (real_inst, inst)

-- | Look up the instrument.  Also return the instrument name after resolving
-- any alias.  This is what goes in 'Score.event_instrument', since it's what
-- the performer understands.
lookup_instrument :: Score.Instrument
    -> Deriver (Score.Instrument, Maybe Instrument)
lookup_instrument inst = do
    aliases <- Internal.get_dynamic state_instrument_aliases
    let real_inst = Map.findWithDefault inst inst aliases
    lookup_inst <- gets $ state_lookup_instrument . state_constant
    return (real_inst, lookup_inst real_inst)

-- | Merge the given environ into the environ in effect.
with_environ :: Env.Environ -> Deriver a -> Deriver a
with_environ environ
    | Env.null environ = id
    | otherwise = Internal.local $ \state -> state
        { state_environ = environ <> state_environ state }


-- ** control

-- | Return an entire signal.
get_control :: Score.Control -> Deriver (Maybe (RealTime -> Score.TypedVal))
get_control control = get_control_function control >>= \x -> case x of
    Just f -> return $ Just f
    Nothing -> get_control_signal control >>= return . fmap signal_function

signal_function :: Score.TypedControl -> (RealTime -> Score.TypedVal)
signal_function sig t = Signal.at t <$> sig

get_control_signal :: Score.Control -> Deriver (Maybe Score.TypedControl)
get_control_signal control = Map.lookup control <$> get_controls

get_controls :: Deriver Score.ControlMap
get_controls = Internal.get_dynamic state_controls

get_control_functions :: Deriver Score.ControlFunctionMap
get_control_functions = Internal.get_dynamic state_control_functions

-- | Get the control value at the given time, taking 'state_control_functions'
-- into account.
control_at :: Score.Control -> RealTime -> Deriver (Maybe Score.TypedVal)
control_at control pos = get_control_function control >>= \x -> case x of
    Just f -> return $ Just $ f pos
    Nothing -> do
        maybe_sig <- Map.lookup control <$> get_controls
        return $ fmap (Signal.at pos) <$> maybe_sig

get_control_function :: Score.Control
    -> Deriver (Maybe (RealTime -> Score.TypedVal))
get_control_function control = do
    functions <- Internal.get_dynamic state_control_functions
    case Map.lookup control functions of
        Nothing -> return Nothing
        Just f -> do
            dyn <- Internal.get_control_function_dynamic
            return $ Just $ BaseTypes.call_control_function f control dyn

untyped_control_at :: Score.Control -> RealTime -> Deriver (Maybe Signal.Y)
untyped_control_at cont = fmap (fmap Score.typed_val) . control_at cont

-- | Get a ControlValMap at the given time, taking 'state_control_functions'
-- into account.
controls_at :: RealTime -> Deriver Score.ControlValMap
controls_at pos = do
    state <- get
    ruler <- Internal.get_ruler
    return $ state_controls_at pos ruler (state_dynamic state)
        (state_event_serial (state_threaded state))

state_controls_at :: RealTime -> Ruler.Marklists
    -- ^ Ruler marklists from the same track as the Dynamic.  Needed by
    -- control functions, via 'BaseTypes.dyn_ruler'.
    -> Dynamic -> Int -- ^ 'state_event_serial'
    -> Score.ControlValMap
state_controls_at pos ruler dyn serial = Map.fromList $
    map (resolve (Internal.convert_dynamic ruler dyn serial) pos) $
    Seq.equal_pairs (\a b -> fst a == fst b)
        (Map.toAscList fs) (Map.toAscList controls)
    where
    fs = state_control_functions dyn
    controls = state_controls dyn
    resolve cf_dyn pos p = case p of
        Seq.Both (k, f) _ -> (k, call k f)
        Seq.First (k, f) -> (k, call k f)
        Seq.Second (k, sig) -> (k, Signal.at pos (Score.typed_val sig))
        where
        call control f = Score.typed_val $
            BaseTypes.call_control_function f control cf_dyn pos

-- *** control signal

with_control :: Score.Control -> Score.TypedControl -> Deriver a -> Deriver a
with_control control signal = with_controls [(control, signal)]

with_constant_control :: Score.Control -> Score.Typed Signal.Y -> Deriver a
    -> Deriver a
with_constant_control control val =
    with_control control (Signal.constant <$> val)

with_controls :: [(Score.Control, Score.TypedControl)] -> Deriver a -> Deriver a
with_controls controls
    | null controls = id
    | otherwise = Internal.local $ \state -> state
        { state_controls = Util.Map.insert_list controls (state_controls state)
        }

-- | Remove both controls and control functions.  Use this when a control has
-- already been applied, and you don't want it to affect further derivation.
remove_controls :: [Score.Control] -> Deriver a -> Deriver a
remove_controls controls
    | null controls = id
    | otherwise = Internal.local $ \state-> state
        { state_controls = Util.Map.delete_keys controls (state_controls state)
        , state_control_functions =
            Util.Map.delete_keys controls (state_control_functions state)
        }

with_control_function :: Score.Control -> BaseTypes.ControlFunction
    -> Deriver a -> Deriver a
with_control_function control f = Internal.local $ \state -> state
    { state_control_functions =
        Map.insert control f (state_control_functions state)
    }

-- | Replace the controls entirely.
with_control_maps :: Score.ControlMap -> Score.ControlFunctionMap
    -> Deriver a -> Deriver a
with_control_maps cmap cfuncs = Internal.local $ \state -> state
    { state_controls = cmap
    , state_control_functions = cfuncs
    }

-- | Modify the given control according to the Merger.
--
-- If both signals are typed, the existing type wins over the relative
-- signal's type.  If one is untyped, the typed one wins.
--
-- As documetned in 'merge', this acts like a Set if there is no existing
-- control.
with_merged_control :: Merger Signal.Control -> Score.Control
    -> Score.TypedControl -> Deriver a -> Deriver a
with_merged_control merger control signal deriver = do
    controls <- get_controls
    let new = merge merger (Map.lookup control controls) signal
    with_control control new deriver

-- | Like 'with_controls', but merge them with their respective default
-- 'Merger's.
with_merged_controls :: [(Score.Control, Score.TypedControl)] -> Deriver a
    -> Deriver a
with_merged_controls control_vals deriver
    | null control_vals = deriver
    | otherwise = do
        let (controls, new_vals) = unzip control_vals
        mergers <- mapM get_default_merger controls
        signals <- get_controls
        let old_vals = map (flip Map.lookup signals) controls
            merged = zipWith3 merge mergers old_vals new_vals
        with_controls (zip controls merged) deriver

resolve_merge :: Merge Signal.Control -> Score.Control
    -> Deriver (Merger Signal.Control)
resolve_merge DefaultMerge control = get_default_merger control
resolve_merge (Merge merger) _ = return merger

get_control_merge :: Expr.Symbol -> Deriver (Merger Signal.Control)
get_control_merge name = do
    mergers <- gets (state_mergers . state_constant)
    require ("unknown control merger: " <> ShowVal.show_val name)
        (Map.lookup name mergers)

-- | Get the default merger for this control, or 'merge_mul' if there is none.
get_default_merger :: Score.Control -> Deriver (Merger Signal.Control)
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
merge :: Merger Signal.Control -> Maybe Score.TypedControl
    -> Score.TypedControl -> Score.TypedControl
merge Set _ new = new
merge (Merger _ merger ident) maybe_old new =
    Score.Typed (Score.type_of old <> Score.type_of new)
        (merger (Score.typed_val old) (Score.typed_val new))
    where old = fromMaybe (Score.untyped ident) maybe_old
    -- Using ident is *not* the same as just emitting the 'new' signal for
    -- subtraction!

merge_vals :: Merger Signal.Control -> Signal.Y -> Signal.Y -> Signal.Y
merge_vals merger old new = case merger of
    Set -> new
    Merger _ merge _ -> maybe new snd $ Signal.head $
        merge (Signal.constant old) (Signal.constant new)
        -- This is awkward.  Maybe the merge function should be on scalars?

-- *** ControlMod

-- | Emit a 'ControlMod'.
modify_control :: Merger Signal.Control -> Score.Control -> Signal.Control
    -> Deriver ()
modify_control merger control signal = Internal.modify_collect $ \collect ->
    collect { collect_control_mods =
        ControlMod control signal merger : collect_control_mods collect }

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
with_control_mods mods end deriver = do
    foldr ($) deriver (map apply mods)
    where
    apply (ControlMod control signal merger) =
        with_merged_control merger control $ Score.untyped $
            Signal.drop_at_after end signal

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
named_pitch_at :: Score.PControl -> RealTime -> Deriver (Maybe PSignal.Pitch)
named_pitch_at name pos = do
    psig <- get_named_pitch name
    return $ maybe Nothing (PSignal.at pos) psig

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

get_named_pitch :: Score.PControl -> Deriver (Maybe PSignal.PSignal)
get_named_pitch name
    | name == Score.default_pitch = Just <$> Internal.get_dynamic state_pitch
    | otherwise = Map.lookup name <$> Internal.get_dynamic state_pitches

named_nn_at :: Score.PControl -> RealTime -> Deriver (Maybe Pitch.NoteNumber)
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

-- | Run the deriver in a context with the given pitch signal.
with_merged_pitch :: Merger PSignal.PSignal -> Score.PControl
    -> PSignal.PSignal -> Deriver a -> Deriver a
with_merged_pitch merger name signal deriver = do
    modify_pitch name (\old -> apply_pitch_merger merger old signal) deriver

resolve_pitch_merge :: Merge PSignal.PSignal -> Merger PSignal.PSignal
resolve_pitch_merge DefaultMerge = Set
resolve_pitch_merge (Merge merger) = merger

get_pitch_merger :: Expr.Symbol -> Deriver (Merger PSignal.PSignal)
get_pitch_merger name = do
    mergers <- gets (state_pitch_mergers . state_constant)
    require ("unknown pitch merger: " <> showt name) (Map.lookup name mergers)

with_pitch :: PSignal.PSignal -> Deriver a -> Deriver a
with_pitch = with_merged_pitch Set Score.default_pitch

with_constant_pitch :: PSignal.Pitch -> Deriver a -> Deriver a
with_constant_pitch = with_pitch . PSignal.constant

remove_pitch :: Deriver a -> Deriver a
remove_pitch = modify_pitch Score.default_pitch (const mempty)

modify_pitch :: Score.PControl -> (Maybe PSignal.PSignal -> PSignal.PSignal)
    -> Deriver a -> Deriver a
modify_pitch pcontrol f
    | pcontrol == Score.default_pitch = Internal.local $ \state ->
        state { state_pitch = f (Just (state_pitch state)) }
    | otherwise = Internal.local $ \state -> state
        { state_pitches = Map.alter (Just . f) pcontrol (state_pitches state) }

apply_pitch_merger :: Merger PSignal.PSignal -> Maybe PSignal.PSignal
    -> PSignal.PSignal -> PSignal.PSignal
apply_pitch_merger _ Nothing new = new
apply_pitch_merger Set (Just _) new = new
apply_pitch_merger (Merger _ merger _) (Just old) new = merger old new


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
get_score_duration :: Deriver a -> Deriver (CallDuration ScoreTime)
get_score_duration deriver = do
    state <- get
    let (_, out, _) = run (set_mode state) deriver
    return $ collect_score_duration $ state_collect out
    where
    set_mode state = state
        { state_collect = mempty
        , state_dynamic = (state_dynamic state)
            { state_mode = ScoreDurationQuery }
        }

get_real_duration :: Deriver a -> Deriver (CallDuration RealTime)
get_real_duration deriver = do
    state <- get
    let (_, out, _) = run (set_mode state) deriver
    return $ collect_real_duration $ state_collect out
    where
    set_mode state = state
        { state_collect = mempty
        , state_dynamic = (state_dynamic state)
            { state_mode = RealDurationQuery }
        }

-- * postproc

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
shift_control :: ScoreTime -> Deriver a -> Deriver a
shift_control shift deriver = do
    real <- Internal.real shift
    Internal.local
        (\state -> state
            { state_controls = nudge real (state_controls state)
            , state_pitch = nudge_pitch real (state_pitch state)
            })
        deriver
    where
    nudge delay = Map.map (fmap (Signal.shift delay))
    nudge_pitch = PSignal.shift

-- * call

-- | Wrap 'make_val_call' with a 'Typecheck.to_val' to automatically convert
-- to a 'BaseTypes.Val'.  This is not in "Derive.Deriver.Monad" to avoid
-- a circular import with "Derive.BaseTypes".
val_call :: Typecheck.ToVal a => Module.Module -> CallName -> Tags.Tags
    -> Doc.Doc -> WithArgDoc (PassedArgs Tagged -> Deriver a) -> ValCall
val_call module_ name tags doc (call, arg_docs) =
    make_val_call module_ name tags doc (fmap Typecheck.to_val . call, arg_docs)

set_module :: Module.Module -> Call f -> Call f
set_module module_ call = call
    { call_doc = (call_doc call) { cdoc_module = module_ } }
