-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
{- | This has the higher level parts of the deriver library.  That is,
    functions where are considered basic but can be defined outside of
    "Derive.Deriver.Monad".
-}
module Derive.Deriver.Lib (
    Result(..)
    , derive
    , extractResult
    , withDefaultImported
    -- * errors
    , require, requireRight
    , catch

    -- * state access
    , getStack
    , realFunction, scoreFunction
    -- ** import
    , withImported, withImportedSymbols
    , withScopes
    -- ** scale
    , getScale, lookupScale

    -- ** environment
    , lookupVal, isValSet, getVal
    , withVal, withVals
    , withEnviron
    , withValRaw
    , removeVal, removeVals
    , modifyVal
    , withScale, withInstrument
    , withInstrumentAlias, withInstrumentAliases
    , instrumentExists
    , getInstrument, lookupInstrument
    , getTempo

    -- ** control
    , lookupSignal
    , lookupFunction, getFunction
    , isControlSet
    , getControlMap
    , getFunctionMap
    , controlAt, untypedControlAt, controlsAt
    , modifySignals

    -- *** control signal
    , withControl, withConstantControl
    , withControls
    , removeControls
    , withMergedControl, withMergedControls
    , resolveMerge
    , getControlMerge
    , getDefaultMerger

    -- *** ControlMod
    , modifyControl
    , evalControlMods
    , withControlMods

    -- ** pitch
    , pitchAt, namedPitchAt
    , resolvePitch
    , nnAt
    , getPitch
    , namedNnAt
    , lookupPitchSignal
    , loggedPitchNn

    -- *** with signal
    , withPitch, withNamedPitch, withConstantPitch
    , removePitch

    -- * run monad
    , runLogs

    -- * Mode
    , getMode
    , isLilypondMode
    , lookupLilypondConfig
    , getScoreDuration, getRealDuration

    -- * postproc
    , withEvent
    , withEventStack
    , shiftControls

    -- * call
    , valCall
    , setModule
) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Maps as Maps

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
data Result = Result
    { rEvents :: Stream.Stream Score.Event
    , rCache :: Cache
    , rTrackWarps :: [TrackWarp.TrackWarp]
    , rTrackSignals :: Track.TrackSignals
    , rTrackDynamic :: TrackDynamic
    , rIntegrated :: [Integrated]

    -- | The relevant parts of the final state should be extracted into the
    -- above fields, but returning the whole state can be useful for testing.
    , rState :: State
    }

-- | Kick off a derivation.
derive :: Constant -> Dynamic -> Deriver a -> RunResult a
derive constant dynamic = run (initialState constant dynamic)
    . withInitialScope (stateEnviron dynamic)
    . withDefaultImported

extractResult :: RunResult (Stream.Stream Score.Event) -> Result
extractResult (result, state, logs) = Result
    { rEvents = mergeLogs result (moreLogs ++ logs)
    , rCache = collectCache collect <> stateCache (stateConstant state)
    , rTrackWarps =
        TrackWarp.collectTrackWarps blocks (collectWarpMap collect)
    , rTrackSignals = collectTrackSignals collect
    , rTrackDynamic = extractTrackDynamic collect
    , rIntegrated = collectIntegrated collect
    , rState = state
    }
    where
    (moreLogs, blocks) =
        case Ui.runId uiState TrackWarp.getTrackTrees of
            Left err -> ([Log.msg Log.Warn Nothing msg], [])
                where msg = "error collecting TrackWarps: " <> pretty err
            Right (blocks, _, _) -> ([], blocks)
    uiState = stateUi $ stateConstant state
    collect = stateCollect state

-- | Extract the merged TrackDynamic from the Collect.
--
-- 'EnvKey.scale' comes from the inverted Collect because the scale track is
-- often inverted below the note track.  However, the others come from the
-- non-inverted Collect because if the note track sets an instrument, I want to
-- use its instrument, instead of any instrument on individual events.  E.g.
-- @>kendang-pasang@ has events that are @>kendang-wadon@ or @>kendang-lanang@.
--
-- See 'Collect' and 'TrackDynamic' for why.
extractTrackDynamic :: Collect -> TrackDynamic
extractTrackDynamic collect =
    stripDynamic . extract <$> Maps.paired
        (collectTrackDynamic collect) (collectTrackDynamicInverted collect)
    where
    extract = \case
        Lists.First dyn -> dyn
        Lists.Second dyn -> dyn
        Lists.Both normal inverted -> merge normal inverted
    merge normal inverted = normal
        { stateEnviron = keep (stateEnviron inverted) <> stateEnviron normal
        }
    keep env = maybe mempty (Env.fromList . (:[]) . (,) EnvKey.scale) $
        Env.lookup EnvKey.scale env

-- | Given an environ, bring instrument and scale calls into scope.
withInitialScope :: Env.Environ -> Deriver d -> Deriver d
withInitialScope env deriver = setInst (setScale deriver)
    where
    setInst = case Env.getVal EnvKey.instrument env of
        Right inst -> withInstrument inst
        _ -> id
    setScale = case Env.getVal EnvKey.scale env of
        Right str -> \deriver -> do
            scale <- getScale (Expr.strToScaleId str)
            withScale scale deriver
        _ -> id

withDefaultImported :: Deriver a -> Deriver a
withDefaultImported deriver =
    foldr (withImported True) deriver
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
    let (result, state2, logs) = run (state { stateCollect = mempty }) deriver
    mapM_ Log.write logs
    case result of
        Left err -> do
            Log.write $ errorToWarn err
            pure Nothing
        Right val -> do
            when collect $ Internal.mergeCollect (stateCollect state2)
            Internal.setThreaded (stateThreaded state2)
            pure $ Just val


-- * state access

getStack :: Deriver Stack.Stack
getStack = gets (stateStack . stateDynamic)

realFunction :: Deriver (ScoreTime -> RealTime)
realFunction = Warp.warp <$> Internal.getDynamic stateWarp

scoreFunction :: Deriver (RealTime -> ScoreTime)
scoreFunction = Warp.unwarp <$> Internal.getDynamic stateWarp

-- ** import

-- | Merge calls from the given module into scope.
withImported :: Bool -> Module.Module -> Deriver a -> Deriver a
withImported emptyOk module_ deriver = do
    builtins <- Internal.getConstant stateBuiltins
    scopes <- case extractModule module_ builtins of
        Scopes gen trans track val
            | not emptyOk && scopeEmpty gen && scopeEmpty trans
                    && scopeEmpty track && empty val ->
                throw $ "no calls in the imported module: " <> pretty module_
        extracted -> pure extracted
    withScopes (scopes<>) deriver
    where
    scopeEmpty (Scope a b c) = empty a && empty b && empty c
    empty (ScopePriority m) = Map.null m

-- | Import only the given symbols from the module.
withImportedSymbols :: Module.Module -> Set Expr.Symbol -> Deriver a
    -> Deriver a
withImportedSymbols module_ syms deriver = do
    scopes <- extractSymbols (`Set.member` syms) . extractModule module_ <$>
        Internal.getConstant stateBuiltins
    let missing = syms `Set.difference` Set.fromList (scopeSymbols scopes)
    unless (Set.null missing) $
        throw $ "symbols not in module " <> pretty module_ <> ": "
            <> pretty (Set.toList missing)
    withScopes (scopes<>) deriver

-- | Run the derivation with a modified scope.
withScopes :: (Scopes -> Scopes) -> Deriver a -> Deriver a
withScopes modify = Internal.local $ \state ->
    state { stateScopes = modify (stateScopes state) }

-- | Filter out any calls that aren't in the given module.
extractModule :: Module.Module -> Builtins -> Scopes
extractModule module_ (Scopes gen trans track val) = Scopes
    { scopesGenerator = extractScope gen
    , scopesTransformer = extractScope trans
    , scopesTrack = extractScope track
    , scopesVal = extract val
    }
    where
    extractScope (Scope note control pitch) = Scope
        { scopeNote = extract note
        , scopeControl = extract control
        , scopePitch = extract pitch
        }
    extract = make . Map.findWithDefault mempty module_
    make :: CallMap a -> ScopePriority a
    make cmap@(CallMap calls patterns)
        | Map.null calls && null patterns = scopePriority []
        | null prioBlock = scopePriority [(PrioBuiltin, cmap)]
        | otherwise = scopePriority
            [ (PrioBuiltin, cmap { callPatterns = normal })
            , (PrioBlock, mempty { callPatterns = prioBlock })
            ]
        where
        (prioBlock, normal) =
            List.partition hasPrioBlock (callPatterns cmap)
    hasPrioBlock pattern =
        cdocTags (patCallDoc pattern) `Tags.contains` Tags.prioBlock

-- | Filter out calls that don't match the predicate.  LookupCalls are also
-- filtered out.  This might be confusing since you might not even know a
-- call comes from a LookupPattern, but then you can't import it by name.
extractSymbols :: (Expr.Symbol -> Bool) -> Scopes -> Scopes
extractSymbols wanted (Scopes gen trans track val) = Scopes
    { scopesGenerator = extractScope gen
    , scopesTransformer = extractScope trans
    , scopesTrack = extractScope track
    , scopesVal = extract val
    }
    where
    extractScope (Scope note control pitch) = Scope
        { scopeNote = extract note
        , scopeControl = extract control
        , scopePitch = extract pitch
        }
    extract = mapCmap $ \cmap -> mempty
        { callMap = Maps.filterKey wanted (callMap cmap) }
    mapCmap f (ScopePriority m) = ScopePriority $ f <$> m

scopeSymbols :: Scopes -> [Expr.Symbol]
scopeSymbols (Scopes gen trans track val) = mconcat
    [extractScope gen, extractScope trans, extractScope track, extract val]
    where
    extractScope (Scope note control pitch) =
        extract note <> extract control <> extract pitch
    extract (ScopePriority m) = concatMap (Map.keys . callMap) (Map.elems m)

-- ** scale

-- | Lookup a scale_id or throw.
getScale :: Pitch.ScaleId -> Deriver Scale
getScale scaleId =
    maybe (throw $ "get_scale: unknown scale: " <> pretty scaleId) pure
    =<< lookupScale scaleId

lookupScale :: Pitch.ScaleId -> Deriver (Maybe Scale)
lookupScale scaleId = do
    LookupScale lookup <- gets (stateLookupScale . stateConstant)
    env <- Internal.getEnviron
    case lookup env scaleId of
        Nothing -> pure Nothing
        Just (Left err) -> throw $ "lookup " <> pretty scaleId <> ": "
            <> pretty err
        Just (Right scale) -> pure $ Just scale


-- ** environment

lookupVal :: Typecheck.Typecheck a => Env.Key -> Deriver (Maybe a)
lookupVal key =
    either throw pure . Env.checkedVal key =<< Internal.getEnviron

isValSet :: Env.Key -> Deriver Bool
isValSet key = Maybe.isJust . Env.lookup key <$> Internal.getEnviron

-- | Like 'lookup_val', but throw if the value isn't present.
getVal :: Typecheck.Typecheck a => Env.Key -> Deriver a
getVal key = do
    val <- lookupVal key
    maybe (throw $ "environ val not found: " <> pretty key) pure val

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
withVal :: Typecheck.ToVal val => Env.Key -> val -> Deriver a -> Deriver a
withVal key val deriver
    | key == EnvKey.scale, Just scaleId <- DeriveT.toScaleId v = do
        scale <- getScale scaleId
        withValRaw EnvKey.scale val $ withScale scale deriver
    | key == EnvKey.instrument, Just inst <- Typecheck.fromValSimple v =
        withInstrument inst deriver
    | otherwise = do
        env <- Internal.getEnviron
        case Env.putVal key v env of
            Left err -> throw err
            Right env -> Internal.local
                (\state -> state { stateEnviron = env }) deriver
    where v = Typecheck.toVal val

-- | Like 'with_val', but should be slightly more efficient for setting
-- multiple values at once.
withVals :: Typecheck.ToVal val => [(Env.Key, val)] -> Deriver a -> Deriver a
withVals vals deriver
    | null vals = deriver
    | any (`elem` [EnvKey.scale, EnvKey.instrument]) (map fst vals) =
        foldr (uncurry withVal) deriver vals
    | otherwise = Internal.localm with deriver
    where
    with state = do
        environ <- either throw pure $
            foldr (\(k, v) env -> Env.putVal k v =<< env)
                (pure $ stateEnviron state) vals
        environ `seq` pure $! state { stateEnviron = environ }

-- | Merge the given environ into the environ in effect.  Unlike 'with_val' or
-- 'with_vals', this won't set scopes for 'EnvKey.scale' and
-- 'EnvKey.instrument'.
withEnviron :: Env.Environ -> Deriver a -> Deriver a
withEnviron environ
    | Env.null environ = id
    | otherwise = Internal.local $ \state -> state
        { stateEnviron = environ <> stateEnviron state }

-- | Like 'with_val', but don't set scopes for instrument and scale.  Also
-- don't check for types, so you can replace a val with one of a different
-- type.  Due to this it's also more efficient.
withValRaw :: Typecheck.ToVal val => Env.Key -> val -> Deriver a -> Deriver a
withValRaw key val = Internal.local $ \state -> state
    { stateEnviron = Env.insertVal key val (stateEnviron state) }

withValsRaw :: Typecheck.ToVal val => [(EnvKey.Key, val)] -> Deriver a
    -> Deriver a
withValsRaw vals =
    withEnviron $ Env.fromList (map (second Typecheck.toVal) vals)

removeVal :: Env.Key -> Deriver a -> Deriver a
removeVal = removeVals . (:[])
-- remove_val key = Internal.local $ \state ->
--     state { state_environ = Env.delete key $ state_environ state }

removeVals :: [Env.Key] -> Deriver a -> Deriver a
removeVals keys = Internal.local $ \state ->
    state { stateEnviron = foldr Env.delete (stateEnviron state) keys }

modifyVal :: (Typecheck.Typecheck val, Typecheck.ToVal val) => Env.Key
    -> (Maybe val -> val) -> Deriver a -> Deriver a
modifyVal key modify = Internal.localm $ \state -> do
    val <- modify <$>
        requireRight id (Env.checkedVal key (stateEnviron state))
    pure $! insertEnv key val state

insertEnv :: Typecheck.ToVal val => Env.Key -> val -> Dynamic -> Dynamic
insertEnv key val state = state
    { stateEnviron =
        Env.insertVal key (Typecheck.toVal val) (stateEnviron state)
    }

-- | Replace the scale's calls.
--
-- Previously this used 'add_priority' instead of 'replace_priority', which
-- meant you could overlay scales and use both at the same time.  Maybe that's
-- actually a useful feature?  In any case, I don't need it at the moment, so
-- it seems more likely to be confusing than useful.
withScale :: Scale -> Deriver d -> Deriver d
withScale scale = withScopes (val . pitch)
    where
    pitch = sGenerator#sPitch %= replace (scaleToCall scale valToPitch)
    val = sVal %= replace (scaleToCall scale id)
    replace pattern =
        replacePriority PrioScale (mempty { callPatterns = [pattern] })

scaleToCall :: Scale -> (ValCall -> call) -> PatternCall call
scaleToCall scale convert = PatternCall
    { patDescription = description
    , patDoc = scaleCallDoc scale
    , patFunction = \sym ->
        pure $ convert <$> scaleNoteToCall scale (toNote sym)
    }
    where
    description = pretty (scaleId scale) <> ": " <> scalePattern scale
    toNote (Expr.Symbol sym) = Pitch.Note sym

-- | Convert a val call to a pitch call.  This is used so scales can export
-- their ValCalls to pitch generators.
valToPitch :: ValCall -> Generator Pitch
valToPitch (Call name doc vcall) = Call
    { callName = name
    , callDoc = doc
    , callFunc = generatorFunc pitchCall
    }
    where
    convertArgs args = args { passedCtx = tagContext (passedCtx args) }
    pitchCall args = do
        val <- vcall (convertArgs args)
        case PSignal.pitchVal val of
            Just pitch -> lookupCall Symbols.defaultPitch >>= \case
                Nothing -> defaultPitchCall args pitch
                Just pcall -> gfuncF (callFunc pcall) $ PassedArgs
                    { passedVals = [DeriveT.VPSignal (PSignal.constant pitch)]
                    , passedCallName = callName pcall
                    , passedCtx = passedCtx args
                    }
            Nothing -> throw $ "scale call " <> pretty name
                <> " returned non-pitch: " <> ShowVal.showVal val

-- | This is the default pitch call for bare scale degrees if
-- 'Symbols.default_pitch' isn't set.
defaultPitchCall :: PassedArgs val -> PSignal.Pitch
    -> Deriver (Stream.Stream PSignal.PSignal)
defaultPitchCall args pitch = do
    -- This is Args.real, but I can't import it here.
    pos <- Internal.real $ Event.start $ ctxEvent $ passedCtx args
    pure $ Stream.fromEvent $ PSignal.fromSample pos pitch

-- | Run the a deriver with the given instrument in scope.  In addition to
-- assigning the instrument to the 'EnvKey.instrument' field where note calls
-- can inherit it, this also brings the 'Instrument' fields into scope, which
-- is the per-instrument calls and per-instrument environ.
withInstrument :: ScoreT.Instrument -> Deriver d -> Deriver d
withInstrument inst deriver = do
    -- Previously, I would just substitute an empty instrument instead of
    -- throwing, but it turned out to be error prone, since a misspelled
    -- instrument would derive anyway, only without the right calls and
    -- environ.
    (inst, deriveInst) <- getInstrument inst
    let withInst = withValRaw EnvKey.instrument inst
    let withScale = maybe id (withVal EnvKey.scale) $
            Env.lookup EnvKey.scale (instEnviron deriveInst)
    withInst $ withScopes (setScopes (instCalls deriveInst)) $
        withScale $ withEnviron (instEnviron deriveInst) deriver
    where
    -- Replace the calls in the instrument scope type.
    setScopes (Scopes instGen instTrans instTrack instVal)
            (Scopes gen trans track val) =
        Scopes
            { scopesGenerator = (sNote %= replace instGen) gen
            , scopesTransformer = (sNote %= replace instTrans) trans
            , scopesTrack = (sNote %= replace instTrack) track
            , scopesVal = replace instVal val
            }
    replace = replacePriority PrioInstrument

withInstrumentAlias :: ScoreT.Instrument -> ScoreT.Instrument
    -> Deriver a -> Deriver a
withInstrumentAlias alias inst =
    withInstrumentAliases (Map.singleton alias inst)

withInstrumentAliases :: Map ScoreT.Instrument ScoreT.Instrument
    -> Deriver a -> Deriver a
withInstrumentAliases aliases deriver
    | Map.null aliases = deriver
    -- I used to verify that the rhs insts exist, but verification can be
    -- annoying if there is a library ky that creates some general purpose
    -- aliases, e.g. >r{1..4} = >r.
    | otherwise = Internal.local with deriver
    where
    with state = state
        { stateInstrumentAliases = (resolve <$> aliases) <> oldAliases }
        where
        oldAliases = stateInstrumentAliases state
        resolve inst = Map.findWithDefault inst inst oldAliases

instrumentExists :: ScoreT.Instrument -> Deriver Bool
instrumentExists = (Either.isRight . snd <$>) . lookupInstrument

getInstrument :: ScoreT.Instrument -> Deriver (ScoreT.Instrument, Instrument)
getInstrument scoreInst = do
    (realInst, result) <- lookupInstrument scoreInst
    case result of
        Left err -> throw $ "instrument "
            <> "'" <> ShowVal.showVal realInst <> "': " <> err
            <> if realInst == scoreInst then ""
                else " (aliased from " <> ShowVal.showVal scoreInst <> ")"
        Right inst -> pure (realInst, inst)

-- | Look up the instrument.  Also return the instrument name after resolving
-- any alias.  This is what goes in 'Score.event_instrument', since it's what
-- the performer understands.
lookupInstrument :: ScoreT.Instrument
    -> Deriver (ScoreT.Instrument, Either Text Instrument)
lookupInstrument inst = do
    aliases <- Internal.getDynamic stateInstrumentAliases
    let realInst = Map.findWithDefault inst inst aliases
    lookupInst <- gets $ stateLookupInstrument . stateConstant
    pure (realInst, lookupInst realInst)

-- | Get instantaneous tempo at the given time.
getTempo :: ScoreTime -> Deriver Double
getTempo t = do
    warp <- Internal.getWarp
    pure $ Warp.tempoAt warp t


-- ** control

lookupSignal :: ScoreT.Control -> Deriver (Maybe (ScoreT.Typed Signal.Control))
lookupSignal = Typecheck.lookupSignal

lookupFunction :: ScoreT.Control -> Deriver (Maybe ScoreT.TypedFunction)
lookupFunction = Typecheck.lookupFunction . flip DeriveT.Ref Nothing

getFunction :: ScoreT.Control -> Deriver ScoreT.TypedFunction
getFunction = Typecheck.resolveFunction . flip DeriveT.Ref Nothing

-- | Get the control value at the given time.
controlAt :: ScoreT.Control -> RealTime
    -> Deriver (Maybe (ScoreT.Typed Signal.Y))
controlAt control pos = fmap (fmap ($ pos)) <$> lookupFunction control

untypedControlAt :: ScoreT.Control -> RealTime -> Deriver (Maybe Signal.Y)
untypedControlAt control = fmap (fmap ScoreT.valOf) . controlAt control

isControlSet :: ScoreT.Control -> Deriver Bool
isControlSet = isValSet . ScoreT.controlName

-- *** all signals / functions

-- TODO for notes, to get just signals, no cfs.  Keeping with function / signal
-- naming, should be get_signals and SignalMap?
getControlMap :: Deriver ScoreT.ControlMap
getControlMap =
    fmap Map.fromList $ mapMaybeM convert . Env.toList =<< Internal.getEnviron
    where
    convert (key, val) = fmap (ScoreT.Control key,) <$> toSignal val
    toSignal val = case Typecheck.valToSignal val of
        Nothing -> pure Nothing
        Just (Right sig) -> pure $ Just sig
        Just (Left dsig) -> Just <$> dsig

-- | Like 'get_function', but get them all.
getFunctionMap :: Deriver ScoreT.FunctionMap
getFunctionMap = do
    cfDyn <- Internal.getControlFunctionDynamic
    let toFunction val = case Typecheck.valToFunctionDyn cfDyn val of
            Nothing -> pure Nothing
            Just (Left dtf) -> Just <$> dtf
            Just (Right tf) -> pure $ Just tf
    let resolve (key, val) = fmap (ScoreT.Control key,) <$> toFunction val
    Map.fromAscList <$>
        (mapMaybeM resolve . Env.toList =<< Internal.getEnviron)

-- | Get a ControlValMap at the given time.
{-# SCC controlsAt #-}
controlsAt :: RealTime -> Deriver ScoreT.ControlValMap
controlsAt = fmap (fmap ScoreT.valOf) . typedControlsAt

typedControlsAt :: RealTime -> Deriver ScoreT.TypedControlValMap
typedControlsAt pos = fmap (fmap ($ pos)) <$> getFunctionMap

-- *** control signal

withControl :: ScoreT.Control -> ScoreT.Typed Signal.Control -> Deriver a
    -> Deriver a
withControl control signal = withValRaw (ScoreT.controlName control) signal

withConstantControl :: ScoreT.Control -> Signal.Y -> Deriver a -> Deriver a
withConstantControl control val =
    withControl control (ScoreT.untyped (Signal.constant val))

withControls :: [(ScoreT.Control, ScoreT.Typed Signal.Control)] -> Deriver a
    -> Deriver a
withControls controls =
    withValsRaw (map (first ScoreT.controlName) controls)

-- | Remove both controls and control functions.  Use this when a control has
-- already been applied, and you don't want it to affect further derivation.
removeControls :: [ScoreT.Control] -> Deriver a -> Deriver a
removeControls controls = Internal.local $ \state -> state
    { stateEnviron = foldr Env.delete (stateEnviron state) keys }
    where keys = map ScoreT.controlName controls

-- | Modify the given control according to the Merger.
--
-- If both signals are typed, the existing type wins over the relative
-- signal's type.  If one is untyped, the typed one wins.
--
-- As documented in 'merge', this acts like a Set if there is no existing
-- control.
withMergedControl :: Merger -> ScoreT.Control -> ScoreT.Typed Signal.Control
    -> Deriver a -> Deriver a
withMergedControl merger control signal =
    modifySignal control (\mbSig -> merge merger mbSig signal)

-- | This is not just 'with_control', because I have to merge a control signal
-- into a possible ControlFunction.
modifySignal :: ScoreT.Control
    -> (Maybe ScoreT.TypedSignal -> ScoreT.TypedSignal) -> Deriver a
    -> Deriver a
modifySignal (ScoreT.Control control) modify = Internal.localm $ \state -> do
    val <- requireRight ((control <> ": ")<>) $
        Env.modifySignal modify control (stateEnviron state)
    pure $! insertEnv control val state

-- | Like 'with_controls', but merge them with their respective default
-- 'Merger's.
withMergedControls :: [(ScoreT.Control, ScoreT.Typed Signal.Control)]
    -> Deriver a -> Deriver a
withMergedControls controlVals deriver
    | null controlVals = deriver
    | otherwise = do
        let (controls, newSigs) = unzip controlVals
        mergers <- mapM getDefaultMerger controls
        oldSigs <- mapM lookupSignal controls
        let merged = zipWith3 merge mergers oldSigs newSigs
        withControls (zip controls merged) deriver

resolveMerge :: Merge -> ScoreT.Control -> Deriver Merger
resolveMerge DefaultMerge control = getDefaultMerger control
resolveMerge (Merge merger) _ = pure merger

getControlMerge :: Expr.Symbol -> Deriver Merger
getControlMerge name = do
    mergers <- gets (stateMergers . stateConstant)
    require ("unknown control merger: " <> ShowVal.showVal name)
        (Map.lookup name mergers)

-- | Get the default merger for this control, or 'merge_mul' if there is none.
getDefaultMerger :: ScoreT.Control -> Deriver Merger
getDefaultMerger control = do
    defaults <- Internal.getDynamic stateControlMergeDefaults
    pure $ Map.findWithDefault defaultMerge control defaults
    where
    defaultMerge = mergeMul

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
merge (Merger _ merger ident) maybeOld new =
    ScoreT.Typed (ScoreT.typeOf old <> ScoreT.typeOf new)
        (merger (ScoreT.valOf old) (ScoreT.valOf new))
    where old = fromMaybe (ScoreT.untyped (Signal.constant ident)) maybeOld
    -- Using ident is *not* the same as just emitting the 'new' signal for
    -- subtraction!

-- *** ControlMod

-- | Emit a 'ControlMod'.
modifyControl :: Merger -> ScoreT.Control -> Signal.Control -> Deriver ()
modifyControl merger control signal = Internal.modifyCollect $ \collect ->
    collect { collectControlMods =
        ControlMod control signal merger : collectControlMods collect }

-- | Modify all VSignal and VPSignal types in environ.
modifySignals :: (Signal.Control -> Signal.Control)
    -> (PSignal.PSignal -> PSignal.PSignal) -> Deriver a -> Deriver a
modifySignals modifyControl modifyPitch = Internal.local $ \state -> state
    { stateEnviron = Env.map update (stateEnviron state)
    , statePitch = modifyPitch (statePitch state)
    }
    where
    update = \case
        DeriveT.VSignal sig -> DeriveT.VSignal (modifyControl <$> sig)
        DeriveT.VPSignal sig -> DeriveT.VPSignal (modifyPitch sig)
        DeriveT.VCFunction cf -> DeriveT.VCFunction $
            cf { DeriveT.cfSignal = modifyControl <$> DeriveT.cfSignal cf }
        val -> val

-- | Apply the collected control mods to the given deriver and clear them out.
evalControlMods :: RealTime -- ^ Trim controls to end at this time.
    -- If a ControlMod is local to a slice it should end when the slice ends,
    -- and since it bypasses 'Derive.Control.trim_signal', I have to trim
    -- it explicitly.
    -> Deriver a -> Deriver a
evalControlMods end deriver = do
    mods <- gets (collectControlMods . stateCollect)
    if null mods then deriver else do
        -- TODO Wait, is this really legit?
        Internal.modifyCollect $ \collect ->
            collect { collectControlMods = [] }
        withControlMods mods end deriver

withControlMods :: [ControlMod] -> RealTime -> Deriver a -> Deriver a
withControlMods mods end deriver = foldr ($) deriver (map apply mods)
    where
    apply (ControlMod control signal merger) =
        withMergedControl merger control $ ScoreT.untyped $
            Signal.clipAfter end signal
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
pitchAt :: RealTime -> Deriver (Maybe PSignal.Pitch)
pitchAt pos = (`PSignal.at` pos) <$> getPitch

-- | Like 'pitch_at', this is a raw pitch.
namedPitchAt :: ScoreT.PControl -> RealTime -> Deriver (Maybe PSignal.Pitch)
namedPitchAt name pos = (`PSignal.at` pos) <$> getPitchSignal name

-- | Resolve the raw pitch returned from 'pitch_at' to the final transposed
-- pitch.
resolvePitch :: RealTime -> PSignal.Pitch -> Deriver PSignal.Transposed
resolvePitch pos pitch = do
    controls <- controlsAt pos
    pure $ PSignal.apply controls pitch

-- | Unlike 'pitch_at', the transposition has already been applied, because you
-- can't transpose any further once you have a NoteNumber.
nnAt :: RealTime -> Deriver (Maybe Pitch.NoteNumber)
nnAt pos = justm (pitchAt pos) $ \pitch ->
    loggedPitchNn ("nn " <> pretty pos) =<< resolvePitch pos pitch

getPitch :: Deriver PSignal.PSignal
getPitch = Internal.getDynamic statePitch

-- | Re-export 'Typecheck.lookup_pitch_signal', defined there to avoid
-- circular import.
lookupPitchSignal :: ScoreT.PControl -> Deriver (Maybe PSignal.PSignal)
lookupPitchSignal = Typecheck.lookupPitchSignal

getPitchSignal :: ScoreT.PControl -> Deriver PSignal.PSignal
getPitchSignal pcontrol =
    -- The PControl itself doesn't add the # because that's the ref syntax,
    -- but let's add the # to remind that it's a PControl.
    require ("no named pitch #" <> ShowVal.showVal pcontrol)
        =<< lookupPitchSignal pcontrol

namedNnAt :: ScoreT.PControl -> RealTime -> Deriver (Maybe Pitch.NoteNumber)
namedNnAt name pos = do
    controls <- controlsAt pos
    justm (namedPitchAt name pos) $ \pitch ->
        loggedPitchNn ("named_nn " <> pretty (name, pos)) $
            PSignal.apply controls pitch

-- | Version of 'PSignal.pitch_nn' that logs errors.
loggedPitchNn :: Text -> PSignal.Transposed
    -> Deriver (Maybe Pitch.NoteNumber)
loggedPitchNn msg pitch = case PSignal.pitchNn pitch of
    Left err -> do
        Log.warn $ "pitch_nn " <> msg <> ": " <> pretty err
        pure Nothing
    Right nn -> pure $ Just nn

-- *** with signal

withPitch :: PSignal.PSignal -> Deriver a -> Deriver a
withPitch = modifyPitch ScoreT.defaultPitch . const

-- TODO now that named pitch is just env VPSignals, do I really need this?
withNamedPitch :: ScoreT.PControl -> PSignal.PSignal -> Deriver a -> Deriver a
withNamedPitch pcontrol = modifyPitch pcontrol . const

withConstantPitch :: PSignal.Pitch -> Deriver a -> Deriver a
withConstantPitch = withPitch . PSignal.constant

removePitch :: Deriver a -> Deriver a
removePitch = modifyPitch ScoreT.defaultPitch (const mempty)

modifyPitch :: ScoreT.PControl -> (Maybe PSignal.PSignal -> PSignal.PSignal)
    -> Deriver a -> Deriver a
modifyPitch pcontrol modify deriver
    | pcontrol == ScoreT.defaultPitch = Internal.local
        (\state -> state { statePitch = modify (Just (statePitch state)) })
        deriver
    | otherwise = do
        mbSig <- lookupVal (ScoreT.pcontrolName pcontrol)
        withValRaw (ScoreT.pcontrolName pcontrol) (modify mbSig) deriver

-- * run monad

-- | Embed a LogId into Deriver.  This is for computations that need logging
-- but not a full Deriver.
runLogs :: Log.LogId a -> Deriver a
runLogs action = do
    let (val, logs) = Log.runId action
    mapM_ Log.write logs
    pure val

runTry :: (State -> State) -> Deriver a -> Deriver (Either Error a, State)
runTry withState deriver = do
    state <- get
    let (val, state2, logs) = run (withState state) deriver
    mapM_ Log.write logs
    pure (val, state2)

-- * 'Mode'

getMode :: Deriver Mode
getMode = gets (stateMode . stateDynamic)

isLilypondMode :: Deriver Bool
isLilypondMode = Maybe.isJust <$> lookupLilypondConfig

lookupLilypondConfig :: Deriver (Maybe Lilypond.Types.Config)
lookupLilypondConfig = getMode >>= \mode -> pure $ case mode of
    Lilypond config -> Just config
    _ -> Nothing

-- | Get the 'CallDuration' of the given deriver.
getScoreDuration :: Deriver a
    -> Deriver (Either Error (CallDuration ScoreTime))
getScoreDuration deriver = do
    (val, out) <- runTry setMode deriver
    pure $ case val of
        Left err -> Left err
        Right _ -> Right $ collectScoreDuration $ stateCollect out
    where
    setMode state = state
        { stateCollect = mempty
        , stateDynamic = (stateDynamic state)
            { stateMode = ScoreDurationQuery }
        }

getRealDuration :: Deriver a -> Deriver (Either Error (CallDuration RealTime))
getRealDuration deriver = do
    (val, out) <- runTry setMode deriver
    pure $ case val of
        Left err -> Left err
        Right _ -> Right $ collectRealDuration $ stateCollect out
    where
    setMode state = state
        { stateCollect = mempty
        , stateDynamic = (stateDynamic state)
            { stateMode = RealDurationQuery }
        }

-- * postproc

-- | Replace the 'state_stack' with the one from the event.  This is useful
-- for transformers, so they can show a stack trace to the event they are
-- processing.
withEventStack :: Score.Event -> Deriver a -> Deriver a
withEventStack event =
    maybe id withStack (Stack.blockTrackRegionOf (Score.eventStack event))
    where
    withStack (blockId, trackId, (s, e)) = Internal.withStackBlock blockId
        . Internal.withStackTrack trackId . Internal.withStackRegion s e

-- | A combination of 'catch' and 'with_event_stack'.
withEvent :: Score.Event -> Deriver a -> Deriver (Maybe a)
withEvent event = catch False . withEventStack event

-- | Shift the controls of a deriver.  You're supposed to apply the warp
-- before deriving the controls, but I don't have a good solution for how to
-- do this yet, so I can leave these here for the moment.
shiftControls :: ScoreTime -> Deriver a -> Deriver a
shiftControls shift deriver = do
    real <- Internal.real shift
    modifySignals (Signal.shift real) (PSignal.shift real) deriver

-- * call

-- | Wrap 'make_val_call' with a 'Typecheck.to_val' to automatically convert
-- to a 'DeriveT.Val'.  This is not in "Derive.Deriver.Monad" to avoid
-- a circular import with "Derive.DeriveT".
valCall :: Typecheck.ToVal a => Module.Module -> CallName -> Tags.Tags
    -> Doc.Doc -> WithArgDoc (PassedArgs Tagged -> Deriver a) -> ValCall
valCall module_ name tags doc (call, argDocs) =
    makeValCall module_ name tags doc (fmap Typecheck.toVal . call, argDocs)

setModule :: Module.Module -> Call f -> Call f
setModule module_ call = call
    { callDoc = (callDoc call) { cdocModule = module_ } }
