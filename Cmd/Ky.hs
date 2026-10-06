-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Load ky files, which are separate files containing call definitions.
-- The syntax is defined by 'Ky.parse_ky'.
module Cmd.Ky (
    update
    , check
    , load
    , compile
    , compileDefinitions
#ifdef TESTING
    , module Cmd.Ky
#endif
) where
import qualified Control.Monad.Except as Except
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Util.ParseText as ParseText

import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.Macro as Macro
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Eval as Eval
import qualified Derive.Expr as Expr
import qualified Derive.Library as Library
import qualified Derive.Parse.Ky as Ky
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


-- | Check if ky files have changed, and if they have, update
-- 'Cmd.state_ky_cache' and clear the performances.
update :: Ui.State -> Cmd.State -> Maybe Ky.Code
    -> IO (Maybe (Ui.State, Cmd.State, [Log.Msg]))
update uiState cmdState mbKyText =
    justm (checkCache lookupBackend cache allocs mbKyPath paths mbKyText) $
    \((kyCache, allocs), logs) -> pure $ Just
        ( Ui.config#UiConfig.allocations #= allocs $ uiState
        , cmdState
            { Cmd.stateKyCache = Just kyCache
            , Cmd.statePlay = (Cmd.statePlay cmdState)
                -- TODO should I kill threads?
                { Cmd.statePerformance = mempty
                , Cmd.stateCurrentPerformance = mempty
                , Cmd.statePerformanceThreads = mempty
                }
            }
        , logs
        )
    where
    lookupBackend = fmap Inst.instBackend . Cmd.getLookupInst cmdState
    allocs = Ui.config#UiConfig.allocations #$ uiState
    cache = Cmd.stateKyCache cmdState
    (mbKyPath, paths) = stateKyPaths cmdState

-- | Trigger a ky reload, but first check for an error and return it
-- synchronously rather than waiting for play.
check :: Cmd.CmdT IO Cmd.Error
check = do
    cmdState <- Cmd.get
    uiState <- Ui.get
    liftIO (update uiState cmdState Nothing) >>= \case
        Nothing -> pure ""
        -- Logs are just boring stuff about reloaded files.
        Just (uiState, cmdState, _logs) ->
            case Cmd.stateKyCache cmdState of
                Just (Cmd.KyCache (Left err) _) -> pure err
                _ -> do
                    Cmd.put cmdState
                    -- TODO This has allocs since they used to be saved with
                    -- the main Ui.State, but now they should move to
                    -- Cmd.State.
                    Ui.unsafePut uiState
                    pure ""

-- | Reload the ky files if they're out of date, Nothing if no reload is
-- needed.
checkCache :: (InstT.Qualified -> Maybe Inst.Backend) -> Maybe Cmd.KyCache
    -> UiConfig.Allocations -> Maybe FilePath -> [FilePath] -> Maybe Ky.Code
    -> IO (Maybe ((Cmd.KyCache, UiConfig.Allocations), [Log.Msg]))
checkCache lookupBackend prevCache oldAllocs mbKyPath paths mbKyText = run $ do
    when isPermanent abort
    -- if $base.ky exists, kyText becomes 'import $base.ky'. mbKyText Nothing
    -- means just check the file.  Just is the old internal ky path, for old
    -- scores.
    -- TODO Can I directly invoke the import machinery rather than constructing
    -- a fake import source?
    kyText <- case mbKyPath of
        Nothing -> maybe abort pure mbKyText
        Just kyPath -> ifM (liftIO $ Directory.doesFileExist kyPath)
            (pure $ "import '" <> txt kyPath <> "'")
            (maybe abort pure mbKyText)
    Ky.Ky defs imported allocs <- try . first ParseText.showError
        =<< liftIO (Ky.loadKy paths kyText)
    -- This uses the contents of all the files for the fingerprint, which
    -- means it has to read and parse them on each respond cycle.  If this
    -- turns out to be too expensive, I can go back to the modification time
    -- like I had before.
    let fingerprint = Cmd.fingerprint imported
    when (oldFingerprint == fingerprint) abort
    let (builtins, logs) = compileLibrary (loadedFnames imported) $
            compileDefinitions defs
    let errs = UiConfig.verifyAllocations lookupBackend allocs
    unless (null errs) $
        Except.throwError $ Just $ "instrument: " <> Text.intercalate "; " errs
    pure
        ( (builtins, Map.fromList (Ky.defAliases defs), fingerprint, allocs)
        , logs
        )
    where
    run = fmap apply . Except.runExceptT
    apply (Left Nothing) = Nothing
    apply (Left (Just err))
        | failedPreviously err = Nothing
        | otherwise = Just
            ( (Cmd.KyCache (Left err) mempty, oldAllocs)
            , [Log.msg Log.Warn Nothing $ "broken ky: " <> err]
            )
    apply (Right ((builtins, aliases, fingerprint, allocs), logs)) = Just
        ((Cmd.KyCache (Right (builtins, aliases)) fingerprint, allocs), logs)
    try = tryRight . first Just
    abort = Except.throwError Nothing
    oldFingerprint = case prevCache of
        Just (Cmd.KyCache _ fprint) -> fprint
        _ -> mempty
    isPermanent = case prevCache of
        Just (Cmd.PermanentKy {}) -> True
        _ -> False
    -- If it failed last time then don't replace the error.  Otherwise, I'll
    -- continually clear the performance and get an endless loop.
    failedPreviously err = case prevCache of
        Just (Cmd.KyCache (Left oldErr) _) -> err == oldErr
        _ -> False

-- | Like 'check_cache', but assuming no existing cmd or ui state.
load :: [FilePath] -> Text
    -> IO (Either Text
        (Derive.Builtins, Derive.InstrumentAliases, UiConfig.Allocations))
load paths kyText =
    bimap ParseText.showError compile <$> liftIO (Ky.loadKy paths kyText)

compile :: Ky.Ky Ky.Loaded
    -> (Derive.Builtins, Derive.InstrumentAliases, UiConfig.Allocations)
compile (Ky.Ky defs imported allocs) =
    (builtins, Map.fromList (Ky.defAliases defs), allocs)
    where
    -- Logs are boring, just loaded this or that.
    (builtins, _logs) = compileLibrary (loadedFnames imported) $
        compileDefinitions defs

loadedFnames :: [Ky.Loaded] -> [FilePath]
loadedFnames loads = [fname | Ky.Loaded fname _ <- loads]

stateKyPaths :: Cmd.State -> (Maybe FilePath, [FilePath])
stateKyPaths cmdState =
    ( Cmd.saveFileKyPath . snd <$> Cmd.stateSaveFile cmdState
    , maybe id (:) (Cmd.stateSaveDir cmdState)
        (Cmd.configKyPaths (Cmd.stateConfig cmdState))
    )

compileLibrary :: [FilePath] -> Library.Library -> (Derive.Builtins, [Log.Msg])
compileLibrary imports lib = Log.runId $ do
    Log.notice $
        "reloaded ky: [" <> Text.unwords (map showImport imports) <> "]"
    Library.compileLog lib

showImport "" = "<score>"
showImport fname = txt (FilePath.takeFileName fname)

compileDefinitions :: Ky.Definitions -> Library.Library
compileDefinitions (Ky.Definitions (gnote, tnote) (gcontrol, tcontrol)
        (gpitch, tpitch) val _aliases) =
    Derive.Scopes
        { scopesGenerator = Derive.Scope
            { scopeNote = map (compile makeGenerator) gnote
            , scopeControl = map (compile makeGenerator) gcontrol
            , scopePitch = map (compile makeGenerator) gpitch
            }
        , scopesTransformer = Derive.Scope
            { scopeNote = map (compile makeTransformer) tnote
            , scopeControl = map (compile makeTransformer) tcontrol
            , scopePitch = map (compile makeTransformer) tpitch
            }
        , scopesTrack = mempty
        , scopesVal = map (compile makeValCall) val
        }
    where
    compile make (fname, (sym, expr)) =
        Library.Single sym (make fname (symToName sym) expr)
    symToName (Expr.Symbol name) = Derive.CallName name

makeGenerator :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> Ky.Expr -> Derive.Generator d
makeGenerator fname name varExpr
    | Just expr <- noFreeVars varExpr = simpleGenerator fname name expr
    | otherwise = Macro.generator Module.local name mempty
        (Doc.Doc $ "Defined in " <> txt fname <> ".") varExpr

makeTransformer :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> Ky.Expr -> Derive.Transformer d
makeTransformer fname name varExpr
    | Just expr <- noFreeVars varExpr = simpleTransformer fname name expr
    | otherwise = Macro.transformer Module.local name mempty
        (Doc.Doc $ "Defined in " <> txt fname <> ".") varExpr

makeValCall :: FilePath -> Derive.CallName -> Ky.Expr -> Derive.ValCall
makeValCall fname name varExpr
    | Just expr <- noFreeVars varExpr = case expr of
        callExpr :| [] -> simpleValCall fname name callExpr
        _ -> broken
    | otherwise = case varExpr of
        Ky.Expr (callExpr :| []) -> Macro.valCall Module.local name mempty
            (Doc.Doc $ "Defined in " <> txt fname <> ".") callExpr
        _ -> broken
    where
    broken = brokenValCall name $
        "Broken val call defined in " <> txt fname
        <> ": val calls don't support pipeline syntax: "
        <> ShowVal.showVal varExpr

simpleGenerator :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> DeriveT.Expr -> Derive.Generator d
simpleGenerator fname name expr =
    Derive.generator Module.local name mempty (makeDoc fname name expr) $
    case assignSymbol expr of
        Nothing -> Sig.call0 generator
        Just sym ->
            Sig.call (Sig.manyVals "arg" "Args parsed by reapplied call.") $
                \_vals args -> Eval.reapplyGenerator args sym
    where generator args = Eval.evalToplevel (Derive.passedCtx args) expr

simpleTransformer :: Derive.CallableExpr d => FilePath -> Derive.CallName
    -> DeriveT.Expr -> Derive.Transformer d
simpleTransformer fname name expr =
    Derive.transformer Module.local name mempty (makeDoc fname name expr) $
    case assignSymbol expr of
        Nothing -> Sig.call0t transformer
        Just sym ->
            Sig.callt (Sig.manyVals "arg" "Args parsed by reapplied call.") $
                \_vals -> reapply sym
    where
    transformer args deriver =
        Eval.evalTransformers (Derive.passedCtx args)
            (NonEmpty.toList expr) deriver
    reapply sym args deriver = do
        call <- Eval.getTransformer sym
        Eval.applyTransformer (Derive.passedCtx args) call
            (Derive.passedVals args) deriver

simpleValCall :: FilePath -> Derive.CallName -> DeriveT.Call
    -> Derive.ValCall
simpleValCall fname name callExpr =
    Derive.valCall Module.local name mempty (makeDoc fname name expr) $
    case assignSymbol expr of
        Nothing -> Sig.call0 $ \args ->
            Eval.eval (Derive.passedCtx args) (Expr.ValCall callExpr)
        Just sym ->
            Sig.call (Sig.manyVals "arg" "Args parsed by reapplied call.") $
                \_vals -> callArgs sym
    where
    expr = callExpr :| []
    callArgs sym args = do
        call <- Eval.getValCall sym
        Derive.callFunc call $ args
            { Derive.passedCallName = Derive.callName call }

brokenValCall :: Derive.CallName -> Text -> Derive.ValCall
brokenValCall name msg = Derive.makeValCall Module.local name mempty
    (Doc.Doc msg)
    (Sig.call (Sig.manyVals "arg" "broken") $ \_ _ -> Derive.throw msg)

-- | If the Ky.Expr has no 'Ky.VarTerm's, it doesn't need to be a macro.
noFreeVars :: Ky.Expr -> Maybe DeriveT.Expr
noFreeVars (Ky.Expr expr) = traverse conventCall expr
    where
    conventCall (Ky.Call sym terms) =
        Expr.Call sym <$> traverse convertTerm terms
    convertTerm (Ky.VarTerm _) = Nothing
    convertTerm (Ky.ValCall call) = Expr.ValCall <$> conventCall call
    convertTerm (Ky.Literal val) = Just $ Expr.Literal val

makeDoc :: FilePath -> Derive.CallName -> DeriveT.Expr -> Doc.Doc
makeDoc fname name expr = Doc.Doc $
    pretty name <> " defined in " <> txt fname <> ": " <> ShowVal.showVal expr

-- | If there are arguments in the definition, then don't accept any in the
-- score.  I could do partial application, but it seems confusing, so
-- I won't add it unless I need it.
assignSymbol :: Expr.Expr a -> Maybe Expr.Symbol
assignSymbol (Expr.Call sym [] :| []) = Just sym
assignSymbol _ = Nothing
