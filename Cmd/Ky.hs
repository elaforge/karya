-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Load ky files, which are separate files containing call definitions.
-- The syntax is defined by 'Ky.parse_ky'.
module Cmd.Ky (
    reload
    , load
    , compile
    , compileDefinitions
#ifdef TESTING
    , module Cmd.Ky
#endif
) where
import qualified Data.Hashable as Hashable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified System.Directory as Directory

import qualified Util.Doc as Doc
import qualified Util.Log as Log
import qualified Util.ParseM as ParseM

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
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
import qualified Derive.TScore.T as TScore.T
import qualified Derive.TScore.TScore as TScore

import qualified Instrument.Inst as Inst
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


type Error = Text

-- | Trigger a ky reload, update 'Cmd.stateKyCache', clear the performances.
--
-- Throw if there's an error parsing or interpreting.
--
-- If integrate is False, record the tscore fingerprint but don't integrate.
-- This is for loading a score, where the tscore should already be integrated,
-- and only needs to be integrated again when it changes.
reload :: Bool -> Maybe Ky.Code -> Cmd.CmdT IO ()
reload integrate mbKyCode = do
    cmdState <- Cmd.get
    uiState <- Ui.get
    -- Nothing happens when PermanentKy or no file.
    errKy <- Cmd.abortUnless =<< liftIO (checkLoadKy cmdState mbKyCode)
    -- Omit the extra throwing line number, this is a reporting the ky parse
    -- error, which already has provenance.
    -- TODO maybe should be an expected vs unexpected distinction?
    ky <- either Cmd.throw_ pure errKy
    (uiState, cmdState, logs, mbTscore) <-
        Cmd.requireRight id $ checkKy uiState cmdState ky
    mapM_ Log.write logs
    Cmd.put cmdState
    -- TODO This has allocs since they used to be saved with the main Ui.State,
    -- but now they should move to Cmd.State.
    Ui.unsafePut uiState
    when integrate $ whenJust mbTscore integrateTscore

integrateTscore :: Cmd.M m => Ky.TScore -> m ()
integrateTscore (fname, row, code, score) = do
    cmdState <- Cmd.get
    TScore.cmdIntegrate cmdState score >>= \case
        Right newBlocks -> mapM_ Create.view newBlocks
        Left errs ->
            Cmd.throw_ $ Text.intercalate "\n" $
                map (ParseM.showError . ParseM.addRow row) errors
            where errors = map (TScore.T.convertError fname code) errs

checkLoadKy :: Cmd.State -> Maybe Ky.Code
    -> IO (Maybe (Either Error (Ky.Ky Ky.Loaded)))
checkLoadKy cmdState mbKyCode
    -- PermanentKy is used only in tests, so we shouldn't see it here.
    | Just (Cmd.PermanentKy {}) <- Cmd.stateKyCache cmdState = pure Nothing
    | otherwise = loadKy mbKyPath paths mbKyCode
    where (mbKyPath, paths) = stateKyPaths cmdState

checkKy :: Ui.State -> Cmd.State -> Ky.Ky Ky.Loaded
    -> Either Error (Ui.State, Cmd.State, [Log.Msg], Maybe Ky.TScore)
checkKy uiState cmdState ky = do
    let (kyBuiltins, logs) = compileLibrary (compileDefinitions defs)
    let errs = UiConfig.verifyAllocations lookupBackend allocs
    unless (null errs) $
        Left $ "instrument: " <> Text.intercalate "; " errs
    tscore <- case tscores of
        [tscore] -> pure tscore
        -- This happens if tscore section is removed.
        [] -> pure ("", 0, "", mempty)
        _ -> Left $ "imported tscores not supported yet: "
            <> Text.unwords [txt fname | (fname, _, _, _) <- tscores]
    let kyTscoreFingerprint = tscoreFingerprint tscores
    pure
        ( Ui.config#UiConfig.allocations #= allocs $ uiState
        , cmdState
            { Cmd.stateKyCache = Just $ Cmd.KyCache $ Cmd.KyCached
                { kyBuiltins
                , kyAliases = Map.fromList (Ky.defAliases defs)
                , kyTscoreFingerprint
                }
            , Cmd.statePlay = (Cmd.statePlay cmdState)
                -- TODO should I kill threads?
                { Cmd.statePerformance = mempty
                , Cmd.stateCurrentPerformance = mempty
                , Cmd.statePerformanceThreads = mempty
                }
            }
        , logs
        , if kyTscoreFingerprint == oldTscoreFingerprint
            then Nothing
            else Just tscore
        )
    where
    Ky.Ky defs _imported allocs tscores = ky
    lookupBackend = fmap Inst.instBackend . Cmd.getLookupInst cmdState
    oldTscoreFingerprint = case Cmd.stateKyCache cmdState of
        Just (Cmd.KyCache cache) -> Cmd.kyTscoreFingerprint cache
        _ -> mempty

-- TODO Cmd.Fingerprint has file names, but I think no good reason, I can just
-- put filenames in the hash.

tscoreFingerprint :: [Ky.TScore] -> Cmd.Fingerprint
tscoreFingerprint tscores =
    Cmd.Fingerprint fnames (foldl' Hashable.hashWithSalt 0 codes)
    where (fnames, _, codes, _) = List.unzip4 tscores

-- TODO rename to 'load'?
-- | If $base.ky exists, import it.  mbKyCode Nothing means just check the
-- file.  Just is the old internal ky path, for old scores.
loadKy :: Maybe FilePath -> [FilePath] -> Maybe Ky.Code
    -> IO (Maybe (Either Error (Ky.Ky Ky.Loaded)))
loadKy mbKyPath paths mbKyCode = do
    exists <- maybe (pure False) Directory.doesFileExist mbKyPath
    case mbKyPath of
        Just kyPath | exists -> fmt <$> Ky.loadKy paths kyPath
        _ -> case mbKyCode of
            Nothing -> pure Nothing
            Just code -> fmt <$> Ky.loadKyCode paths code
    where fmt = Just . first ParseM.showError

-- | Like 'reload', but assuming no existing cmd or ui state.
load :: [FilePath] -> Text
    -> IO (Either Text
        (Derive.Builtins, Derive.InstrumentAliases, UiConfig.Allocations))
load paths kyText =
    bimap ParseM.showError compile <$> Ky.loadKyCode paths kyText

compile :: Ky.Ky Ky.Loaded
    -> (Derive.Builtins, Derive.InstrumentAliases, UiConfig.Allocations)
compile (Ky.Ky defs _imported allocs _tscores) =
    (builtins, Map.fromList (Ky.defAliases defs), allocs)
    where
    -- Logs will be about name collisions.
    (builtins, _logs) = compileLibrary (compileDefinitions defs)

stateKyPaths :: Cmd.State -> (Maybe FilePath, [FilePath])
stateKyPaths cmdState =
    ( Cmd.saveFileKyPath . snd <$> Cmd.stateSaveFile cmdState
    , maybe id (:) (Cmd.stateSaveDir cmdState)
        (Cmd.configKyPaths (Cmd.stateConfig cmdState))
    )

compileLibrary :: Library.Library -> (Derive.Builtins, [Log.Msg])
compileLibrary lib = Log.runId $ Library.compileLog lib

compileDefinitions :: Ky.Definitions -> Library.Library
compileDefinitions (Ky.Definitions (gnote, tnote) (gcontrol, tcontrol)
        (gpitch, tpitch) val _aliases) =
    Derive.Scopes
        { scopesGenerator = Derive.Scope
            { scopeNote = map (compile1 makeGenerator) gnote
            , scopeControl = map (compile1 makeGenerator) gcontrol
            , scopePitch = map (compile1 makeGenerator) gpitch
            }
        , scopesTransformer = Derive.Scope
            { scopeNote = map (compile1 makeTransformer) tnote
            , scopeControl = map (compile1 makeTransformer) tcontrol
            , scopePitch = map (compile1 makeTransformer) tpitch
            }
        , scopesTrack = mempty
        , scopesVal = map (compile1 makeValCall) val
        }
    where
    compile1 make (fname, (sym, expr)) =
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
