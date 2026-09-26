-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-} -- for BUILD_DIR
{-# LANGUAGE MagicHash #-}
-- | REPL implementation that directly uses the GHC API.
--
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/api
-- https://wiki.haskell.org/GHC/As_a_library
module Cmd.ReplGhc (
    Session(..), makeSession
    , interpreter, interpret, complete
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Text as Text

import           System.FilePath ((</>))

-- GHC imports
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths

import qualified GHC.Types.Error as Error
import qualified GHC.Utils.Outputable as Outputable

#if GHC_VERSION >= 91201
import qualified GHC.Driver.Ppr as Ppr
import qualified GHC.Utils.Error as Error
#endif

#if GHC_VERSION >= 90201

import qualified Control.Monad.Catch as Catch
#if GHC_VERSION < 91201
import qualified GHC.Driver.CmdLine as CmdLine
#endif
import qualified GHC.Utils.Logger as Logger
import           Global (liftIO)

#elif GHC_VERSION >= 90001

#error ghc 9.1 not supported

#elif GHC_VERSION >= 80401

import           GHC (gcatch)
import qualified CmdLineParser as CmdLine
import qualified DynFlags
import           MonadUtils (liftIO) -- Old GHC defines its own liftIO!
import qualified Outputable

#else

import           GHC (gcatch)
import qualified DynFlags
import           MonadUtils (liftIO)
import qualified Outputable

#endif

import qualified Util.Exceptions as Exceptions
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Cmd.Cmd as Cmd
import qualified App.ReplProtocol as ReplProtocol
import           Global hiding (liftIO)


#if GHC_VERSION >= 90001
gcatch :: GHC.GhcT IO a -> (Catch.SomeException -> GHC.GhcT IO a)
    -> GHC.GhcT IO a
gcatch = Catch.catch
#endif

-- | The base directory of this build.  Comes from CPP.
buildDir :: FilePath
buildDir = BUILD_DIR

ghciFlags :: FilePath
ghciFlags = buildDir </> "ghci-flags"

-- | The actual session runs in another thread, so this is the communication
-- channel.  @(expr, namespace, response_mvar)@
newtype Session = Session (Chan.Chan (Query, MVar.MVar Response))

data Query = QCommand !Text | QCompletion !Text
    deriving (Show)
data Response = RCommand !Cmd | RCompletion ![Text]
type Cmd = Cmd.CmdT IO ReplProtocol.CmdResult

type Ghc a = GHC.GhcT IO a

makeSession :: IO Session
makeSession = Session <$> Chan.newChan

interpret :: Session -> Text -> IO Cmd
interpret (Session chan) expr = do
    mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (QCommand expr, mvar)
    response <- MVar.takeMVar mvar
    case response of
        RCommand cmd -> pure cmd
        _ -> errorIO "unexpected response to QCommand"

complete :: Session -> Text -> IO [Text]
complete (Session chan) prefix = do
    mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (QCompletion prefix, mvar)
    response <- MVar.takeMVar mvar
    case response of
        RCompletion words -> pure words
        _ -> errorIO "unexpected response to QCompletion"

-- | Initialize the GHC API and run the interpreter loop.
interpreter :: Session -> IO ()
interpreter (Session chan) = do
#if GHC_VERSION < 80200
    GHC.parseStaticFlags [] -- not sure if this is necessary
#endif
    flags <- Exceptions.tryIO (readFile ghciFlags)
    args <- case flags of
        Left exc -> do
            Log.error $ "error reading ghci flags from "
                <> showt ghciFlags <> ": " <> showt exc
                <> ", the REPL is probably not going to work"
            pure []
        Right flags -> pure $ lines flags

    GHC.runGhcT (Just GHC.Paths.libdir) $ do
        parseFlags args
        GHC.setTargets =<< mapM makeTarget toplevelModules
        ((result, logs, warns), timeMsg) <-
            Thread.timeActionText (reload toplevelModules)
        let expected = map ((++ ".hs, interpreted") . Lists.replace1 '.' "/")
                (toplevelModules ++ expectedReloads)
        logs <- pure $ filter
            (\log -> not $ (any (`List.isInfixOf` log) expected)) logs
        liftIO $ do
            unless (null logs) $ do
                Log.warn $ "unexpected logs from reload: "
                    <> Text.intercalate "; " (map txt logs)
                -- This module uses CPP, so I can't use backslash continuation.
                Log.warn $ mconcat
                    [ "Sometimes ghc thinks a module should be recompiled"
                    , " but the shakefile doesn't. I'm not sure what causes"
                    , " it, but it means all the modules have to be compiled."
                    , " Delete and rebuild to fix."
                    ]
            unless (null warns) $
                Log.warn $ "warnings from reload: "
                    <> Text.intercalate "; " (map txt warns)
            Log.notice $ "loaded modules for repl: " <> timeMsg
            case result of
                Left err -> Log.warn $ "error loading REPL modules: " <> txt err
                _ -> pure ()
        forever $ do
            (query, returnMvar) <- liftIO $ Chan.readChan chan
            result <- respond toplevelModules query
            liftIO $ MVar.putMVar returnMvar result
    where
    expectedReloads = ["User.Elaforge.Repl"] -- TODO should be User.*.Repl
    toplevelModules = ["Cmd.Repl.Environ", "Local.Repl"]

respond :: [String] -> Query -> GHC.GhcT IO Response
respond toplevelModules (QCommand expr) = RCommand <$> if
    -- Emitted via :r from the repl.
    | expr == ":reload" -> makeResponse <$> reload toplevelModules
    | otherwise -> (`gcatch` catch) $ makeResponse <$> compile (untxt expr)
    where
    catch (exc :: Exception.SomeException) =
        pure $ pure $ ReplProtocol.errorResult $ "Exception: " <> showt exc
respond _ (QCompletion prefix) = RCompletion <$>
    -- Don't bother with 50 zillion completions.
    if prefix == "" then pure [] else do
        rdrs <- GHC.getRdrNamesInScope
        dflags <- GHC.getSessionDynFlags
        pure $ filter (prefix `Text.isPrefixOf`) $
            Lists.uniqueSort $ map (txt . showPpr dflags) rdrs

showPpr :: Outputable.Outputable a => GHC.DynFlags -> a -> String
#if GHC_VERSION >= 90201
showPpr _ = Outputable.showPprUnsafe
#else
showPpr = Outputable.showPpr
#endif

makeResponse :: Result (Cmd.CmdL ReplProtocol.Result) -> Cmd
makeResponse (val, logs, warns) = case val of
    Left err -> pure $ ReplProtocol.CmdResult
        (ReplProtocol.Raw ("compile error: " <> txt err)) allLogs
    Right cmd -> do
        result <- cmd
        pure $ ReplProtocol.CmdResult result allLogs
    where
    allLogs = map (Log.msg Log.Notice Nothing . txt) logs
        ++ map (Log.msg Log.Warn Nothing . txt) warns

-- * implementation

-- | (Either error cmd, logs, warns)
type Result a = (Either String a, [String], [String])

-- | Load or reload the target modules.  Return errors if the load failed.
reload :: [String] -> Ghc (Result (Cmd.CmdL ReplProtocol.Result))
reload toplevelModules = do
    (result, logs, warns) <- collectLogs $
        GHC.load GHC.LoadAllTargets <* setContext toplevelModules
    pure (mkcmd result, logs, warns)
    where
    mkcmd (Right ok)
        | GHC.succeeded ok = Right (pure $ ReplProtocol.Raw "reloaded")
        | otherwise = Left "reload failed"
    mkcmd (Left err) = Left err
    -- Set the module loaded callback to print out modules as they are loaded.
    -- module_load_callback = HscTypes.withLocalCallbacks $ \cbs ->
    --     cbs { GHC.reportModuleCompilationResult = load_callback }
    -- load_callback mod_summary maybe_err = do
    --     liftIO $ putStrLn $ show_ppr (HscTypes.ms_mod mod_summary)
    --     GHC.defaultWarnErrLogger maybe_err
    -- show_ppr :: (Outputable.Outputable a) => a -> String
    -- show_ppr = Outputable.showSDoc . Outputable.ppr

compile :: String -> Ghc (Result (Cmd.CmdL ReplProtocol.Result))
compile expr = do
    (hval, logs, warns) <- collectLogs $ GHC.compileExpr typedExpr
    pure (fmap coerce hval, logs, warns)
    where
    -- The _toResult method is defined in "Cmd.Repl.Global".
    typedExpr =
        "fmap _toResult (" ++ expr ++ ") :: Cmd.CmdL ReplProtocol.Result"
    -- This should be safe because I just asserted the the type above.
    coerce val = GHC.Exts.unsafeCoerce# val :: Cmd.CmdL ReplProtocol.Result

setContext :: [String] -> Ghc ()
setContext modNames = do
    let prelude = GHC.simpleImportDecl (GHC.mkModuleName "Prelude")
    GHC.setContext $ GHC.IIDecl prelude
        : map (GHC.IIModule . GHC.mkModuleName) modNames

#if GHC_VERSION >= 90201

collectLogs :: Ghc a -> Ghc (Result a)
collectLogs action = do
    logs <- liftIO $ IORef.newIORef []
    GHC.pushLogHookM $ const (logAction logs)
    val <- fmap Right action
        `gcatch` \(exc :: Exception.SomeException) -> pure (Left (show exc))
    GHC.popLogHookM
    logs <- liftIO $ IORef.readIORef logs
    -- GHC.getWarnings is gone, apparently replaced by either printing directly
    -- or throwing an exception, e.g.
    -- compiler/main/HscTypes.lhs:handleFlagWarnings.
    let warns = []
    pure (val, reverse logs, warns)

logAction :: IORef.IORef [String] -> Logger.LogAction
#if GHC_VERSION >= 90601
-- type LogAction = LogFlags -> MessageClass -> SrcSpan -> SDoc -> IO ()
logAction logs _ _ _ msg =
#else
-- type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan
--               -> SDoc -> IO ()
logAction logs _ _ _ _ msg =
#endif
    liftIO $ IORef.modifyIORef logs (formatted:)
    where formatted = Outputable.showSDocUnsafe msg

#else

-- | Run a Ghc action and collect logs and warns.
collectLogs :: Ghc a -> Ghc (Result a)
collectLogs action = do
    logs <- liftIO $ IORef.newIORef []
    val <- fmap Right (catchLogs logs >> action) `gcatch`
        \(exc :: Exception.SomeException) -> pure (Left (show exc))
    logs <- liftIO $ IORef.readIORef logs
    -- GHC.getWarnings is gone, apparently replaced by either printing directly
    -- or throwing an exception, e.g.
    -- compiler/main/HscTypes.lhs:handleFlagWarnings.
    let warns = []
    pure (val, reverse logs, warns)
    where
    catchLogs logs = modifyFlags $ \flags ->
        flags { GHC.logAction = logAction logs }
    modifyFlags :: (GHC.DynFlags -> GHC.DynFlags) -> Ghc ()
    modifyFlags f = do
        dflags <- GHC.getSessionDynFlags
        void $ GHC.setSessionDynFlags $! f dflags

logAction :: IORef.IORef [String]
    -> GHC.DynFlags -> DynFlags.WarnReason -> GHC.Severity -> GHC.SrcSpan
    -> Outputable.PprStyle -> Outputable.SDoc -> IO ()
logAction logs dflags _warnReason _severity _span style msg =
    liftIO $ IORef.modifyIORef logs (formatted:)
    where
    formatted = Outputable.showSDoc dflags $
        Outputable.withPprStyle style msg

#endif

parseFlags :: [String] -> Ghc ()
parseFlags args = do
    dflags <- GHC.getSessionDynFlags

#if GHC_VERSION >= 90201
    logger <- GHC.getLogger
    (dflags, argsLeft, warns) <- GHC.parseDynamicFlags logger dflags
        (map (GHC.mkGeneralLocated "cmdline") args)
#else
    (dflags, argsLeft, warns) <- GHC.parseDynamicFlags dflags
        (map (GHC.mkGeneralLocated "cmdline") args)
#endif

#if GHC_VERSION >= 91201
    -- GHC.Types.Error.Messages GHC.Driver.Errors.Types.DriverMessage
    -- -> [String]
    let showWarns = map (Ppr.showSDoc dflags)
            . Error.pprMsgEnvelopeBagWithLocDefault . Error.getMessages
#elif GHC_VERSION >= 80401
    let showWarns = map (GHC.unLoc . CmdLine.warnMsg)
#else
    let showWarns = map GHC.unLoc
#endif

    unless (null warns) $
        liftIO $ Log.warn $ "warnings parsing flags " <> showt args <> ": "
            <> Text.unlines (map txt (showWarns warns))
    unless (null argsLeft) $
        liftIO $ Log.warn $
            "ignoring unparsed args: " <> showt (map GHC.unLoc argsLeft)
    void $ GHC.setSessionDynFlags $ dflags
        { GHC.ghcMode = GHC.CompManager
        , GHC.ghcLink = GHC.LinkInMemory
#if GHC_VERSION >= 90601
        , GHC.backend = GHC.interpreterBackend
#elif GHC_VERSION >= 90201
        , GHC.backend = GHC.Interpreter
#else
        , GHC.hscTarget = GHC.HscInterpreted
#endif
        , GHC.verbosity = 1
        }

makeTarget :: GHC.GhcMonad m => String -> m GHC.Target

#if GHC_VERSION >= 90601
-- guessTarget :: GhcMonad m => String -> Maybe UnitId -> Maybe Phase
-- -> m Target
makeTarget moduleName = do
    target <- GHC.guessTarget moduleName Nothing Nothing
    -- obj_allowed must be False, otherwise I get
    -- Cannot add module Cmd.Repl.Environ to context: not interpreted
    -- ghci unsets this if the module name starts with *, so I guess it means
    -- you can load the .o or must interpret.
    pure $ target { GHC.targetAllowObjCode = False }
#else

makeTarget moduleName = pure $ GHC.Target
    { GHC.targetId = GHC.TargetModule (GHC.mkModuleName moduleName)
    -- ghci unsets this if the module name starts with *, so I guess it means
    -- you can load the .o or must interpret.
    , GHC.targetAllowObjCode = False
    , GHC.targetContents = Nothing
    }

#endif
