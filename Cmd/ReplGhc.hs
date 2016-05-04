-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-} -- for BUILD_DIR
{-# LANGUAGE MagicHash, ScopedTypeVariables #-}
-- | REPL implementation that directly uses the GHC API.
--
-- Supported versions: 74, 78
module Cmd.ReplGhc (
    Session(..), make_session
    , interpreter, interpret
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Text as Text

-- GHC imports
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths

-- The liftIO here is not the same one in Control.Monad.Trans!
-- GHC defines its own MonadIO.
import MonadUtils (liftIO)
import qualified Outputable
import System.FilePath ((</>))

import qualified Util.File as File
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified App.ReplUtil as ReplUtil
import Global hiding (liftIO)


-- | The base directory of this build.  Comes from CPP.
build_dir :: FilePath
build_dir = BUILD_DIR

ghci_flags :: FilePath
ghci_flags = build_dir </> "ghci-flags"

-- | The actual session runs in another thread, so this is the communication
-- channel.  @(expr, namespace, response_mvar)@
newtype Session = Session (Chan.Chan (Text, MVar.MVar Cmd))
type Cmd = Cmd.CmdL ReplUtil.Response

-- | Text version of the Cmd type.
cmd_type :: String
cmd_type = "Cmd.CmdL String"

type Ghc a = GHC.GhcT IO a

make_session :: IO Session
make_session = Session <$> Chan.newChan

interpret :: Session -> [String] -> Text -> IO (Cmd.CmdT IO ReplUtil.Response)
interpret (Session chan) _local_modules expr = do
    mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (expr, mvar)
    MVar.takeMVar mvar

interpreter :: Session -> IO ()
interpreter (Session chan) = do
    GHC.parseStaticFlags [] -- not sure if this is necessary
    flags <- File.tryIO (readFile ghci_flags)
    args <- case flags of
        Left exc -> do
            Log.error $ "error reading ghci flags from "
                <> showt ghci_flags <> ": " <> showt exc
                <> ", the REPL is probably not going to work"
            return []
        Right flags -> return $ words flags

    GHC.runGhcT (Just GHC.Paths.libdir) $ do
        parse_flags args
        -- obj_allowed must be False, otherwise I get
        -- Cannot add module Cmd.Repl.Environ to context: not interpreted
        GHC.setTargets [make_target False toplevel_module]
        ((result, logs, warns), time) <-
            Log.format_time <$> Log.time_eval reload
        logs <- return $ filter
            (not . ("Cmd/Repl/Environ.hs, interpreted" `List.isInfixOf`)) logs
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
            Log.notice $ "loaded modules for repl: " <> time
            case result of
                Left err -> Log.warn $ "error loading REPL modules: " <> txt err
                _ -> return ()
        forever $ do
            (expr, return_mvar) <- liftIO $ Chan.readChan chan
            result <- case untxt expr of
                ':' : colon -> colon_cmd colon
                _ -> normal_cmd (untxt expr)
                    `GHC.gcatch` \(exc :: Exception.SomeException) ->
                        -- set_context throws if the reload failed.
                        return $ return $
                            ReplUtil.raw $ "Exception: " <> showt exc
            liftIO $ MVar.putMVar return_mvar result
    where
    toplevel_module = "Cmd.Repl.Environ"

    normal_cmd :: String -> Ghc Cmd
    normal_cmd expr = do
        set_context [toplevel_module]
        make_response <$> compile expr

    colon_cmd :: String -> Ghc Cmd
    colon_cmd "r" = make_response <$> reload
    colon_cmd "R" = make_response <$> reload
    colon_cmd colon = return $ return $
        ReplUtil.raw $ "Unknown colon command: " <> showt colon

make_response :: Result (Cmd.CmdL String) -> Cmd
make_response (result, logs, warns) = case result of
    Left err -> return (ReplUtil.Raw ("compile error: " <> txt err), all_logs)
    Right cmd -> do
        result <- cmd
        return (ReplUtil.Format (txt result), all_logs)
    where all_logs = map txt (logs ++ warns)

-- * implementation

-- | (Either error result, logs, warns)
type Result a = (Either String a, [String], [String])

-- | Load or reload the target modules.  Return errors if the load failed.
reload :: Ghc (Result (Cmd.CmdL String))
reload = do
    (result, logs, warns) <- collect_logs $ GHC.load GHC.LoadAllTargets
    return (mkcmd result, logs, warns)
    where
    mkcmd (Right ok)
        | GHC.succeeded ok = Right (return "reloaded")
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

compile :: String -> Ghc (Result (Cmd.CmdL String))
compile expr = do
    (hval, logs, warns) <- collect_logs $ GHC.compileExpr typed_expr
    return (fmap coerce hval, logs, warns)
    where
    typed_expr = "fmap show (" ++ expr ++ ") :: " ++ cmd_type
    coerce val = GHC.Exts.unsafeCoerce# val :: Cmd.CmdL String

set_context :: [String] -> Ghc ()
set_context mod_names = do
    let prelude = GHC.simpleImportDecl (GHC.mkModuleName "Prelude")
    GHC.setContext $ GHC.IIDecl prelude
        : map (GHC.IIModule . GHC.mkModuleName) mod_names

-- | Run a Ghc action and collect logs and warns.
collect_logs :: Ghc a -> Ghc (Result a)
collect_logs action = do
    logs <- liftIO $ IORef.newIORef []
    val <- fmap Right (catch_logs logs >> action) `GHC.gcatch`
        \(exc :: Exception.SomeException) -> return (Left (show exc))
    logs <- liftIO $ IORef.readIORef logs
    -- GHC.getWarnings is gone, apparently replaced by either printing directly
    -- or throwing an exception, e.g.
    -- compiler/main/HscTypes.lhs:handleFlagWarnings.
    let warns = []
    return (val, reverse logs, warns)
    where
    catch_logs logs = modify_flags $ \flags ->
        flags { GHC.log_action = log_action logs }
    -- log_action :: GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> PprStyle
    --     -> MsgDoc -> IO ()
    log_action logs dflags _severity _span style msg =
        liftIO $ IORef.modifyIORef logs (formatted:)
        where
        formatted = Outputable.showSDoc dflags $
            Outputable.withPprStyle style msg

modify_flags :: (GHC.DynFlags -> GHC.DynFlags) -> Ghc ()
modify_flags f = do
    dflags <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $! f dflags

parse_flags :: [String] -> Ghc ()
parse_flags args = do
    dflags <- GHC.getSessionDynFlags
    (dflags, args_left, warns) <- GHC.parseDynamicFlags dflags
        (map (GHC.mkGeneralLocated "cmdline") args)
    unless (null warns) $
        liftIO $ Log.warn $ "warnings parsing flags " <> showt args <> ": "
            <> showt (map GHC.unLoc warns)
    unless (null args_left) $
        liftIO $ Log.warn $
            "ignoring unparsed args: " <> showt (map GHC.unLoc args_left)
    void $ GHC.setSessionDynFlags $ dflags
        { GHC.ghcMode = GHC.CompManager
        , GHC.ghcLink = GHC.LinkInMemory
        , GHC.hscTarget = GHC.HscInterpreted
        , GHC.verbosity = 1
        }

make_target :: Bool -> String -> GHC.Target
make_target obj_allowed module_name = GHC.Target
    { GHC.targetId = GHC.TargetModule (GHC.mkModuleName module_name)
    -- ghci unsets this if the module name starts with *, so I guess it means
    -- you can load the .o or must interpret.
    , GHC.targetAllowObjCode = obj_allowed
    , GHC.targetContents = Nothing
    }
