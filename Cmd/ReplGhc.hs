-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, ScopedTypeVariables #-}
-- | REPL implementation that directly uses the GHC API.
--
-- Supported versions: 70, 74
module Cmd.ReplGhc (
    Session(..), make_session
    , interpreter, interpret
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.IORef as IORef
import qualified Data.List as List
-- GHC imports
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths

-- The liftIO here is not the same one in Control.Monad.Trans!
-- GHC defines its own MonadIO.
import MonadUtils (MonadIO, liftIO)
import qualified Outputable
import System.FilePath ((</>))

import Util.Control hiding (liftIO)
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Cmd.Cmd as Cmd


-- | The actual session runs in another thread, so this is the communication
-- channel.  @(expr, namespace, response_mvar)@
newtype Session = Session (Chan.Chan (String, MVar.MVar Cmd))
type Cmd = Cmd.CmdL String

-- | Text version of the Cmd type.
cmd_type :: String
cmd_type = "Cmd.CmdL String"

type Ghc a = GHC.GhcT IO a

make_session :: IO Session
make_session = Session <$> Chan.newChan

interpret :: Session -> [String] -> String -> IO (Cmd.CmdT IO String)
interpret (Session chan) _local_modules expr = do
    mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (expr, mvar)
    MVar.takeMVar mvar

ghci_flags :: FilePath
ghci_flags = BUILD_DIR </> "ghci-flags"

interpreter :: Session -> IO ()
interpreter (Session chan) = do
    GHC.parseStaticFlags [] -- not sure if this is necessary
    flags <- Exception.try (readFile ghci_flags)
    -- Ghc moved .o to dyn flags, but I'll have to wait for 7.8, or make a .a
    -- and use -l.
    let is_obj fn = BUILD_DIR `List.isPrefixOf` fn && ".o" `List.isSuffixOf` fn
    args <- filter (not . is_obj) <$> case flags of
        Left (exc :: Exception.SomeException) -> do
            Log.error $ "error reading ghci flags from "
                ++ show ghci_flags ++ ": " ++ show exc
                ++ ", the REPL is probably not going to work"
            return []
        Right flags -> return $ words flags

    GHC.runGhcT (Just GHC.Paths.libdir) $ do
        parse_flags args
        -- obj_allowed must be False, otherwise I get
        -- Cannot add module Cmd.Repl.Environ to context: not interpreted
        GHC.setTargets [make_target False toplevel]
        (result, logs, warns) <- reload
        case result of
            Left err -> liftIO $
                Log.warn $ "error loading REPL modules: " ++ err
                    ++ Seq.join "; " warns ++ " / " ++ Seq.join "; " logs
            _ -> return ()
        forever $ do
            (expr, return_mvar) <- liftIO $ Chan.readChan chan
            result <- case expr of
                ':' : colon -> colon_cmd colon
                _ -> normal_cmd expr
                    `GHC.gcatch` \(exc :: Exception.SomeException) ->
                        -- set_context throws if the reload failed.
                        return $ return $ "Exception: " ++ show exc
            liftIO $ MVar.putMVar return_mvar result
    where
    toplevel = "Cmd.Repl.Environ"

    normal_cmd :: String -> Ghc Cmd
    normal_cmd expr = do
        set_context [toplevel]
        format_response <$> compile expr

    colon_cmd :: String -> Ghc Cmd
    colon_cmd "r" = format_response <$> reload
    colon_cmd "R" = format_response <$> reload
    -- colon_cmd ('b' : mods) = return <$> (browse toplevel (words mods))
    colon_cmd colon = return $ return $ "Unknown colon command: " ++ show colon

-- | Convert warnings and a possibly failed compile into a chatty cmd.
format_response :: (Either String Cmd, [String], [String]) -> Cmd
format_response (result, logs, warns) = decorate $ case result of
        Left err -> Cmd.throw $ "compile error: " ++ err
        Right cmd -> cmd
    where
    decorate = fmap (\s -> log_msg ++ s ++ warn_msg)
    warn_msg =
        if null warns then "" else "\nWarnings:\n" ++ Seq.join "\n" warns
    log_msg =
        if null logs then "" else "Logs:\n" ++ Seq.join "\n" logs ++ "\n"


-- * implementation

-- | (Either error result, logs, warns)
type Result a = (Either String a, [String], [String])

-- | Load or reload the target modules.  Return errors if the load failed.
reload :: Ghc (Result Cmd)
reload = do
    (result, logs, warns) <- handle_errors $ GHC.load GHC.LoadAllTargets
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

compile :: String -> Ghc (Result Cmd)
compile expr = do
    (hval, logs, warns) <- handle_errors $ GHC.compileExpr typed_expr
    return (fmap coerce hval, logs, warns)
    where
    typed_expr = "fmap show (" ++ expr ++ ") :: " ++ cmd_type
    coerce val = GHC.Exts.unsafeCoerce# val :: Cmd

set_context :: [String] -> Ghc ()
set_context mod_names = do
    let prelude = GHC.simpleImportDecl (GHC.mkModuleName "Prelude")
    GHC.setContext $ GHC.IIDecl prelude
        : map (GHC.IIModule . GHC.mkModuleName) mod_names

-- | Run a Ghc action and collect logs and warns.
handle_errors :: Ghc a -> Ghc (Result a)
handle_errors action = do
    logs <- liftIO $ IORef.newIORef []
    val <- fmap Right (catch_logs logs >> action) `GHC.gcatch`
        \(exc :: Exception.SomeException) -> return (Left (show exc))
    warns <- get_warnings
    logs <- liftIO $ IORef.readIORef logs
    return (val, reverse logs, warns)
    where
    catch_logs logs = modify_flags $ \flags ->
        flags { GHC.log_action = log_action logs }
    -- log_action :: GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> PprStyle
    --     -> MsgDoc -> IO ()
    log_action logs dflags _severity _span style msg =
        liftIO $ IORef.modifyIORef logs (err:)
        where
        err = Outputable.showSDoc dflags $ Outputable.withPprStyle style msg
            -- ErrUtils.mkLocMessage span msg

modify_flags :: (GHC.DynFlags -> GHC.DynFlags) -> Ghc ()
modify_flags f = do
    dflags <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $! f dflags

get_warnings :: Ghc [String]
-- GHC.getWarnings is gone, apparently replaced by either printing directly or
-- throwing an exception, e.g. compiler/main/HscTypes.lhs:handleFlagWarnings.
get_warnings = return []

parse_flags :: [String] -> Ghc ()
parse_flags args = do
    dflags <- GHC.getSessionDynFlags
    (dflags, args_left, warns) <- GHC.parseDynamicFlags dflags
        (map (GHC.mkGeneralLocated "cmdline") args)
    unless (null warns) $
        liftIO $ Log.warn $ "warnings parsing flags " ++ show args ++ ": "
            ++ show (map GHC.unLoc warns)
    unless (null args_left) $
        liftIO $ Log.warn $
            "ignoring unparsed args: " ++ show (map GHC.unLoc args_left)
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
