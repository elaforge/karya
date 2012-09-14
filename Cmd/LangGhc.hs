{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | REPL implementation that directly uses the GHC API.
--
-- Supported versions: 70, 74
module Cmd.LangGhc (
    Session(..), make_session
    , interpreter, interpret
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad
import System.FilePath ((</>))

import qualified Data.IORef as IORef

-- GHC imports
#if __GLASGOW_HASKELL__ == 70
import qualified Bag
import qualified ErrUtils
#endif
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths
-- The liftIO here is not the same one in Control.Monad.Trans!
-- GHC defines its own MonadIO.
import MonadUtils (MonadIO, liftIO)
import qualified Outputable

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Derive.ParseBs as ParseBs


-- TODO A lot of symbols moved to the GHC API in 7.4.1, when I switch I can
-- remove the direct imports of internal modules.

-- | The actual session runs in another thread, so this is the communication
-- channel.  @(expr, namespace, response_mvar)@
newtype Session = Session (Chan.Chan (String, Id.Namespace, MVar.MVar Cmd))
type Cmd = Cmd.CmdL String

-- | Text version of the Cmd type.
cmd_type :: String
cmd_type = "Cmd.CmdL String"

type Ghc a = GHC.GhcT IO a

make_session :: IO Session
make_session = Session <$> Chan.newChan

interpret :: Session -> [String] -> State.State
    -> Cmd.State -> String -> IO (Cmd.CmdT IO String)
interpret (Session chan) _local_modules ui_state _cmd_state expr = do
    mvar <- MVar.newEmptyMVar
    let ns = State.config_namespace (State.state_config ui_state)
    Chan.writeChan chan (expr, ns, mvar)
    MVar.takeMVar mvar

ghci_flags :: FilePath
ghci_flags = BUILD_DIR </> "ghci-flags"

-- load .o:
-- target <- GHC.guessTarget fname Nothing (phase)
--
-- guessTarget appears to only be for haskell modules
-- look at Target: Target { targetId :: TargetId, targetAllowObjCode = ?,
--      targetContents = Nothing }
--      TargetId: TargetFile fpath Nothing
--
-- Main.hs does: mapM_ (consIORef v_Ld_inputs) (reverse objs)
-- TODO using objTarget below causes it to silently not link anything.  Am
-- I losing a warning?

interpreter :: Session -> IO ()
interpreter (Session chan) = do
    GHC.parseStaticFlags []  -- not sure if this is necessary
    -- let ofile = "build/test/obj/Util/Git/libgit_wrappers.cc.o"
    -- let objTarget = GHC.Target (GHC.TargetFile ofile Nothing) False Nothing
    -- IORef.modifyIORef GHC.v_Ld_inputs ("hi":)
    flags <- Exception.try (readFile ghci_flags)
    args <- case flags of
        Left (exc :: Exception.SomeException) -> do
            Log.error $ "error reading ghci flags from "
                ++ show ghci_flags ++ ": " ++ show exc
                ++ ", the REPL is probably not going to work"
            return []
        Right flags -> return $ words flags

    -- run :: GHC.DynFlags -> Ghc a -> Ghc a
    -- run dflags = GHC.defaultErrorHandler dflags
    --          . GHC.defaultCleanupHandler dflags
    GHC.runGhcT (Just GHC.Paths.libdir) $ do
        parse_flags args
        -- Otherwise I get
        -- Cannot add module Cmd.Lang.Environ to context: not interpreted
        GHC.setTargets $ [make_target False toplevel]
        -- GHC.setTargets $ objTarget : [make_target True toplevel]
        (result, logs, warns) <- reload
        case result of
            Left err -> liftIO $
                Log.warn $ "error loading REPL modules: " ++ err
                    ++ Seq.join "; " warns ++ " / " ++ Seq.join "; " logs
            _ -> return ()
        forever $ do
            (expr, namespace, return_mvar) <- liftIO $ Chan.readChan chan
            result <- case expr of
                ':' : colon -> colon_cmd colon
                _ -> normal_cmd namespace expr
                    `GHC.gcatch` \(exc :: Exception.SomeException) ->
                        -- set_context throws if the reload failed.
                        return $ return $  "Exception: " ++ show exc
            liftIO $ MVar.putMVar return_mvar result
    where
    toplevel = "Cmd.Lang.Environ"

    normal_cmd :: Id.Namespace -> String -> Ghc Cmd
    normal_cmd namespace expr = case expand_macros namespace expr of
        Left err -> return $ return $ "expand_macros: " ++ err
        Right expr -> do
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
        Left err -> return $ "Failed: " ++ err
        Right cmd -> cmd
    where
    decorate = fmap (\s -> log_msg ++ s ++ warn_msg)
    warn_msg =
        if null warns then "" else "\nWarnings:\n" ++ Seq.join "\n" warns
    log_msg =
        if null logs then "" else "Logs:\n" ++ Seq.join "\n" logs ++ "\n"


-- * implementation

-- | Replace \@some-id with @(auto_id ns \"some-id\")@
expand_macros :: Id.Namespace -> String -> Either String String
expand_macros namespace expr = ParseBs.expand_macros replace expr
    where
    replace ident = "(auto_id " <> show (Id.un_namespace namespace) <> " "
        <> show ident <> ")"

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
    typed_expr = "fmap PPrint.str_pshow (" ++ expr ++ ") :: " ++ cmd_type
    coerce val = GHC.Exts.unsafeCoerce# val :: Cmd

set_context :: [String] -> Ghc ()
set_context mod_names = do
#if __GLASGOW_HASKELL__ >= 74
    let prelude = GHC.simpleImportDecl (GHC.mkModuleName "Prelude")
    GHC.setContext $ GHC.IIDecl prelude
        : map (GHC.IIModule . GHC.mkModuleName) mod_names
#else
    mods <- sequence
        [GHC.findModule (GHC.mkModuleName m) Nothing | m <- mod_names]
    prelude <- GHC.findModule (GHC.mkModuleName "Prelude") Nothing
    -- Get the entire top level scope of the first arg, and the exports
    -- from the second (non HPT imports).
    GHC.setContext mods [(prelude, Nothing)]
#endif

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
#if __GLASGOW_HASKELL__ >= 74
-- getWarnings is gone, apparently replaced by either printing directly or
-- throwing an exception, e.g. compiler/main/HscTypes.lhs:handleFlagWarnings.
get_warnings = return []
#else
get_warnings = do
    warns <- GHC.getWarnings
    GHC.clearWarnings
    return (extract warns)
    where
    extract = map (Outputable.showSDoc . ErrUtils.errMsgShortDoc)
        . Bag.bagToList
#endif

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
