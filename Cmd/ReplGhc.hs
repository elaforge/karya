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
    Session(..), make_session
    , interpreter, interpret, complete
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.Char as Char
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Text as Text

import           System.FilePath ((</>))

-- GHC imports
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths

#if GHC_VERSION >= 90001

import qualified Control.Monad.Catch as Catch
import qualified GHC.Utils.Outputable as Outputable
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.Driver.CmdLine as CmdLine
import           Global (liftIO)

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

import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Seq as Seq
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
build_dir :: FilePath
build_dir = BUILD_DIR

ghci_flags :: FilePath
ghci_flags = build_dir </> "ghci-flags"

-- | The actual session runs in another thread, so this is the communication
-- channel.  @(expr, namespace, response_mvar)@
newtype Session = Session (Chan.Chan (Query, MVar.MVar Response))

data Query = QCommand !Text | QCompletion !Text
    deriving (Show)
data Response = RCommand !Cmd | RCompletion ![Text]
type Cmd = Cmd.CmdT IO ReplProtocol.CmdResult

type Ghc a = GHC.GhcT IO a

make_session :: IO Session
make_session = Session <$> Chan.newChan

interpret :: Session -> Text -> IO Cmd
interpret (Session chan) expr = do
    mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (QCommand expr, mvar)
    response <- MVar.takeMVar mvar
    case response of
        RCommand cmd -> return cmd
        _ -> errorIO "unexpected response to QCommand"

complete :: Session -> Text -> IO [Text]
complete (Session chan) prefix = do
    mvar <- MVar.newEmptyMVar
    Chan.writeChan chan (QCompletion prefix, mvar)
    response <- MVar.takeMVar mvar
    case response of
        RCompletion words -> return words
        _ -> errorIO "unexpected response to QCompletion"

-- | Initialize the GHC API and run the interpreter loop.
interpreter :: Session -> IO ()
interpreter (Session chan) = do
#if GHC_VERSION < 80200
    GHC.parseStaticFlags [] -- not sure if this is necessary
#endif
    flags <- File.tryIO (readFile ghci_flags)
    args <- case flags of
        Left exc -> do
            Log.error $ "error reading ghci flags from "
                <> showt ghci_flags <> ": " <> showt exc
                <> ", the REPL is probably not going to work"
            return []
        Right flags -> return $ lines flags

    GHC.runGhcT (Just GHC.Paths.libdir) $ do
        parse_flags args
        -- obj_allowed must be False, otherwise I get
        -- Cannot add module Cmd.Repl.Environ to context: not interpreted
        GHC.setTargets $ map (make_target False) toplevel_modules
        ((result, logs, warns), time_msg) <-
            Thread.timeActionText (reload toplevel_modules)
        let expected = map ((++ ".hs, interpreted") . Seq.replace1 '.' "/")
                (toplevel_modules ++ expected_reloads)
        logs <- return $ filter
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
            Log.notice $ "loaded modules for repl: " <> time_msg
            case result of
                Left err -> Log.warn $ "error loading REPL modules: " <> txt err
                _ -> return ()
        forever $ do
            (query, return_mvar) <- liftIO $ Chan.readChan chan
            result <- respond toplevel_modules query
            liftIO $ MVar.putMVar return_mvar result
    where
    expected_reloads = ["User.Elaforge.Repl"] -- TODO should be User.*.Repl
    toplevel_modules = ["Cmd.Repl.Environ", "Local.Repl"]


respond :: [String] -> Query -> GHC.GhcT IO Response
respond toplevel_modules (QCommand e) = RCommand <$> case untxt e of
    ':' : colon -> colon_cmd (map Char.toLower colon)
    expr -> (`gcatch` catch) $ make_response <$> compile expr
    where
    catch (exc :: Exception.SomeException) =
        return $ return $ ReplProtocol.error_result $ "Exception: " <> showt exc
    colon_cmd :: String -> Ghc Cmd
    colon_cmd cmd
        | cmd `elem` ["r", "reload"] = make_response <$> reload toplevel_modules
        | otherwise = return $ return $
            ReplProtocol.error_result $ "Unknown colon command: " <> showt cmd
respond _ (QCompletion prefix) = RCompletion <$>
    -- Don't bother with 50 zillion completions.
    if prefix == "" then return [] else do
        rdrs <- GHC.getRdrNamesInScope
        dflags <- GHC.getSessionDynFlags
        return $ filter (prefix `Text.isPrefixOf`) $
            Seq.unique_sort $ map (txt . Outputable.showPpr dflags) rdrs

make_response :: Result (Cmd.CmdL ReplProtocol.Result) -> Cmd
make_response (val, logs, warns) = case val of
    Left err -> return $ ReplProtocol.CmdResult
        (ReplProtocol.Raw ("compile error: " <> txt err)) all_logs
    Right cmd -> do
        result <- cmd
        return $ ReplProtocol.CmdResult result all_logs
    where
    all_logs = map (Log.msg Log.Notice Nothing . txt) logs
        ++ map (Log.msg Log.Warn Nothing . txt) warns

-- * implementation

-- | (Either error cmd, logs, warns)
type Result a = (Either String a, [String], [String])

-- | Load or reload the target modules.  Return errors if the load failed.
reload :: [String] -> Ghc (Result (Cmd.CmdL ReplProtocol.Result))
reload toplevel_modules = do
    (result, logs, warns) <- collect_logs $
        GHC.load GHC.LoadAllTargets <* set_context toplevel_modules
    return (mkcmd result, logs, warns)
    where
    mkcmd (Right ok)
        | GHC.succeeded ok = Right (return $ ReplProtocol.Raw "reloaded")
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
    (hval, logs, warns) <- collect_logs $ GHC.compileExpr typed_expr
    return (fmap coerce hval, logs, warns)
    where
    -- The _to_result method is defined in "Cmd.Repl.Global".
    typed_expr =
        "fmap _to_result (" ++ expr ++ ") :: Cmd.CmdL ReplProtocol.Result"
    -- This should be safe because I just asserted the the type above.
    coerce val = GHC.Exts.unsafeCoerce# val :: Cmd.CmdL ReplProtocol.Result

set_context :: [String] -> Ghc ()
set_context mod_names = do
    let prelude = GHC.simpleImportDecl (GHC.mkModuleName "Prelude")
    GHC.setContext $ GHC.IIDecl prelude
        : map (GHC.IIModule . GHC.mkModuleName) mod_names

-- | Run a Ghc action and collect logs and warns.
collect_logs :: Ghc a -> Ghc (Result a)
collect_logs action = do
    logs <- liftIO $ IORef.newIORef []
    val <- fmap Right (catch_logs logs >> action) `gcatch`
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

-- type LogAction = DynFlags -> WarnReason -> Severity -> SrcSpan
--               -> MsgDoc -> IO ()

#if GHC_VERSION >= 90001
log_action :: IORef.IORef [String] -> DynFlags.LogAction
log_action logs dflags _warn_reason _severity _span msg =
    liftIO $ IORef.modifyIORef logs (formatted:)
    where formatted = Outputable.showSDoc dflags msg
#else
log_action :: IORef.IORef [String]
    -> GHC.DynFlags -> DynFlags.WarnReason -> GHC.Severity -> GHC.SrcSpan
    -> Outputable.PprStyle -> Outputable.SDoc -> IO ()
log_action logs dflags _warn_reason _severity _span style msg =
    liftIO $ IORef.modifyIORef logs (formatted:)
    where
    formatted = Outputable.showSDoc dflags $
        Outputable.withPprStyle style msg
#endif

modify_flags :: (GHC.DynFlags -> GHC.DynFlags) -> Ghc ()
modify_flags f = do
    dflags <- GHC.getSessionDynFlags
    void $ GHC.setSessionDynFlags $! f dflags

parse_flags :: [String] -> Ghc ()
parse_flags args = do
    dflags <- GHC.getSessionDynFlags
    (dflags, args_left, warns) <- GHC.parseDynamicFlags dflags
        (map (GHC.mkGeneralLocated "cmdline") args)
#if GHC_VERSION >= 80401
    let un_msg = GHC.unLoc . CmdLine.warnMsg
#else
    let un_msg = GHC.unLoc
#endif
    unless (null warns) $
        liftIO $ Log.warn $ "warnings parsing flags " <> showt args <> ": "
            <> showt (map un_msg warns)
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
