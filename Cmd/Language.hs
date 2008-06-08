{- | Process a textual language, which may look familiar, to perform UI state
changes.

The incoming commands are received via Msg.Socket msgs.
-}
module Cmd.Language where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans as Trans

import qualified Language.Haskell.Interpreter.GHC as GHC
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg


-- TODO Later can be configured?  Or is it just cwd?  It might mess up the
-- importing if it's not '.', unless I can use dynFlags to set -i.
app_dir = ""

cmd_language :: GHC.InterpreterSession -> Msg.Msg -> Cmd.CmdIO
cmd_language session msg = do
    (response_hdl, text) <- case msg of
        Msg.Socket hdl s -> return (hdl, s)
        _ -> Cmd.abort
    Log.notice $ "got lang: " ++ show text
    ui_state <- State.get
    cmd_state <- Cmd.get_state
    local_modules <- get_local_modules

    cmd <- Trans.liftIO $ GHC.withSession session
            (interpret local_modules ui_state cmd_state text)
        `Exception.catchDyn` catch_interpreter_error
        `Exception.catch` catch_all
    response <- cmd
    Trans.liftIO $ catch_io_errors $ do
        when (not (null response)) $
            IO.hPutStrLn response_hdl response
        IO.hClose response_hdl
    return $ if response == (magic_quit_string++"\n")
        then Cmd.Quit else Cmd.Done

get_local_modules = do
    let lang_dir = app_dir </> "Local" </> "Lang"
    fns <- Trans.liftIO $ Directory.getDirectoryContents lang_dir
        `Exception.catch` \exc -> do
            Log.warn $ "error reading local lang dir: " ++ show exc
            return []
    let mod_fns = map (lang_dir </>) (filter is_hs fns)
    mod_fns <- Trans.liftIO $ filterM Directory.doesFileExist mod_fns
    return $ map (Seq.replace "/" "." . FilePath.dropExtension) mod_fns

is_hs fn = take 1 fn /= "." && FilePath.takeExtension fn == ".hs"

-- | Hack so that language cmds can quit the app, since they return strings.
magic_quit_string :: String
magic_quit_string = "-- * YES, really quit * --"

catch_io_errors = Exception.handleJust Exception.ioErrors $ \exc -> do
    Log.warn $ "caught exception from socket write: " ++ show exc

catch_interpreter_error :: GHC.InterpreterError -> IO (Cmd.CmdT IO String)
catch_interpreter_error exc = return $ do
    Log.warn ("interpreter error: " ++ show_ghc_exc exc)
    return $ "interpreter error: " ++ show_ghc_exc exc

catch_all :: Exception.Exception -> IO (Cmd.CmdT IO String)
catch_all exc = return $ do
    Log.warn ("error: " ++ show exc)
    return $ "error: " ++ show exc

show_ghc_exc (GHC.WontCompile ghc_errs) =
    "Won't compile " ++ Seq.join "\n" (map GHC.errMsg ghc_errs)
show_ghc_exc exc = show exc

-- | Interpreted code should be of this type.  However, due to
-- 'mangle_code', it really runs in CmdT Identity String
type LangType = State.State -> Cmd.State -> Cmd.CmdVal String

-- | Interpret the given string inside a CmdT Identity monad, and return
-- the resulting CmdT.
--
-- Since I got errors trying to have the type of the code be CmdT directly
-- ("error loading interface for Cmd"), I do a workaround where I have it
-- return a function of type LangType instead.  LanguageEnviron contains
-- a 'run' function to run that in CmdT, and then this function packages it
-- back up in a CmdT again and returns it.  It's a little roundabout but it
-- seems to work.
--
-- TODO figure out what the original error means, and if I can get around it
interpret :: [GHC.ModuleName] -> State.State -> Cmd.State -> String
    -> GHC.Interpreter (Cmd.CmdT IO String)
interpret local_mods ui_state cmd_state text = do
    GHC.loadModules $ ["Cmd.LanguageEnviron"] ++ local_mods
    GHC.setTopLevelModules ["Cmd.LanguageEnviron"]
    GHC.setImports $ ["Prelude"] ++ local_mods

    cmd_func <- GHC.interpret (mangle_code text) (GHC.as :: LangType)
    let (cmd_state2, _midi, logs, ui_res) = cmd_func ui_state cmd_state
    return (merge_cmd_state cmd_state2 logs ui_res)

-- | Create a CmdT that merges the given state into itself.
merge_cmd_state cmd_state logs ui_res = do
    Cmd.modify_state (const cmd_state)
    mapM_ Log.write logs
    case ui_res of
        Left (State.StateError err) -> return $ "error: " ++ err
        Right (response, ui_state2, updates) -> do
            -- I trust that they modified the state through the State
            -- ops, which means the updates should reflect any track
            -- changes.
            State.put ui_state2
            mapM_ State.update updates
            return response

-- | Automatically put the input code into CmdT by putting it in
-- LanguageEnviron.run.
mangle_code :: String -> String
mangle_code text = Seq.strip $ "run $ do\n" ++ indent text
    where indent = unlines . map ("    "++) . lines
