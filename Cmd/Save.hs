{- | Functions to save and restore state to and from files.
-}
module Cmd.Save where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Directory as Directory

import qualified Util.Log as Log

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd

version_string = "seq state, 1"

cmd_save maybe_fname = do
    cmd_state <- Cmd.get_state
    ui_state <- State.get
    let fname = maybe (Cmd.state_default_save_file cmd_state) id maybe_fname
    Log.notice $ "write state to " ++ show fname
    Trans.liftIO $ write fname (version_string ++ "\n"
        ++ serialize_ui_state ui_state ++ "\n\n"
        ++ serialize_cmd_state cmd_state ++ "\n")

cmd_load maybe_fname = do
    cmd_state <- Cmd.get_state
    let fname = maybe (Cmd.state_default_save_file cmd_state) id maybe_fname
    Log.notice $ "read state from " ++ show fname
    [ver, ui_str, _, cmd_str] <- fmap lines (Trans.liftIO (IO.readFile fname))
    State.modify (const (deserialize_ui_state ui_str))
    Cmd.modify_state (const (deserialize_cmd_state cmd_str))
    -- Emit track updates for all tracks, just like an undo.
    State.update_all_tracks

enoent_exc exc = case Exception.ioErrors exc of
    Just io_error | IO.Error.isDoesNotExistError io_error -> Just io_error
    _ -> Nothing

write filename s = do
    Exception.catchJust enoent_exc
        (Directory.renameFile filename (filename ++ ".last"))
        (\exc -> return ())
    IO.writeFile filename s

serialize_ui_state :: State.State -> String
serialize_ui_state st = show st

deserialize_ui_state :: String -> State.State
deserialize_ui_state s = read s

serialize_cmd_state :: Cmd.State -> String
-- It doesn't make any sense to save the current keydown map.
serialize_cmd_state st = show (st { Cmd.state_keys_down = Map.empty })

deserialize_cmd_state :: String -> Cmd.State
deserialize_cmd_state s = read s
