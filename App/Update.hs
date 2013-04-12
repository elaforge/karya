-- | Load and save files to update them to the latest version.  Useful when
-- a non-versioned datatype changes.
--
-- Git saves are flattened into a plain saves.
module App.Update where
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import Util.Control
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize as Serialize


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [from_fn, to_fn] -> update from_fn to_fn
        _ -> fail_with "usage: update from_fn to_fn"

update :: String -> String -> IO ()
update from_fn to_fn = do
    either_state <- if SaveGit.is_git from_fn
        then load_git from_fn
        else unserialize from_fn
    case either_state of
        Left err -> fail_with $ "Reading " ++ show from_fn ++ ": " ++ err
        Right (Serialize.SaveState st dt) ->
            Serialize.serialize to_fn (Serialize.SaveState st dt)

unserialize :: FilePath -> IO (Either String Serialize.SaveState)
unserialize = fmap fix . Serialize.unserialize
    where
    fix (Left err) = Left err
    fix (Right Nothing) = Left "file not found"
    fix (Right (Just v)) = Right v

load_git :: SaveGit.Repo -> IO (Either String Serialize.SaveState)
load_git repo = do
    result <- SaveGit.load repo Nothing
    either (return . Left)
        (\(state, _, _) -> Right <$> Serialize.make_save_state state) result

err_msg :: String -> IO ()
err_msg = IO.hPutStrLn IO.stderr

fail_with :: String -> IO ()
fail_with msg = do
    err_msg msg
    Exit.exitWith (Exit.ExitFailure 1)
