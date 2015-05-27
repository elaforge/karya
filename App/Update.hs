-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Load and save files to update them to the latest version.  Useful when
-- a non-versioned datatype changes.
--
-- Git saves are flattened into a plain saves.
module App.Update where
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Ui.State as State
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import Global


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [from_fn, to_fn] -> update from_fn to_fn
        _ -> fail_with "usage: update from_fn to_fn"

update :: FilePath -> FilePath -> IO ()
update from_fn to_fn = do
    either_state <- if SaveGit.is_git from_fn
        then load_git from_fn
        else unserialize from_fn
    case either_state of
        Left err -> fail_with $ "Reading " <> showt from_fn <> ": " <> err
        Right state -> Save.write_state to_fn state

unserialize :: FilePath -> IO (Either Text State.State)
unserialize = fmap fix . Save.read_state_
    where
    fix (Left err) = Left err
    fix (Right Nothing) = Left "file not found"
    fix (Right (Just v)) = Right v

load_git :: SaveGit.Repo -> IO (Either Text State.State)
load_git repo = fmap extract <$> SaveGit.load repo Nothing
    where extract (state, _, _) = state

err_msg :: Text -> IO ()
err_msg = Text.IO.hPutStrLn IO.stderr

fail_with :: Text -> IO ()
fail_with msg = do
    err_msg msg
    Exit.exitWith (Exit.ExitFailure 1)
