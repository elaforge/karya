-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Load and save files to update them to the latest version.  Useful when
-- a non-versioned datatype changes.
--
-- Git saves are flattened into a plain saves.
--
-- @tools/update_all.py@ can update a whole directory of saves.
module App.Update where
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Util.Git as Git
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Save as Save
import qualified Derive.DeriveSaved as DeriveSaved
import Global


main :: IO ()
main = Git.initialize $ do
    args <- Environment.getArgs
    case args of
        [from_fn, to_fn] -> update from_fn to_fn
        _ -> fail_with "usage: update from_fn to_fn"

update :: FilePath -> FilePath -> IO ()
update from_fn to_fn = do
    result <- Save.read_ from_fn
    case result of
        Left err -> fail_with $ "Reading " <> showt from_fn <> ": " <> err
        Right state -> Save.write_state to_fn state

err_msg :: Text -> IO ()
err_msg = Text.IO.hPutStrLn IO.stderr

fail_with :: Text -> IO ()
fail_with msg = do
    err_msg msg
    Exit.exitWith (Exit.ExitFailure 1)
