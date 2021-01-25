-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Miscellaneous utilities for Cmds.
--
-- "Cmd.Cmd" is forced to be huge, due to circular dependencies, so utilities
-- that don't fall foul of that should generally go in here.
module Cmd.CmdUtil where
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error

import qualified System.Exit as Exit

import qualified Util.Processes as Processes
import qualified Cmd.Cmd as Cmd

import           Global


read_process :: FilePath -> [String] -> Cmd.CmdT IO Text
read_process cmd args = do
    (exit, stdout, stderr) <- liftIO $
        Processes.readProcessWithExitCode Nothing cmd args ""
    case exit of
        Exit.ExitFailure code -> Cmd.throw $ "cmd failed: " <> showt code
            <> ", stderr: " <> decode_utf8 stderr
        _ -> return $ decode_utf8 stdout

decode_utf8 :: ByteString.ByteString -> Text
decode_utf8 = Encoding.decodeUtf8With Encoding.Error.lenientDecode
