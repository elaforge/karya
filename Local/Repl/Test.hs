-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Put your own local commands here.
module Local.Repl.Test where
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd


test_cmd :: Cmd.CmdL ()
test_cmd = Log.notice "test command"
