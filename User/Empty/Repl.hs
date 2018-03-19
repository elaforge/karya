-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Things defined in this module wind up in the global REPL namespace.
--
-- Put your own local commands here and reload with @:r@.  You can also put
-- local imports, e.g.  import modules out of Local.Instrument to get at
-- per-instrument allocations.
module User.Empty.Repl where
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd


test_cmd :: Cmd.CmdL ()
test_cmd = Log.notice "test command"
