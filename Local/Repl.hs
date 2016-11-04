-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Put your own local commands here and reload with @:r@.  You can also put
-- local imports, e.g.  import modules out of Local.Instrument to get at
-- per-instrument allocations.
module Local.Repl where
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Local.Instrument.Kontakt.ScGamelan as Kontakt.ScGamelan
import qualified Local.Instrument.Kontakt.Wayang as Kontakt.Wayang


test_cmd :: Cmd.CmdL ()
test_cmd = Log.notice "test command"


wayang_allocations = Kontakt.Wayang.allocations
sc_gamelan_allocations = Kontakt.ScGamelan.kebyar_allocations
