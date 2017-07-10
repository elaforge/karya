-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Put your own local commands here and reload with @:r@.  You can also put
-- local imports, e.g.  import modules out of Local.Instrument to get at
-- per-instrument allocations.
module Local.Repl where
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Repl.LInst as LInst

import qualified Local.Instrument.Kontakt.ScGamelan as Kontakt.ScGamelan
import qualified Local.Instrument.Kontakt.Wayang as Kontakt.Wayang


test_cmd :: Cmd.CmdL ()
test_cmd = Log.notice "test command"


wayang_allocations = Kontakt.Wayang.allocations
sc_gamelan_allocations = Kontakt.ScGamelan.kebyar_allocations


underwater = do
    let a = LInst.add
    a "marim" "pianoteq/" "loop1" [0]
    a "bass" "pianoteq/" "loop1" [1]
    a "lead" "pianoteq/" "loop1" [2]
    a "synb" "pianoteq/" "loop1" [3]
    a "pan" "pianoteq/" "loop1" [4]
    a "wood" "pianoteq/" "loop1" [5]
    a "pizz" "pianoteq/" "loop1" [6]

    -- chords
    a "maj" "pianoteq/" "loop3" []
    a "min" "pianoteq/" "loop3" []
    a "fmaj" "pianoteq/" "loop3" []
    -- sfx
    a "rush1" "pianoteq/" "loop3" []
    a "rush2" "pianoteq/" "loop3" []
    -- percussion
    a "shake" "pianoteq/" "loop2" []
    a "bd" "pianoteq/" "loop2" []

piano = do
    let a name = LInst.add name "pianoteq/" "loop1" [0]
    a "pno"
    a "bass"
    a "glock"
    a "pipe"
    a "string"
    a "bell"

elektrodes = do
    let a = LInst.add
    a "elec" "pianoteq/" "loop1" [0]
    a "bass" "pianoteq/" "loop1" [1]
    a "syn" "pianoteq/" "loop1" [2]
    a "pno" "pianoteq/" "loop1" [3]
    a "hit" "pianoteq/" "loop1" [4]

    -- percussion
    a "bd" "pianoteq/" "loop2" []
    a "sn" "pianoteq/" "loop2" []
    a "hh-c" "pianoteq/" "loop2" []
    a "hh-o" "pianoteq/" "loop2" []
    a "hh-m" "pianoteq/" "loop2" []
    a "tom" "pianoteq/" "loop2" []
