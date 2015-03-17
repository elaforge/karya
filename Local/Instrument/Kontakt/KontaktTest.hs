-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt.KontaktTest where
import qualified Util.Log as Log
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.Midi.Perform as Perform
import qualified Local.Instrument.Kontakt as Kontakt
import Global


derive :: String -> [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_with with_synth

with_synth :: Derive.Deriver a -> Derive.Deriver a
with_synth = DeriveTest.with_inst_db Kontakt.synth_descs

perform :: [Text] -> Derive.Events
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform insts = DeriveTest.perform_inst Kontakt.synth_descs
    [(inst, [n]) | (n, inst) <- zip [0..] insts]
