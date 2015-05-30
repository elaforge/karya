-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt.KontaktTest where
import qualified Util.Log as Log
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Cmd.Simple as Simple
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.Midi.Perform as Perform
import qualified Local.Instrument.Kontakt as Kontakt


derive :: Simple.Aliases -> String -> [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_setup . with_synth

with_synth :: Simple.Aliases -> DeriveTest.Setup
with_synth aliases = DeriveTest.with_synth_descs aliases Kontakt.synth_descs

perform :: Simple.Aliases -> Derive.Events
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform aliases = DeriveTest.perform_inst aliases Kontakt.synth_descs
    [(inst, [n]) | (n, inst) <- zip [0..] (map fst aliases)]
