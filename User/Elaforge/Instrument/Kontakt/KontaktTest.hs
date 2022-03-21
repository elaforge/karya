-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Kontakt.KontaktTest where
import qualified Util.Log as Log
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Types as Midi.Types
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Kontakt as Kontakt

import           Global


derive :: UiConfig.Allocations -> Text -> [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_setup . with_synth

with_synth :: UiConfig.Allocations -> DeriveTest.Setup
with_synth allocs = DeriveTest.with_synths allocs [Kontakt.synth]

perform :: UiConfig.Allocations -> Stream.Stream Score.Event
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
perform allocs = DeriveTest.perform_synths allocs [Kontakt.synth]

perform_synth :: UiConfig.Allocations -> Stream.Stream Score.Event
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
perform_synth allocs = DeriveTest.perform_synths allocs [Kontakt.synth]
