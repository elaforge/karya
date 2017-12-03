-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Pianoteq_test where
import qualified Util.Log as Log
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Perform.Midi.Types as Midi.Types
import qualified Local.Instrument.Pianoteq as Pianoteq


test_harp_harmonic = do
    let run = perform . Derive.r_events
            . DeriveTest.derive_tracks_setup setup "inst=harp"
            . UiTest.note_track
        extract ((_, midi), logs) = (DeriveTest.note_on_cc midi, logs)

    -- Harmonic is cancelled out properly.
    equal (extract $ run [(0, 1, "o -- 4c"), (1, 1, "4d")])
        ( [ Right (66, 0), Right (69, 127), Right (67, 0), Left Key.c4
          , Right (69, 0), Left Key.d4
          ]
        , []
        )
    -- This actually tests a particular hack in Perform.Midi.Perform dealing
    -- with Signal.constant and notes <0.
    equal (extract $ run [(0, 1, "g 1 -- 4c"), (1, 1, "5c")])
        ( [ Right (66, 0), Right (69, 0), Right (67, 127), Left Key.d4
          , Right (67, 0), Left Key.c4
          , Left Key.c5
          ]
        , []
        )

setup :: DeriveTest.Setup
setup = DeriveTest.with_synths_simple allocs [Pianoteq.synth]

perform :: Stream.Stream Score.Event
    -> (([Midi.Types.Event], [Midi.WriteMessage]), [Log.Msg])
perform = DeriveTest.perform_synths_simple allocs [Pianoteq.synth]

allocs = [("harp", "pianoteq/harp")]
