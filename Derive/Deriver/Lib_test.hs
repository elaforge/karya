-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Deriver.Lib_test where
import qualified Data.Map as Map

import Util.Test
import qualified Derive.Controls as Controls
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
import Global


test_with_instrument_controls = do
    let run title controls = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_tracks_setup (with_config controls) title
                [(">i1", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
        with_config controls = DeriveTest.with_midi_config "i1" "s/1"
            (Common.controls #= controls $ Common.empty_config)
            (Patch.config mempty [])
    -- This doesn't test the controls directly, but rather that the
    -- transposition is applied as expected.
    equal (run "" mempty) ([(0, 1, "4c")], [])
    let octave_up = Map.fromList [(Controls.octave, 1)]
    equal (run "" octave_up) ([(0, 1, "5c")], [])
    -- Controls are merged with their default mergers.
    equal (run "%t-oct=1" octave_up) ([(0, 1, "6c")], [])
