-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.LoadInstruments_test where
import qualified App.LoadInstruments as LoadInstruments
import qualified App.Path as Path

import           Util.Test


test_warns :: Test
test_warns = do
    (_, warns) <- LoadInstruments.load_synths (Path.AppDir ".")
    -- TODO Fix it:
    -- ["sc-kempli: shadowed note calls in module inst: \"+\": +; +"]
    equal warns []
    equal LoadInstruments.synth_warnings []
