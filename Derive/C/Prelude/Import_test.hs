-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Import_test where
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Ui.UiTest as UiTest

import           Util.Test


test_import :: Test
test_import = do
    let run title = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks title [(">", [(0, 1, "X")])]
    strings_like (snd (run "")) ["note generator not found"]
    equal (run "import bali.reyong") ([0, 0, 0, 0], [])
    strings_like (snd (run "import europe no-such-module"))
        ["no calls in the imported module"]
    strings_like (snd (run "import no-such-module"))
        ["no calls in the imported module"]

    strings_like (snd (run "imports bali.reyong no-sym")) ["symbols not in"]
    equal (run "imports bali.reyong X") ([0, 0, 0, 0], [])

test_scale :: Test
test_scale = do
    let run title pitch = DeriveTest.extract Score.initial_note $
            DeriveTest.derive_tracks title $
            UiTest.note_track [(0, 1, pitch)]
    equal (run "scale pelog" "41") ([Just "41"], [])
    equal (run "scale-inst=gender-panerus | scale pelog" "1") ([Just "1"], [])
    -- scale also sets scale= env var, so it defaults if you call it again.
    equal (run "scale pelog | scale-inst=gender-panerus | scale" "1")
        ([Just "1"], [])
