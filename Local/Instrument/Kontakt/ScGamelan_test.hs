-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt.ScGamelan_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Kontakt.ScGamelan as ScGamelan

import Global


test_wrap_octaves = do
    let run extract title = DeriveTest.extract extract
            . derive ("scale=legong" <> title) . UiTest.note_track1
    equal (run DeriveTest.e_pitch " | inst = calung-p"
            ["3u", "4i", "4u", "5i"])
        (["4u", "4i", "4u", "4i"], [])
    -- Ensure transposition isn't applied twice.
    equal (run DeriveTest.e_pitch " | inst = calung-p | %t-dia=1" ["4a"])
        (["4i"], [])
    -- I can wrap even if it goes below the range of the scale itself.
    equal (run DeriveTest.e_pitch " | inst = jegog-p | %t-dia=-1" ["3i"])
        (["3a"], [])
    let extract e = (DeriveTest.e_instrument e, DeriveTest.e_pitch e)
    equal (run extract " | inst = calung | import bali.gangsa | unison"
        ["4u", "5i"])
        ( [ ("calung-p", "4u"), ("calung-s", "4u")
          , ("calung-p", "4i"), ("calung-s", "4i")
          ]
        , []
        )

derive :: String -> [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_setup $
    DeriveTest.with_synths (ScGamelan.kebyar_allocations "dev") [Kontakt.synth]
