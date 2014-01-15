-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Bali.Kotekan_test where
import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_norot = do
    let run = derive extract $
            inst_title <> " | inst-top = (pitch (4f))"
        extract e = (DeriveTest.e_note e, Score.event_instrument e)
    equal (run [(2, -2, "norot 1 -- 3a")])
        ([ ((1, 1, "3b"), polos), ((1, 1, "4e"), sangsih)
         , ((2, -1, "3a"), polos), ((2, -1, "4d"), sangsih)
         ], [])
    equal (run [(3, -3, "norot 1 -- 3a")])
        ([ ((1, 1, "3a"), polos), ((1, 1, "4d"), sangsih)
         , ((2, 1, "3b"), polos), ((2, 1, "4e"), sangsih)
         , ((3, -1, "3a"), polos), ((3, -1, "4d"), sangsih)
         ], [])
    equal (run [(2, -2, "norot 1 -- 4c")])
        ([ ((1, 1, "4d"), polos), ((1, 1, "4d"), sangsih)
         , ((2, -1, "4c"), polos), ((2, -1, "4f"), sangsih)
         ], [])
    equal (run [(2, -2, "norot 1 diamond -- 4c")])
        ([ ((1, 1, "4d"), polos), ((1, 1, "3b"), sangsih)
         , ((2, -1, "4c"), polos), ((2, -1, "4c"), sangsih)
         ], [])
    equal (run [(2, -2, "kotekan = 2 | norot 1 -- 3a")])
        ([((1, 1, "3b"), sangsih), ((2, -1, "3a"), polos)], [])

    equal (run [(8, -8, "kotekan = 2 | >norot 1 -- 3a")])
        ([ ((5, 1, "3a"), polos), ((5, 1, "3a"), sangsih)
         , ((6, 1, "3a"), polos), ((6, 1, "3a"), sangsih)
         , ((7, 1, "3b"), sangsih)
         , ((8, -1, "3a"), polos)
         ], [])

test_gender_norot = do
    let run = derive_pasang extract ""
        extract e = (Score.event_start e, DeriveTest.e_pitch e)
    pprint (run [(5, -5, "gnorot 1 -- 3a")])


test_kempyung = do
    let run title = derive extract (inst_title <> title <> " | kempyung")
        extract e = (Score.event_start e, Score.initial_note e)
        notes = [(0, 1, "4c"), (1, 1, "4d")]
    equal (run "" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4g")], [])
    equal (run " | inst-top = (pitch (4f))" notes)
        ([(0, Just "4c"), (0, Just "4f"), (1, Just "4d"), (1, Just "4d")], [])

test_nyogcag = do
    let run = derive extract (inst_title <> " | nyog")
        extract e = (Score.event_start e, DeriveTest.e_inst e)
    let notes = [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")]
    equal (run notes) ([(0, "i1"), (1, "i2"), (2, "i1")], [])

test_noltol = do
    let run arg = derive extract (" | noltol " <> arg)
        extract e = (Score.event_start e, DeriveTest.e_inst e,
            DeriveTest.e_attributes e)
    let notes = [(0, 1, "n >i1 -- 4c"), (1, 1, "n >i2 -- 4d"),
            (2, 1, "n >i1 -- 4e")]
    equal (run "2.5" notes)
        ([(0, "i1", "+"), (1, "i2", "+"), (2, "i1", "+")], [])
    equal (run "2" notes)
        ([ (0, "i1", "+"), (1, "i1", "+loose+mute"), (1, "i2", "+")
         , (2, "i1", "+")
         ], [])

    let run2 = derive extract (inst_title <> " | noltol 1 | nyog")
    equal (run2 [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (3, 1, "4f")])
        ([ (0, "i1", "+"), (1, "i1", "+loose+mute")
         , (1, "i2", "+"), (2, "i2", "+loose+mute")
         , (2, "i1", "+"), (3, "i2", "+")
         ], [])

derive_pasang :: (Score.Event -> a) -> String -> [UiTest.EventSpec]
    -> (([a], [a]), [String])
derive_pasang extract title notes =
    e_pasang extract $ DeriveTest.derive_tracks $
    UiTest.note_spec (inst_title <> title, notes, [])

e_pasang :: (Score.Event -> a) -> Derive.Result -> (([a], [a]), [String])
e_pasang extract = first group_inst
    . DeriveTest.extract (\e -> (Score.event_instrument e, extract e))
    where
    group_inst ns = ([n | (inst, n) <- ns, inst == polos],
        [n | (inst, n) <- ns, inst == sangsih])

derive :: (Score.Event -> a) -> String -> [UiTest.EventSpec] -> ([a], [String])
derive extract title notes =
    DeriveTest.extract extract $ DeriveTest.derive_tracks $
    UiTest.note_spec (title, notes, [])

inst_title :: String
inst_title = "i3 | inst-polos = >i1 | inst-sangsih = >i2"

polos :: Score.Instrument
polos = Score.Instrument "i1"

sangsih :: Score.Instrument
sangsih = Score.Instrument "i2"
