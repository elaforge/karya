-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Kontakt.Wayang_test where
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified User.Elaforge.Instrument.Kontakt.KontaktTest as KontaktTest
import Global


test_wayang = do
    let run notes = extract $ KontaktTest.perform allocations $
            Derive.r_events $ KontaktTest.derive allocations "" $
                UiTest.note_spec ("wu" <> wayang_title, notes, [])
        extract ((_, midi), logs) = (DeriveTest.note_on midi, logs)
    equal (run [(0, 1, "4i")]) ([Key2.d3], [])
    equal (run [(1, 1, "+mute -- 4i")]) ([Key2.b_2, Key2.d_1], [])
    equal (run [(2, 1, "+mute+loose -- 4i")]) ([Key2.a_2, Key2.d_1], [])

test_wayang_zero_dur = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_blocks_setup
                (KontaktTest.with_synth allocations)
        top = "top -- inst = wu | cancel"
        extract e = (Score.event_duration e,
            not $ null $ DeriveTest.e_control "mute" e)
    -- should be muted, since it's 0 dur and not the end
    equal (run [(top, UiTest.note_track [(0, 0, "3i"), (1, 0, "3i")])])
        ([(1, True), (31, True)], [])
    -- The 3o is a final, not a mute.
    let sub = ("sub=ruler", UiTest.note_track [(0, 1, "3i"), (1, 0, "3o")])
    equal (run [(top, [(">", [(0, 1, "sub")])]), sub])
        ([(1, False), (1, False)], [])
    equal (run [(top, [(">", [(0, 1, "sub"), (1, 1, "sub")])]), sub])
        ([(1, False), (1, False), (1, False)], [])

test_wayang_pasang = do
    let run notes = KontaktTest.derive allocations "import bali.gangsa" $
            UiTest.note_spec (title, notes, [])
        title = wayang_title <> " | unison"
    equal (DeriveTest.extract DeriveTest.e_instrument $ run [(0, 1, "")])
        (["wu", "wi"], [])
    let result = run [(0, 1, "4i")]
    equal (DeriveTest.extract DeriveTest.e_instrument result)
        (["wu", "wi"], [])
    equal (fst $ DeriveTest.extract Score.initial_nn result)
        [Just 62.5, Just 63]

    let ((_events, midi), logs) =
            KontaktTest.perform allocations (Derive.r_events result)
    equal logs []
    -- Note no PitchBend, which means the split instruments applied
    -- tuning=umbang and tuning=isep to their pitches.
    equal (DeriveTest.midi_channel midi)
        [ (2, Midi.ControlChange 1 0), (1, Midi.ControlChange 1 0)
        , (2, Midi.NoteOn Key.e5 127), (1, Midi.NoteOn Key.e5 127)
        , (2, Midi.NoteOff Key.e5 127), (1, Midi.NoteOff Key.e5 127)
        ]

test_wayang_kempyung = do
    let run inst_suffix append notes = DeriveTest.extract extract $
            KontaktTest.derive (make_allocations inst_suffix)
                "import bali.gangsa" $ UiTest.note_spec
                (wayang_title <> append <> " | kempyung", notes, [])
        extract e = (DeriveTest.e_instrument e, DeriveTest.e_note e)
    -- Top note is 6i.
    equal (run "kantilan" "" [(0, 1, "5e"), (1, 1, "5u")])
        ([ ("wu", (0, 1, "5e")), ("wi", (0, 1, "6i"))
         , ("wu", (1, 1, "5u")), ("wi", (1, 1, "5u"))
         ], [])
    equal (run "pemade" "" [(0, 1, "4e"), (1, 1, "4u")])
        ([ ("wu", (0, 1, "4e")), ("wi", (0, 1, "5i"))
         , ("wu", (1, 1, "4u")), ("wi", (1, 1, "4u"))
         ], [])
    equal (run "pemade" " | inst-top = (pitch (4a))"
            [(0, 1, "4o"), (1, 1, "4e")])
        ([ ("wu", (0, 1, "4o")), ("wi", (0, 1, "4a"))
         , ("wu", (1, 1, "4e")), ("wi", (1, 1, "4e"))
         ], [])

allocations :: DeriveTest.SimpleAllocations
allocations = make_allocations "pemade"

make_allocations :: Text -> DeriveTest.SimpleAllocations
make_allocations suffix =
    [ ("w", "kontakt/wayang-" <> suffix)
    , ("wu", "kontakt/wayang-umbang")
    , ("wi", "kontakt/wayang-isep")
    ]

wayang_title :: Text
wayang_title = " | scale=wayang | inst=w | inst-polos=wu  | inst-sangsih=wi"
