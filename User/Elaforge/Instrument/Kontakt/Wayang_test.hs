-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Kontakt.Wayang_test where
import qualified Util.Log as Log
import qualified Cmd.Repl.LAlloc as LAlloc
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Kontakt.KontaktTest as KontaktTest

import           Global
import           Util.Test


test_wayang :: Test
test_wayang = do
    let run notes = extract $ KontaktTest.perform allocs $
            Derive.r_events $ KontaktTest.derive allocs "" $
                UiTest.note_spec ("pu" <> wayang_title, notes, [])
        extract ((_, midi), logs) =
            (DeriveTest.note_on midi, map Log.msg_text logs)
    -- TODO this is an unfriendly low level msg, and why the c3?
    equal (run [(0, 1, "3i")])
        ([Key2.c3], ["out of range for patch scale: (0s, 50.5)"])
    equal (run [(0, 1, "3o")]) ([Key2.f3], [])
    equal (run [(0, 1, "4i")]) ([Key2.e4], [])
    equal (run [(1, 1, "+mute -- 4i")]) ([Key2.b_2, Key2.e0], [])
    equal (run [(2, 1, "+mute+loose -- 4i")]) ([Key2.a_2, Key2.e0], [])

test_wayang_zero_dur :: Test
test_wayang_zero_dur = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_blocks_setup (KontaktTest.with_synth allocs)
        top = "top -- scale=wayang | inst=pu | cancel"
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

test_wayang_pasang :: Test
test_wayang_pasang = do
    let run notes = KontaktTest.derive allocs "import bali.gangsa" $
            UiTest.note_spec (title, notes, [])
        title = wayang_title <> " | inst=p | unison"
    equal (DeriveTest.extract DeriveTest.e_instrument $ run [(0, 1, "")])
        (["pu", "pi"], [])
    let result = run [(0, 1, "4i")]
    equal (DeriveTest.extract DeriveTest.e_instrument result)
        (["pu", "pi"], [])
    equal (fst $ DeriveTest.extract Score.initial_nn result)
        [Just 62.5, Just 63]

    let ((_events, midi), logs) =
            KontaktTest.perform allocs (Derive.r_events result)
    equal logs []
    -- Note no PitchBend, which means the split instruments applied
    -- tuning=umbang and tuning=isep to their pitches.
    equal (DeriveTest.midi_channel midi)
        [ (1, Midi.ControlChange 1 0), (0, Midi.ControlChange 1 0)
        , (1, Midi.NoteOn Key.e5 127), (0, Midi.NoteOn Key.e5 127)
        , (1, Midi.NoteOff Key.e5 127), (0, Midi.NoteOff Key.e5 127)
        ]

test_wayang_kempyung :: Test
test_wayang_kempyung = do
    let run append notes = DeriveTest.extract extract $
            KontaktTest.derive allocs "import bali.gangsa" $
            UiTest.note_spec
                (wayang_title <> append <> " | kempyung", notes, [])
        extract e = (DeriveTest.e_instrument e, DeriveTest.e_note e)
    -- Top note is 6i.
    equal (run " | inst=k" [(0, 1, "5e"), (1, 1, "5u")])
        ([ ("ku", (0, 1, "5e")), ("ki", (0, 1, "6i"))
         , ("ku", (1, 1, "5u")), ("ki", (1, 1, "5u"))
         ], [])
    equal (run " | inst=p" [(0, 1, "4e"), (1, 1, "4u")])
        ([ ("pu", (0, 1, "4e")), ("pi", (0, 1, "5i"))
         , ("pu", (1, 1, "4u")), ("pi", (1, 1, "4u"))
         ], [])
    equal (run " | inst=p | inst-top = (pitch (4a))"
            [(0, 1, "4o"), (1, 1, "4e")])
        ([ ("pu", (0, 1, "4o")), ("pi", (0, 1, "4a"))
         , ("pu", (1, 1, "4e")), ("pi", (1, 1, "4e"))
         ], [])

allocs :: UiConfig.Allocations
allocs = LAlloc.wayang_midi "test" "p" "k"

wayang_title :: Text
wayang_title = " | scale=wayang"
