module Local.Instrument.Kontakt.Wayang_test where
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.UiTest as UiTest
import qualified Cmd.Simple as Simple
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Local.Instrument.Kontakt.KontaktTest as KontaktTest
import Global


test_wayang = do
    let run notes = extract $ KontaktTest.perform aliases $
            Derive.r_events $ KontaktTest.derive aliases "" $
                UiTest.note_spec ("u", notes, [])
        extract (_, midi, logs) = (DeriveTest.note_on midi, logs)
    equal (run [(0, 1, "4i")]) ([Key2.e4], [])
    equal (run [(1, 1, "+mute -- 4i")]) ([Key2.b_2, Key2.e0], [])
    equal (run [(2, 1, "+mute+loose -- 4i")]) ([Key2.a_2, Key2.e0], [])

test_wayang_zero_dur = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_blocks_setup (KontaktTest.with_synth aliases)
        top = "top -- inst = >u | infer-duration"
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
    let run notes = KontaktTest.derive aliases "import bali.gangsa" $
            UiTest.note_spec (title, notes, [])
        title = wayang_title <> " | unison"
    equal (DeriveTest.extract DeriveTest.e_inst $ run [(0, 1, "")])
        (["u", "i"], [])
    let result = run [(0, 1, "4i")]
    equal (DeriveTest.extract DeriveTest.e_inst result)
        (["u", "i"], [])
    equal (fst $ DeriveTest.extract Score.initial_nn result)
        [Just 62.95, Just 62.5]

    let (_events, midi, logs) = KontaktTest.perform aliases
            (Derive.r_events result)
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
            KontaktTest.derive (make_aliases inst_suffix)
                "import bali.gangsa" $ UiTest.note_spec
                (wayang_title <> append <> " | kempyung", notes, [])
        extract e = (DeriveTest.e_inst e, DeriveTest.e_note e)
    -- Top note is 6i.
    equal (run "kantilan" "" [(0, 1, "5e"), (1, 1, "5u")])
        ([ ("u", (0, 1, "5e")), ("i", (0, 1, "6i"))
         , ("u", (1, 1, "5u")), ("i", (1, 1, "5u"))
         ], [])
    equal (run "pemade" "" [(0, 1, "4e"), (1, 1, "4u")])
        ([ ("u", (0, 1, "4e")), ("i", (0, 1, "5i"))
         , ("u", (1, 1, "4u")), ("i", (1, 1, "4u"))
         ], [])
    equal (run "pemade" " | inst-top = (pitch (4a))"
            [(0, 1, "4o"), (1, 1, "4e")])
        ([ ("u", (0, 1, "4o")), ("i", (0, 1, "4a"))
         , ("u", (1, 1, "4e")), ("i", (1, 1, "4e"))
         ], [])

aliases :: Simple.Aliases
aliases = make_aliases "pemade"

make_aliases :: Text -> [(Text, Text)]
make_aliases suffix =
    [ ("w", "kontakt/wayang-" <> suffix)
    , ("u", "kontakt/wayang-umbang"), ("i", "kontakt/wayang-isep")
    ]

wayang_title :: String
wayang_title =
    " | scale = wayang | n >w | inst-polos = >u  | inst-sangsih = >i"
