module Local.Instrument.Kontakt.Mridangam_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Local.Instrument.Kontakt.KontaktTest as KontaktTest
import Global


test_mridangam = do
    let run pitch = KontaktTest.derive aliases ("# = (" <> pitch <> ")")
            . map (((,) ">m") . notes)
        notes ns = [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
        perf = KontaktTest.perform aliases . Derive.r_events
        aliases = [("m", "kontakt/mridangam-d")]

    let (_events, midi, logs) = perf $ run "3g#" [["k", "t", "n", "d", "i"]]
    equal logs []
    equal (mapMaybe Midi.channel_message $ filter Midi.is_note_on $
            DeriveTest.extract_midi_msg midi)
        [ Midi.NoteOn Key.c2 127, Midi.NoteOn Key.c3 127
        , Midi.NoteOn Key.c4 127, Midi.NoteOn Key.c5 127
        , Midi.NoteOn Key.c7 127
        ]

    -- Ensure multiple calls works.  This is already tested in
    -- "Derive.Call_test", but here's another test.
    equal (DeriveTest.extract DeriveTest.e_attributes $ run "3b" [["do"]])
        (["+din", "+thom"], [])

    -- I get ControlChange.
    let (_events, midi, logs) = perf $ run "3g#" [["n", "d"], ["o/"]]
    equal logs []
    let extract = mapMaybe Midi.channel_message . DeriveTest.extract_midi_msg
    equal (extract midi)
        [ Midi.ControlChange 102 2, Midi.NoteOn Key.c1 127
        , Midi.NoteOn Key.c4 127
        , Midi.NoteOff Key.c1 127, Midi.NoteOff Key.c4 127
        , Midi.NoteOn Key.c5 127, Midi.NoteOff Key.c5 127
        ]
