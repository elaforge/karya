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
    let run pitch notes tracks = KontaktTest.derive "" $
            [ ("*", [(0, 0, pitch)])
            , (">kontakt/mridangam",
                [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) notes])
            ] ++ tracks
        perf = KontaktTest.perform ["kontakt/mridangam"] . Derive.r_events
    let (_events, midi, logs) =
            perf $ run "3b" ["k", "t", "n", "d", "i"] []
    equal logs []
    equal (mapMaybe Midi.channel_message $ filter Midi.is_note_on $
            map snd (DeriveTest.extract_midi midi))
        [ Midi.NoteOn Key.d3 127, Midi.NoteOn Key.d4 127
        , Midi.NoteOn Key.d5 127, Midi.NoteOn Key.d6 127
        , Midi.NoteOn Key.d8 127
        ]
    -- Ensure multiple calls works.  This is already tested in
    -- "Derive.Call_test", but here's another test.
    equal (DeriveTest.extract DeriveTest.e_attributes $
            run "3b" ["do"] [("dyn", [(0, 0, ".5")])])
        (["+din", "+thom"], [])
