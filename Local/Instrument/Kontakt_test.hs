-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt_test where
import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Key as Key
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi

import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Midi.Perform as Perform
import qualified Local.Instrument.Kontakt as Kontakt


test_wayang = do
    let run notes = extract $ perform ["kontakt/wayang-umbang"] $
            Derive.r_events $ derive $
                UiTest.note_spec ("kontakt/wayang-umbang", notes, [])
        extract (_, midi, logs) = (DeriveTest.note_on midi, logs)
    equal (run [(0, 1, "4i")]) ([Key2.e4], [])
    equal (run [(1, 1, "+mute -- 4i")]) ([Key2.b_2, Key2.e0], [])
    equal (run [(2, 1, "+mute+loose -- 4i")]) ([Key2.a_2, Key2.e0], [])

test_wayang_pasang = do
    let run notes = derive $ UiTest.note_spec (title, notes, [])
        extract = DeriveTest.e_inst
        title = wayang_title "" <> " | unison"
    equal (DeriveTest.extract extract $ run [(0, 1, "")])
        (["kontakt/wayang-umbang", "kontakt/wayang-isep"], [])
    let result = run [(0, 1, "4i")]
    equal (DeriveTest.extract extract result)
        (["kontakt/wayang-umbang", "kontakt/wayang-isep"], [])

    let (_events, midi, logs) = perform
            ["kontakt/wayang-umbang", "kontakt/wayang-isep"]
            (Derive.r_events result)
    equal logs []
    -- Note no PitchBend, which means the split instruments applied
    -- tuning=umbang and tuning=isep to their pitches.
    equal (DeriveTest.midi_channel midi)
        [ (1, Midi.NoteOn Key.e5 127), (0, Midi.NoteOn Key.e5 127)
        , (1, Midi.NoteOff Key.e5 127), (0, Midi.NoteOff Key.e5 127)
        ]

test_wayang_kempyung = do
    let run suffix append notes = DeriveTest.extract extract $ derive $
            UiTest.note_spec
                (wayang_title suffix <> append <> " | kempyung", notes, [])
        extract e = (DeriveTest.e_inst e, DeriveTest.e_note e)
        umbang = "kontakt/wayang-umbang"
        isep = "kontakt/wayang-isep"
    -- Top note is 6i.
    equal (run "-k" "" [(0, 1, "5e"), (1, 1, "5u")])
        ([ (umbang, (0, 1, "5e")), (isep, (0, 1, "6i"))
         , (umbang, (1, 1, "5u")), (isep, (1, 1, "5u"))
         ], [])
    equal (run "-p" "" [(0, 1, "4e"), (1, 1, "4u")])
        ([ (umbang, (0, 1, "4e")), (isep, (0, 1, "5i"))
         , (umbang, (1, 1, "4u")), (isep, (1, 1, "4u"))
         ], [])
    equal (run "-p" " | inst-top = (pitch (4a))" [(0, 1, "4o"), (1, 1, "4e")])
        ([ (umbang, (0, 1, "4o")), (isep, (0, 1, "4a"))
         , (umbang, (1, 1, "4e")), (isep, (1, 1, "4e"))
         ], [])

wayang_title :: String -> String
wayang_title suffix =
    " | scale = wayang | n >kontakt/wayang" <> suffix <> " | scale = wayang\
    \ | inst-polos = >kontakt/wayang-umbang\
    \ | inst-sangsih = >kontakt/wayang-isep"

test_mridangam = do
    let run pitch notes tracks = derive $
            [ ("*", [(0, 0, pitch)])
            , (">kontakt/mridangam",
                [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) notes])
            ] ++ tracks
        perf = perform ["kontakt/mridangam"] . Derive.r_events
    let (_events, midi, logs) =
            perf $ run "3b" ["k", "t", "n", "d", "m"] []
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

test_mridangam_double = do
    let run title notes = DeriveTest.extract extract $
            derive [(">kontakt/mridangam" <> title, notes)]
        extract e = (Score.event_start e, Score.initial_dynamic e)
    -- The second one doesn't have enough space.
    equal (run " | double-time = 1s" [(1, 0, "oo"), (2, 0, "oo")])
        ([(0, 0.5), (1, 1), (1.5, 0.5), (2, 1)], [])

derive :: [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_with
    (DeriveTest.with_inst_db Kontakt.synth_descs) "import bali.kotekan"

perform :: [Text] -> Derive.Events
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform insts = DeriveTest.perform_inst Kontakt.synth_descs
    [(inst, [n]) | (n, inst) <- zip [0..] insts]
