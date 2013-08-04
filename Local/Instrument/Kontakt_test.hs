-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt_test where
import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import Derive.Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Midi.Perform as Perform
import qualified Local.Instrument.Kontakt as Kontakt


test_kendang = do
    let e_attrs = DeriveTest.extract $ \e ->
            (Score.event_start e, DeriveTest.e_inst e, Score.event_attributes e)
        strokes ns = [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
    let wadon_inst = "kkt/kendang-wadon"
        lanang_inst = "kkt/kendang-lanang"
        kendang_inst = "kkt/kendang"
    let run_kendang suffix stroke_events = e_attrs $
            derive [('>' : untxt kendang_inst ++ suffix, strokes stroke_events)]
    equal (run_kendang "" ["PL", "k", "P", "u", "U"])
        ( [ (0, kendang_inst, wadon <> plak)
          , (1, kendang_inst, wadon <> pak)
          , (2, kendang_inst, lanang <> pak)
          , (3, kendang_inst, wadon <> tut)
          , (4, kendang_inst, lanang <> tut)
          ]
        , []
        )
    let (events, logs) = run_kendang " | realize"
            ["PL", "k", "P", "t", "T", "t", "T"]
    equal logs []
    equal [(p, attrs) | (p, inst, attrs) <- events, inst == lanang_inst]
        [(1, ka <> soft), (2, pak), (3, ka <> soft), (4, pang), (5, pak),
            (6, pang)]
    equal [(p, attrs) | (p, inst, attrs) <- events, inst == wadon_inst]
        [(0, plak), (1, pak), (2, ka <> soft), (3, pang), (4, pak), (5, pang),
            (6, pak)]

    -- Soft attributes.
    let e_vel = DeriveTest.extract $ \e ->
            (Score.event_start e, Pretty.pretty (Score.event_attributes e),
                Score.initial_dynamic e)
    equal (e_vel $ derive
            [ ('>' : untxt wadon_inst, strokes ["-", "+", ".", "P", "^"])
            , ("dyn", [(0, 0, "1")])
            ])
        ([(0, "+de+soft", 0.3), (1, "+de", 1), (2, "+ka+soft", 0.3),
            (3, "+pak", 1), (4, "+pak+soft", 0.3)], [])

test_mridangam = do
    let run pitch notes = perform "kkt/mridangam" $ Derive.r_events $ derive
            [ ("*", [(0, 0, pitch)])
            , (">kkt/mridangam",
                [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) notes])
            ]
    let right = ["k", "t", "n", "d", "d2", "m"]
    let (_events, midi, logs) = run "3b" right
    equal logs []
    equal (mapMaybe Midi.channel_message $ filter Midi.is_note_on $
            map snd (DeriveTest.extract_midi midi))
        [ Midi.NoteOn Key.d3 127, Midi.NoteOn Key.d4 127
        , Midi.NoteOn Key.d5 127, Midi.NoteOn Key.d6 127
        , Midi.NoteOn Key.d7 127, Midi.NoteOn Key.d8 127
        ]

derive :: [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_with
    (DeriveTest.with_inst_db Kontakt.synth_descs)

perform :: Text -> Derive.Events
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform inst = DeriveTest.perform_inst Kontakt.synth_descs [(inst, [0])]
