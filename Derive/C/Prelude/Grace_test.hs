-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Grace_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Articulation as Articulation
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Perform.NN as NN
import qualified Perform.RealTime as RealTime

import Global


-- * note calls

test_mordent = do
    let run = DeriveTest.extract DeriveTest.e_pitch . run_note
    equal (run (1, 1, "`mordent`")) (["4c", "4d", "4c"], [])
    equal (run (1, 1, "`rmordent`")) (["4c", "3b", "4c"], [])

run_note :: UiTest.EventSpec -> Derive.Result
run_note note = derive_tracks [(">", [note]), ("*", [(0, 0, "4c")])]

test_grace = do
    let run extract = DeriveTest.extract extract . derive_tracks
        tracks notes = [(">", notes), ("*", [(0, 0, "4c")])]
        prefix = "legato-detach = 0 | %legato-overlap = 0 | grace-dur = 1 |"

    let legato_tracks note = tracks [(2, 1,
            "legato-detach = .25 | grace-dur = 1 | %legato-overlap = .5 | "
                <> note)]
        dur = 1
        overlap = 0.5
        detach = 0.25
        e_dyn e = (DeriveTest.e_note e, Score.initial_dynamic e)
    equal (run e_dyn $ legato_tracks "grace-dyn = .5 | g (4a) (4b)")
        ( [ ((2-dur*2, dur+overlap, "4a"), 0.5)
          , ((2-dur, dur+overlap, "4b"), 0.5)
          , ((2, dur-detach, "4c"), 1)
          ]
        , []
        )

    -- Ensure the grace-dyn default is picked up too.
    equal (run e_dyn $ legato_tracks "grace-dyn = 1 | g (4b)")
        ([((2-dur, dur+overlap, "4b"), 1), ((2, dur-detach, "4c"), 1)], [])

    let run_n = run DeriveTest.e_note
    equal (run_n $ tracks [(0, 2, ""), (2, 2, prefix <> "g (4a) (4b) (4d)")])
        ([(0, 2, "4c"), (0.5, 0.5, "4a"), (1, 0.5, "4b"), (1.5, 0.5, "4d"),
            (2, 2, "4c")], [])

    -- grace-dur defaults to RealTime, but can be ScoreTime.
    let tempo_tracks note = ("tempo", [(0, 0, "2")])
            : tracks [(4, 2, "%legato-overlap = 0 | " <> note)]
    equal (run_n $ tempo_tracks "grace-dur = 1 | g (4b)")
        ([(1, 1, "4b"), (2, 1, "4c")], [])
    equal (run_n $ tempo_tracks "grace-dur = 1t | g (4b)")
        ([(1.5, 0.5, "4b"), (2, 1, "4c")], [])

    -- grace-place
    let place_tracks note = tracks [(2, 2, prefix <> note)]
    equal (run_n $ place_tracks "%place = 1 | g (4b)")
        ([(2, 1, "4b"), (3, 1, "4c")], [])
    -- Grace notes shorten if the note can't accomodate them all.
    equal (run_n $ place_tracks "%place = 1 | g (4a) (4b) (4d)")
        ( [ (2, 0.5, "4a"), (2.5, 0.5, "4b")
          , (3, 0.5, "4d"), (3.5, 0.5, "4c")
          ]
        , []
        )
    equal (run_n $ place_tracks "%place = .5 | g (4b)")
        ([(1.5, 1, "4b"), (2.5, 1.5, "4c")], [])

    -- Ensure grace works with attr slur.
    let run_a = DeriveTest.extract DeriveTest.e_attributes
            . DeriveTest.derive_tracks_setup setup ""
        setup = CallTest.with_note_generator "("
            (Articulation.c_attr_slur Attrs.pizz Attrs.legato)
    equal (run_a $ tracks [(0, 1, "g (4a) (4b)")])
        (["+pizz", "+legato", "+legato"], [])

test_grace_transpose = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks "grace-dur=1 | %legato-overlap=0"
            . UiTest.note_track
    -- default to diatonic
    equal (run [(0, 2, "g 2 1 -- 4c")])
        ([(-2, 1, "4e"), (-1, 1, "4d"), (0, 2, "4c")], [])
    -- otherwise, the first one sets the default
    equal (run [(0, 2, "g 2c 1 -- 4c")])
        ([(-2, 1, "4d"), (-1, 1, "4c#"), (0, 2, "4c")], [])
    strings_like (snd $ run [(0, 2, "g 2c 1d -- 4c")])
        ["arguments should all have the same type"]

test_grace_hold = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks "grace-dur=1"
            . UiTest.note_track
    equal (run [(0, 2, "g- 2 1 -- 4c")])
        ([(-2, 4, "4e"), (-1, 3, "4d"), (0, 2, "4c")], [])

test_grace_pitch = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks "grace-dur=1"
        extract e = (DeriveTest.e_start_dur e, DeriveTest.e_nns e)
    equal (run $ UiTest.note_track [(0, 2, "g_ 2 1 -- 4c")])
        ( [((-2, 4),
            [ (-2, NN.e4), (-1, NN.e4), (-1, NN.d4)
            , (0, NN.d4), (0, NN.c4)
            ])]
        , []
        )
    -- Don't destroy any later pitch curve.
    equal (run [(">", [(0, 2, "g_ 1")]), ("*", [(0, 0, "4c"), (1, 0, "3c")])])
        ( [((-1, 3),
            [(-1, NN.d4), (0, NN.d4), (0, NN.c4), (1, NN.c4), (1, NN.c3)])]
        , []
        )

test_basic_grace = do
    let run = DeriveTest.extract extract . derive_tracks . UiTest.note_track
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    equal (run [(0, 2, "grace  (list 1) 1s 1 \"(+a) -- 4c")])
        ([((0, 1, "4d"), "+a"), ((1, 1, "4c"), "+")], [])

test_grace_args = do
    let run = DeriveTest.extract DeriveTest.e_pitch . derive_tracks
            . UiTest.note_track
    -- Defaults to diatonic.
    equal (run [(1, 1, "g 2 1 -- 4c")]) (["4e", "4d", "4c"], [])
    -- The chromatic propagates.
    equal (run [(1, 1, "g 2c 1 -- 4c")]) (["4d", "4c#", "4c"], [])
    -- Can't mix types.
    strings_like (snd (run [(1, 1, "g 2 1c -- 4c")]))
        ["arguments should all have the same type"]

test_grace_ly = do
    let run = LilypondTest.derive_measures ["acciaccatura"]
    equal (run
        [ (">", [(0, 2, "g (4a) (4b)"), (2, 2, "g (3a)")])
        , ("*", [(0, 0, "4c"), (2, 0, "4b")])
        ])
        (Right "\\acciaccatura { a'8[ b'8] } c'2 \\acciaccatura { a8 } b'2", [])
    equal (run
        [ ("> | v = 1", [(0, 4, "")])
        , ("*", [(0, 0, "3c")])
        , ("> | v = 2", [(0, 4, "g (3e)")])
        , ("*", [(0, 0, "3d")])
        ])
        (Right "<< { VoiceOne: c1 } { VoiceTwo: \\acciaccatura { e8 } d1 } >>",
            [])

test_roll = do
    let run extract = DeriveTest.extract extract . derive_tracks
    let tracks call = [(">", [(2, 1, call)]), ("*", [(2, 0, "4c")])]
    equal (run DeriveTest.e_note $ tracks "roll 1 .5")
        ([(1.5, 0.5, "4c"), (2, 1, "4c")], [])
    equal (run DeriveTest.e_note $ tracks "roll 2 .5")
        ([(1, 0.5, "4c"), (1.5, 0.5, "4c"), (2, 1, "4c")], [])
    -- The final note should not lose whatever pitch it has.
    let extract e = (Score.event_start e, DeriveTest.e_nns e)
    equal (run extract
            [(">", [(0, 2, "roll 1 .5")]), ("*", [(0, 0, "4c"), (1, 0, "4d")])])
        ( [ (-0.5, [(-RealTime.larger, NN.c4)])
          , (0, [(0, NN.c4), (1, NN.c4), (1, NN.d4)])
          ]
        , []
        )

-- * pitch calls

test_grace_p = do
    let run = CallTest.run_pitch ""
    equal (run [(0, "grace-dur = 2 | g (4c) -2 -1"), (10, "--|")])
        [(0, NN.a3), (2, NN.a3), (2, NN.b3), (4, NN.b3), (4, NN.c4)]
    equal (run [(0, "grace-dur = 2 | g (4c) -2c -1"), (3, "--|")])
        [(0, NN.as3), (1, NN.as3), (1, NN.b3), (2, NN.b3), (2, NN.c4)]

test_mordent_p = do
    let run = CallTest.run_pitch ""
    equal (run [(0, "dur=2 | `mordent` (4c)")])
        [(0, NN.c4), (2, NN.c4), (2, NN.d4), (4, NN.d4), (4, NN.c4)]

-- * misc

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = DeriveTest.derive_tracks ""
