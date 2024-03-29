-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Note_test where
import qualified Derive.Controls as Controls
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ScoreT as ScoreT

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_sub_tracks :: Test
test_sub_tracks = do
    let run = DeriveTest.derive_tracks ""
    let extract_c e =
            ( DeriveTest.e_event e
            , DeriveTest.e_control "c1" e
            , DeriveTest.e_control "c2" e
            )
    let (events, logs) = DeriveTest.extract extract_c $ run
            [ (">", [(0, 2, ""), (2, 2, "")])
            , ("c1", [(0, 0, "0"), (1, 0, "1"), (2, 0, "2")])
            , ("c2", [(0, 0, "3"), (6, 0, "4")])
            ]
    equal logs []
    equal events
        [ ((0, 2, ""), [(0, 0), (1, 0), (1, 1)], [(0, 3)])
        , ((2, 2, ""), [(2, 2)], [(0, 3), (6, 3), (6, 4)])
        ]

    let extract_p e = (DeriveTest.e_event e, DeriveTest.e_nns e)
    let (events, logs) = DeriveTest.extract extract_p $ run
            [ (">", [(0, 2, ""), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (2, 0, "4d"), (3, 0, "4e")])
            ]
    equal logs []
    equal events
        [ ((0, 2, ""), [(0, NN.c4), (2, NN.c4)])
        , ((2, 2, ""), [(2, NN.d4), (3, NN.d4), (3, NN.e4)])
        ]

    -- controls that straddle the note are properly sliced and clipped
    let (events, _logs) = DeriveTest.extract extract_p $ run
            [ (">", [(0, 2, ""), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (4, 0, "i (4d)")])
            ]
    equal events
        [ ((0, 2, ""), [(0, NN.c4), (2, NN.cs4)])
        , ((2, 2, ""), [(0, NN.c4), (4, NN.d4)])
        ]

test_derive_track_signals :: Test
test_derive_track_signals = do
    let run tracknum source =
            map (second (map (second Pitch.nn)))
            . DeriveTest.e_tsigs
            . DeriveTest.derive_tracks_setup (setup tracknum source) ""
        setup tracknum source =
            DeriveTest.with_tsig_sources [(UiTest.mk_tid tracknum, source)]
            <> DeriveTest.with_linear
        pitch = Just (Track.Pitch ScoreT.default_pitch)
    equal (run 1 pitch $ UiTest.regular_notes 4)
        [((UiTest.default_block_id, UiTest.mk_tid 1),
            [(0, 48), (1, 48), (1, 50), (2, 50), (2, 52), (3, 52), (3, 53)])]

    --    0  1  2  3
    -- t1    (- --
    -- t2 "" "" "" ""
    -- t3 c3 d3 e3 f3
    -- Make sure track signals from orphans are incorporated.
    equal (run 2 pitch $ (">", [(1, 2, "(")]) : UiTest.regular_notes 4)
        [ ( (UiTest.default_block_id, UiTest.mk_tid 2)
          , [ (0, NN.c3), (1, NN.c3), (1, NN.d3), (2, NN.d3), (2, NN.e3)
            , (3, NN.e3), (3, NN.f3)
            ]
          )
        ]

    -- This isn't a note track signal, but let's make sure normal track signals
    -- work with orphans as well.
    equal (run 3 Nothing $ (">", [(1, 2, "(")]) : UiTest.regular_notes 4)
        [((UiTest.default_block_id, UiTest.mk_tid 3),
            [(0, 48), (1, 48), (1, 50), (2, 50), (2, 52), (3, 52), (3, 53)])]

test_stash_signal :: Test
test_stash_signal = do
    let run wanted tempo tracks =
            lookup (UiTest.default_block_id, UiTest.mk_tid 2) $
            DeriveTest.e_tsigs $
            DeriveTest.derive_tracks_setup (want wanted) "" $
                ("tempo", tempo) : (">", [(0, 1, ""), (1, 1, "")]) : tracks
        want control = DeriveTest.with_ui $ \state -> UiTest.exec state $
            Ui.set_render_style (Track.Line (Just control)) (UiTest.mk_tid 2)
        draw_dyn = Track.Control Controls.dynamic
        draw_pitch = Track.Pitch ScoreT.default_pitch
    equal (run draw_dyn [(0, 0, "1")] [("dyn", [(0, 0, ".5"), (1, 0, "1")])])
        (Just [(0, 0.5), (1, 0.5), (1, 1)])
    equal (run draw_pitch [(0, 0, "1")] [("*", [(0, 0, "4c"), (1, 0, "4d")])])
        (Just $ map (second realToFrac) [(0, NN.c4), (1, NN.c4), (1, NN.d4)])
    equal (run draw_dyn [(0, 0, "1"), (1, 0, "2")]
            [("dyn", [(0, 0, ".5"), (1, 0, "1"), (2, 0, ".5")])])
        (Just [(0, 0.5), (1, 0.5), (1, 1), (2, 1), (2, 0.5)])

-- * derivers

test_c_note :: Test
test_c_note = do
    -- Test basic Derive.d_note_track plumbing and the note (null) deriver
    -- along with it.
    let run title evts = DeriveTest.extract extract $
            DeriveTest.derive_tracks "" [(title, evts)]
        extract e = (DeriveTest.e_event e, DeriveTest.e_instrument e,
            DeriveTest.e_attributes e)
    let inst = "i"

    let evt s d = ((s, d, ""), inst, "+")
    equal (run ">i" [(0, 1, ""), (1, 2, ""), (3, 3, "")])
        ([evt 0 1, evt 1 2, evt 3 3], [])

    let (evts, logs) = run ">i" [(0, 1, "notfound")]
    equal evts []
    strings_like logs ["note generator not found"]

    let (evts, logs) = run ">i" [(0, 1, "x (")]
    equal evts []
    strings_like logs ["parse error"]

    -- title error throws exception
    let (evts, logs) = run ">i $parse/err" [(0, 1, "")]
    equal evts []
    strings_like logs ["parse error"]

    -- A plain comment counts as a null call, but a comment-bar event is
    -- entirely ignored.
    equal (run ">i" [(0, 1, "--")]) ([((0, 1, "--"), "i", "+")], [])
    equal (run ">i" [(0, 1, "--|")]) ([], [])

    -- event overrides attrs
    equal (run "> | +a" [(0, 1, "+b")]) ([((0, 1, ""), "", "+a+b")], [])
    -- alternate syntax
    equal (run ">i" [(0, 1, ""), (1, 1, "inst=i2 |")])
        ( [ ((0, 1, ""), inst, "+")
          , ((1, 1, "inst=i2 |"), "i2", "+")
          ]
        , []
        )
