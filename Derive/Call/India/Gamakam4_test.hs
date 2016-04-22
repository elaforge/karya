-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Gamakam4_test where
import qualified Data.Map as Map

import qualified Util.Num as Num
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.India.Gamakam4 as Gamakam
import Derive.Call.India.Gamakam4 (Call(..), ParsedPitch(..))
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.NN as NN
import Global
import Types


test_call_maps = do
    equal (Map.keys Gamakam.pitch_call_map) (Map.keys Gamakam.pitch_call_map)
    equal (Map.keys Gamakam.dyn_call_map) (Map.keys Gamakam.dyn_call_map)

-- * pitch

test_sequence = do
    let run c = derive_tracks DeriveTest.e_nns_rounded $
            make_2notes (4, "--") (2, c)
        output nns = ([[(0, NN.c4)], nns, [(6, NN.e4)]], [])
    equal (run "!^=") (output [(4, 62)])
    equal (run "!T10") (output [(4, 63), (5, 62.5)])
    equal (run "!-1") (output [(4, NN.c4), (5, NN.c4)])
    equal (run "!Ta0") (output [(4, 60), (5, 61)])
    equal (run "!Ta=") (output [(4, 60)])

test_sequence_multiple = do
    -- Test multiple sequence calls under one note.
    let run = derive_tracks
    let note = [(">", [(0, 4, "")]), ("*", [(0, 0, "4c")])]
    -- 4c------
    -- !-1 !0  |
    -- Prev pitch is from the previous sequence call.
    equal (run DeriveTest.e_nns_rounded $
            note ++ [("* interleave", [(0, 2, "!-1"), (2, 2, "!0")])])
        ([[(0, 60), (1, 59.5), (2, 59), (3, 59.5), (4, 60)]], [])
    -- Same for dyn.
    equal (run DeriveTest.e_dyn_rounded $
            note ++ [("dyn", [(0, 2, "!<4"), (2, 0, "!>")])])
        ([[(0, 0), (1, 0.37), (2, 0.44), (3, 0.37), (4, 0)]], [])

test_parse_pitch_sequence = do
    let f = Gamakam.parse_pitch_sequence
    equal (f "P10") $ Right [CallArg 'P' "1", CallArg '0' ""]
    equal (f "p1") $ Right [CallArg 'p' "", CallArg '1' ""]
    -- '-' will include the next character so I can pass a negative digit.
    equal (f "P-10") $ Right [CallArg 'P' "-1", CallArg '0' ""]
    equal (f "p-1") $ Right [CallArg 'p' "", CallArg '-' "1"]
    equal (f "1[01]") $
        Right [CallArg '1' "", PitchGroup [CallArg '0' "", CallArg '1' ""]]

test_postfix = do
    let run = derive_tracks DeriveTest.e_nns_rounded . make_2notes (4, "--")
    strings_like (snd $ run (4, "!_==1_"))
        (replicate 2 "postfix call with no preceding call")
    equal (run (4, "!T0==1_"))
        ([[(0, 60)], [(4, 62), (5, 62), (6, 62), (7, 63)], [(8, 64)]], [])

test_resolve_postfix = do
    let f = fmap (map extract) . Gamakam.resolve_postfix . map make
        make name = Gamakam.Call
            (Gamakam.PitchCall "doc" 1 False empty, name) ""
        empty = Gamakam.PCall Sig.no_args $ \() _ctx -> return mempty
        extract (Gamakam.Call (pcall, name) _) =
            (name, Gamakam.pcall_duration pcall)
    equal (f "x_") (Right [('x', 2)])
    equal (f "x.") (Right [('x', 0.5)])
    equal (f "x__") (Right [('x', 3)])
    equal (f "x__.") (Right [('x', 1.5)])

test_prev_pitch = do
    let run = derive_tracks DeriveTest.e_nns_rounded
    -- Prev pitch comes from previous call.
    equal (run $ note_pitch_gamakam
            [(0, 2, "")]
            [(0, 0, "4c")]
            [(0, 0, "!T1="), (1, 0, "!=")])
        ([[(0, NN.cs4), (1, NN.cs4), (2, NN.cs4)]], [])
    -- Prev pitch comes from prev event.
    equal (run (make_2notes (4, "--") (2, "!-1")))
        ([[(0, NN.c4)], [(4, NN.c4), (5, NN.c4)], [(6, NN.e4)]], [])
    -- Prev pitch comes from the parent pitch track.
    equal (run $ note_pitch_gamakam
            [(0, 1, ""), (1, 2, "")]
            [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e")]
            [(0, 0, "!T1="), (2, 0, "!0")])
        -- T1 is 4c#, then set to 4d, and come from 4d to 4e.
        ([[(0, NN.cs4)], [(1, NN.d4), (2, NN.d4), (3, NN.e4)]], [])

    -- TODO
    -- equal (run $ note_pitch_gamakam
    --         [(0, 3, ""), (3, 2, "")]
    --         [(0, 0, "4d"), (1, 0, "4e"), (2, 0, "4d"),
    --             (3, 0, "4c"), (4, 0, "4d")]
    --         [(1, 0, "!="), (2, 0, "--"), (4, 0, "!=")])
    --     ([[(0, NN.d4), (1, NN.d4), (2, NN.d4)],
    --         [(3, NN.c4), (4, NN.c4), (5, NN.c4)]], [])

    -- TODO
    -- let (result, logs) = run $ note_pitch_gamakam
    --         [(0, 3, ""), (3, 1, "")]
    --         [(0, 0, "2d"), (1, 0, "4c"), (2, 0, "4d"), (4, 0, "4c")]
    --         [(0, 0, "!^20"), (1, 0, "!0="), (4, 0, "!=")]
    -- -- Last note should get 4d, but instead gets 4c.
    -- equal logs []
    -- equal result
    --     [ [(0, NN.d2), (1, NN.c4), (2, NN.d4)]
    --     , [(3, 60)]
    --     ]


test_resolve_pitch_calls = do
    let f = fmap (map (fmap extract)) . Gamakam.resolve_pitch_calls
            <=< Gamakam.parse_pitch_sequence
        extract (call, name) = (name, Gamakam.pcall_duration call)
    equal (f "10") $ Right [Call ('1', 1) "", Call ('0', 1) ""]
    equal (f "[10]") $ Right [Call ('1', 0.5) "", Call ('0', 0.5) ""]

test_note_end = do
    let run = DeriveTest.derive_tracks_setup with_tsig "import india.gamakam4"
        with_tsig = DeriveTest.with_tsig_tracknums [4]
    let result = run
            [ (">", [(0, 4, ""), (4, 4, "")])
            , ("*", [(0, 0, "4c")])
            , ("* interleave | transition=1", [(0, 0, "!1"), (8, 0, "--")])
            , ("dyn", [(0, 0, "!<")])
            ]
    -- Transition is up to 4, not 8.
    equal (DeriveTest.extract DeriveTest.e_nns_rounded result)
        ([[(0, 60), (1, 60.5), (2, 61), (3, 61.5)], [(4, 62)]], [])
    -- Track signals come out right.
    let round_vals = map $ second $ map $ second $ Num.roundDigits 2
    equal (round_vals (DeriveTest.e_tsigs result))
        [((UiTest.default_block_id, UiTest.mk_tid 4),
            [(0, 0), (1, 0.58), (2, 0.84), (3, 0.96), (4, 1)])]

test_sequence_interleave = do
    let run c = derive_tracks extract $ make_2notes (4, "--") (6, c)
        extract = DeriveTest.e_nns_rounded
    equal (run "!=") ([[(0, NN.c4)], [(4, NN.c4)], [(10, NN.e4)]], [])

test_alias = do
    let run dur g = derive_tracks DeriveTest.e_nns_rounded $
            make_tracks [(0, dur, "4c", g)]
    equal (run 2 "!0-1") (run 2 "!0a")
    equal (run 2 "!0[e0]") (run 2 "!0n")

-- * dyn

test_parse_dyn_sequence = do
    let f = Gamakam.parse_dyn_sequence
    equal (f "==") $ Right [Call '=' "", Call '=' ""]
    equal (f "<=") $ Right [Call '<' "", Call '=' ""]
    equal (f "<3=") $ Right [Call '<' "3", Call '=' ""]
    equal (f "T9>") $ Right [Call 'T' "9", Call '>' ""]

test_dyn_sequence = do
    let run call1 call2 = derive_tracks DeriveTest.e_dyn_rounded $
            make_dyn_tracks (4, call1) (4, call2)
    equal (run ".5" "!=") ([[(0, 0.5)], [(4, 0.5)], [(4, 0.5)]], [])
    equal (run ".5" "!=>")
        ([ [(0, 0.5)]
         , [(4, 0.5), (6, 0.5), (7, 0.42)]
         , [(8, 0)]
         ], [])
    equal (run ".5" "!<=")
        ([ [(0, 0.5)]
         , [(4, 0), (5, 0.84), (6, 1)]
         , [(6, 1)]
         ], [])
    equal (run ".5" "!T0=") ([[(0, 0.5)], [(6, 0)], [(6, 0)]], [])

make_dyn_tracks :: (ScoreTime, String) -> (ScoreTime, String)
    -> [UiTest.TrackSpec]
make_dyn_tracks (dur1, call1) (dur2, call2) =
    [ (">", [(0, dur1, ""), (dur1, dur2, ""), (dur1 + dur2, 2, "")])
    , ("*", [(0, 0, "4c"), (dur1, 0, "4d"), (dur1 + dur2, 0, "4e")])
    , ("dyn", [(0, dur1, call1), (dur1, dur2, call2)])
    ]

-- * util

derive_tracks :: (Score.Event -> a) -> [UiTest.TrackSpec] -> ([a], [String])
derive_tracks extract = DeriveTest.extract extract
    . DeriveTest.derive_tracks
        "import india.gamakam4 | transition=1 | dyn-transition=1"

note_pitch_gamakam :: [UiTest.EventSpec] -> [UiTest.EventSpec]
    -> [UiTest.EventSpec] -> [UiTest.TrackSpec]
note_pitch_gamakam notes pitches gamakams =
    [(">", notes), ("*", pitches), ("* interleave", gamakams)]

make_tracks :: [(ScoreTime, ScoreTime, String, String)]
    -- ^ (start, dur, pitch, gamakam)
    -> [UiTest.TrackSpec]
make_tracks notes =
    [ (">", [(start, dur, "") | (start, dur, _, _) <- notes])
    , ("*", [(start, 0, pitch) | (start, _, pitch, _) <- notes, pitch /= ""])
    , ("* interleave",
        [(start, 0, call) | (start, _, _, call) <- notes, call /= ""])
    , ("dyn", [(0, 0, "1")])
    ]

make_2notes :: (ScoreTime, String) -> (ScoreTime, String) -> [UiTest.TrackSpec]
make_2notes (dur1, call1) (dur2, call2) = make_tracks
    [ (0, dur1, "4c", call1)
    , (dur1, dur2, "4d", call2)
    , (dur1 + dur2, 2, "4e", "")
    ]
