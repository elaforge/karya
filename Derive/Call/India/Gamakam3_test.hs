-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Gamakam3_test where
import qualified Data.Map as Map

import qualified Util.Num as Num
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.India.Gamakam3 as Gamakam
import Derive.Call.India.Gamakam3 (Expr_(..))
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import Global
import Types


-- gamakam3.a

test_relative = do
    let run c = derive_tracks2 DeriveTest.e_nns_rounded $
            make_2notes (4, "--") (2, c)
        output nns = ([[(0, 60)], nns, [(6, 64)]], [])
    -- equal (run "!T0") (output [(4, 62), (5, 62)]) -- TODO
    equal (run "!T10") (output [(4, 63), (5, 62.5)])
    -- equal (run "!-1") (output [(4, 62), (5, 61)]) -- TODO
    equal (run "!Ta0") (output [(4, 60), (5, 61)])
    equal (run "!Ta=") (output [(4, 60)])

test_parse_sequence2 = do
    let f = Gamakam.parse_sequence2
    equal (f "P10") $ Right [PitchExpr 'P' "1", PitchExpr '0' ""]
    equal (f "p1") $ Right [PitchExpr 'p' "", PitchExpr '1' ""]
    -- '-' will include the next character so I can pass a negative digit.
    equal (f "P-10") $ Right [PitchExpr 'P' "-1", PitchExpr '0' ""]
    equal (f "p-1") $ Right [PitchExpr 'p' "", PitchExpr '-' "1"]

    equal (f "1[01]") $
        Right [PitchExpr '1' "", Group [PitchExpr '0' "", PitchExpr '1' ""]]

test_extension2 = do
    let run = derive_tracks2 DeriveTest.e_nns_rounded . make_2notes (4, "--")
    strings_like (snd $ run (4, "!_==1_"))
        (replicate 2 "extension with no preceding call")
    -- TODO
    -- equal (run (4, "!==1_"))
    --     ([[(0, 60)], [(4, 62), (5, 62), (6, 62), (7, 63)], [(8, 64)]], [])

test_prev_pitch = do
    let run = derive_tracks2 DeriveTest.e_nns_rounded . make_tracks
    pprint $ make_tracks
        [ (0, 1, "4c", "!="), (1, 1, "4d", "")
        , (2, 1, "4e", ""), (3, 1, "4f", "!=")
        ]
    pprint (run
        [ (0, 1, "4c", "!="), (1, 1, "4d", "")
        , (2, 1, "4e", ""), (3, 1, "4f", "!=")
        ])
    -- pprint (run [(0, 1, "4c", "!="), (1, 1, "4d", ""), (2, 1, "4e", "!=")])

test_call_maps = do
    equal (Map.keys Gamakam.pitch_call_map) (Map.keys Gamakam.pitch_call_map)
    equal (Map.keys Gamakam.pitch_call_map2) (Map.keys Gamakam.pitch_call_map2)
    equal (Map.keys Gamakam.dyn_call_map) (Map.keys Gamakam.dyn_call_map)

test_resolve_exprs = do
    let f = fmap (map extract) . Gamakam.resolve_exprs True
            <=< Gamakam.parse_sequence2
        extract expr = case expr of
            PitchExpr call text -> PitchExpr (extract_call call) text
            Group exprs -> Group (map extract exprs)
            DynExpr _ arg1 arg2 exprs ->
                DynExpr () arg1 arg2 (map extract exprs)
        extract_call call =
            (Gamakam.pcall_name call, Gamakam.pcall_duration call)
    equal (f "10") $ Right [PitchExpr ('1', 1) "", PitchExpr ('0', 1) ""]
    equal (f "[10]") $ Right [PitchExpr ('1', 0.5) "", PitchExpr ('0', 0.5) ""]

-- * pitch

test_parse_sequence = do
    let f = Gamakam.parse_sequence
    equal (f " [-]> ") $ Right [DynExpr ">" "" "" [PitchExpr '-' ""]]
    equal (f "!012 -4") $ Right
        [ PitchExpr '0' "", PitchExpr '1' "", PitchExpr '2' ""
        , PitchExpr '-' "4"
        ]
    equal (f "a b1  c") $ Right
        [PitchExpr 'a' "", PitchExpr 'b' "1", PitchExpr 'c' ""]
    equal (f "!ab c") $ Right
        [PitchExpr 'a' "", PitchExpr 'b' "", PitchExpr 'c' ""]

    equal (f "[a]1>.5") $ Right [DynExpr ">" "1" ".5" [PitchExpr 'a' ""]]
    equal (f "[a]>") $ Right [DynExpr ">" "" "" [PitchExpr 'a' ""]]
    equal (f "[a]>[b]<") $ Right
        [ DynExpr ">" "" "" [PitchExpr 'a' ""]
        , DynExpr "<" "" "" [PitchExpr 'b' ""]
        ]
    equal (f "[a[b]>]<") $ Right
        [DynExpr "<" "" ""
            [PitchExpr 'a' "", DynExpr ">" "" "" [PitchExpr 'b' ""]]]
    -- Word notation vs. compact notation.
    equal (f "[a1]>") $ Right [DynExpr ">" "" "" [PitchExpr 'a' "1"]]
    equal (f "![a1]>") $ Right
        [DynExpr ">" "" "" [PitchExpr 'a' "", PitchExpr '1' ""]]
    equal (f "!ab[cd]>") $ Right
        [ PitchExpr 'a' "", PitchExpr 'b' ""
        , DynExpr ">" "" "" [PitchExpr 'c' "", PitchExpr 'd' ""]
        ]

    left_like (f "ab [c") "parse error"
    left_like (f "!ab[c") "parse error"
    left_like (f "ab c]") "parse error"

test_sequence = do
    let run c = derive_tracks DeriveTest.e_nns_rounded $
            make_2notes (4, "--") (6, c)
        output nns = ([[(0, 60)], nns, [(10, 64)]], [])

    -- 4 5 6 7 8 9 10
    -- ------++++++
    equal (run "!!-") (output [(4, 62)])
    -- The error shows up twice because of slicing.
    strings_like (snd $ run "!0nn")
        ["too many arguments: nn", "too many arguments: nn"]

    -- transition=1 takes all the time, and winds up being linear.
    equal (run "transition=1 | !!01")
        (output [(4, 62), (7, 62), (8, 62.67), (9, 63.33)])
    -- Fastest transition.
    equal (run "transition=0 | !!01")
        (output [(4, 62), (7, 62), (8, 62.13), (9, 63.87)])

    -- 4 5 6 7 8 9 10
    -- ----++++----
    equal (run "!!010") (output [(4, 62), (6, 62), (7, 63), (8, 64)])
    equal (run "!!0a0") (output [(4, 62), (6, 62), (7, 61), (8, 60)])

    -- move_absolute
    -- Move to next pitch.
    equal (run "!!-v-") (output [(4, 62), (6, 62), (7, 63), (8, 64)])

    -- Prev to current.
    equal (run "!!<-c-") (output [(4, 60), (6, 60), (7, 61), (8, 62)])

    -- +1 to current.
    equal (run "! P1c !-c-") (output [(4, 63), (6, 63), (7, 62.5), (8, 62)])
    -- Current to -1nn.
    equal (run "!!-y-") (output [(4, 62), (6, 62), (7, 61.5), (8, 61)])

test_move_absolute = do
    let run c dur = derive_tracks DeriveTest.e_nns_rounded $
            [ (">", [(0, dur, "")])
            , ("*", [(0, 0, "4c")])
            , ("* interleave", [(0, 0, c)])
            ]
    equal (run "!!sr" 2) ([[(0, NN.c4), (1, NN.c4), (2, NN.d4)]], [])
    equal (run "!!sn" 2) ([[(0, NN.c4), (1, NN.c4), (2, NN.b3)]], [])
    equal (run "!!smnr" 4)
        ([[(0, NN.c4), (1, NN.c4), (2, NN.f4), (3, NN.b4), (4, NN.d5)]], [])

test_default_args = do
    let run c = derive_tracks DeriveTest.e_dyn_rounded $
            make_dyn_tracks (4, "1") (4, c)
    equal (run "!=>-")
        ([[(0, 1)], [(4, 1), (5, 0.75), (6, 0.5)], [(6, 0.5)]], [])
    equal (run "'=>-from' = .5 | '=>-to' = 0 | !=>-")
        ([[(0, 1)], [(4, 0.5), (5, 0.25), (6, 0)], [(6, 0)]], [])

test_sequence_multiple = do
    -- Test multiple sequence calls under one note.
    let run = derive_tracks
    let note = [(">", [(0, 4, "")]), ("*", [(0, 0, "4c")])]
    -- Prev pitch is from the previous sequence call.
    equal (run DeriveTest.e_nns_rounded $
            note ++ [("* interleave", [(0, 2, "!a"), (2, 2, "!!<c")])])
        ([[(0, 60), (1, 59.5), (2, 59), (3, 59.5), (4, 60)]], [])
    -- Same for dyn.
    equal (run DeriveTest.e_dyn_rounded $
            note ++ [("dyn", [(0, 2, "!<.5"), (2, 0, "!>")])])
        ([[(0, 0), (1, 0.25), (2, 0.5), (3, 0.25), (4, 0)]], [])

test_extension = do
    let run = derive_tracks DeriveTest.e_nns_rounded . make_2notes (4, "--")
    strings_like (snd $ run (4, "!_ - - 1 _"))
        ["extension with no preceding call", "no preceding call"]
    equal (run (4, "!- - 1 _"))
        ([[(0, 60)], [(4, 62), (5, 62), (6, 62), (7, 63)], [(8, 64)]], [])

test_note_end = do
    let run = DeriveTest.derive_tracks_setup with_tsig "import india.gamakam3"
        with_tsig = DeriveTest.with_tsig_tracknums [4]
    let result = run
            [ (">", [(0, 4, ""), (4, 4, "")])
            , ("*", [(0, 0, "4c")])
            , ("* interleave | transition=1", [(0, 0, "!1"), (8, 0, "--")])
            , ("dyn", [(0, 0, "!0<1")])
            ]
    -- Transition is up to 4, not 8.
    equal (DeriveTest.extract DeriveTest.e_nns_rounded result)
        ([[(0, 60), (1, 60.5), (2, 61), (3, 61.5)], [(4, 62)]], [])
    -- Track signals come out right.
    let round_vals = map $ second $ map $ second $ Num.roundDigits 2
    equal (round_vals (DeriveTest.e_tsigs result))
        [((UiTest.default_block_id, UiTest.mk_tid 4),
            [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)])]

test_dyn = do
    let run c = derive_tracks DeriveTest.e_dyn_rounded $
            make_2notes (4, "--") (6, c)
    equal (run "!-") ([[(0, 1)], [(0, 1)], [(0, 1)]], [])
    equal (run "!-[-]> -")
        ([[(0, 1)], [(4, 1), (6, 1), (7, 0.5), (8, 0)], [(0, 1)]], [])

    -- 4 5 6 7 8 9 10
    -- ------++++++
    -- Dyn is as long as the call it modifies.
    equal (run "!-[-]0<")
        ([[(0, 1)], [(4, 0), (7, 0), (8, 0.33), (9, 0.67)], [(0, 1)]], [])
    -- Disabled because -- is now a comment.
    -- equal (run "!![--]0<")
    --     ([ [(0, 1)]
    --      , [(4, 0), (5, 0.17), (6, 0.33), (7, 0.5), (8, 0.67), (9, 0.83)]
    --      , [(0, 1)]], [])
    -- -- Fast and biased to the left.
    -- equal (run "!![--]0<^")
    --     ([ [(0, 1)]
    --      , [(4, 0), (5, 0.51), (6, 0.73), (7, 0.87), (8, 0.95), (9, 0.99)]
    --      , [(0, 1)]], [])
    -- Continue from the previous dyn.
    equal (run "![-]0<.5 [-]>")
        ([ [(0, 1)]
         , [(4, 0), (5, 0.17), (6, 0.33), (7, 0.5), (8, 0.33), (9, 0.17)]
         , [(0, 1)]], [])

test_dyn_prev = do
    let run call1 = derive_tracks DeriveTest.e_dyn_rounded . make_2notes call1
    equal (run (2, "![-]>.5") (2, "![-]=1"))
        ([[(0, 1), (1, 0.75)], [(2, 0.75), (3, 0.88)], [(0, 1)]], [])

test_sequence_interleave = do
    let run c = derive_tracks extract $ make_2notes (4, "--") (6, c)
        extract = DeriveTest.e_nns_rounded
    equal (run "!!0") ([[(0, 60)], [(4, 62)], [(10, 64)]], [])

    -- pprint (run "! P1c !-c-")
    -- pprint (run "! P2 0 -2")

make_tracks :: [(ScoreTime, ScoreTime, Text, Text)] -> [UiTest.TrackSpec]
make_tracks notes =
    [ (">", [(start, dur, "") | (start, dur, _, _) <- notes])
    , ("*", [(start, 0, pitch) | (start, _, pitch, _) <- notes, pitch /= ""])
    , ("* interleave",
        [(start, 0, call) | (start, _, _, call) <- notes, call /= ""])
    , ("dyn", [(0, 0, "1")])
    ]

make_2notes :: (ScoreTime, Text) -> (ScoreTime, Text) -> [UiTest.TrackSpec]
make_2notes (dur1, call1) (dur2, call2) = make_tracks
    [ (0, dur1, "4c", call1)
    , (dur1, dur2, "4d", call2)
    , (dur1 + dur2, 2, "4e", "")
    ]

-- * dyn

test_dyn_sequence = do
    let run call1 call2 = derive_tracks DeriveTest.e_dyn_rounded $
            make_dyn_tracks (4, call1) (4, call2)
    equal (run ".5" "!-") ([[(0, 0.5)], [(4, 0.5)], [(4, 0.5)]], [])
    equal (run ".5" "!->")
        ([ [(0, 0.5)]
         , [(4, 0.5), (6, 0.5), (7, 0.25)]
         , [(8, 0)]
         ], [])
    equal (run ".5" "!<-")
        ([ [(0, 0.5)]
         , [(4, 0.5), (5, 0.75), (6, 1)]
         , [(6, 1)]
         ], [])
    equal (run ".5" "!=>-")
        ([[(0, 0.5)], [(4, 1), (5, 0.75), (6, 0.5)], [(6, 0.5)]], [])

make_dyn_tracks :: (ScoreTime, Text) -> (ScoreTime, Text)
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
        "import india.gamakam3 | transition=1 | dyn-transition=1"

derive_tracks2 :: (Score.Event -> a) -> [UiTest.TrackSpec] -> ([a], [String])
derive_tracks2 extract = DeriveTest.extract extract
    . DeriveTest.derive_tracks
        "import india.gamakam3.a | transition=1 | dyn-transition=1"
