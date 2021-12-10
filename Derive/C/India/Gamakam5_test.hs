-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.India.Gamakam5_test where
import qualified Data.Map as Map

import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.C.India.Gamakam5 as Gamakam
import Derive.C.India.Gamakam5 (Call(..), ParsedPitch(..))
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Global
import Types


test_call_maps :: Test
test_call_maps = do
    -- Make sure they resolve.
    equal (Map.keys Gamakam.pitch_call_map) (Map.keys Gamakam.pitch_call_map)
    equal (Map.keys Gamakam.dyn_call_map) (Map.keys Gamakam.dyn_call_map)

-- * pitch

test_sequence :: Test
test_sequence = do
    let run c = derive_tracks False DeriveTest.e_nns_rounded $
            make_2notes (4, "--|") (2, c)
        output nns = ([[(0, NN.c4), (4, NN.c4)], nns, [(6, NN.e4)]], [])
    putStrLn $ untxt $ UiTest.fmt_tracks $ make_2notes (4, "--|") (2, "X")

    equal (run "!^=") (output [(4, NN.d4), (6, NN.d4)])
    equal (run "!T10") (output [(4, 63), (5, 62.5), (6, 62)])
    equal (run "!-1") (output [(4, NN.c4), (6, NN.c4)])
    equal (run "!Ta0") (output [(4, 60), (5, 61), (6, 62)])
    equal (run "!Ta=") (output [(4, NN.c4), (6, NN.c4)])

-- TODO this is just test_prev_pitch
-- test_sequence_multiple = do
--     -- Test multiple sequence calls under one note.
--     let run = derive_tracks False
--     let note = [(">", [(0, 4, "")]), ("*", [(0, 0, "4c")])]
--     -- 4c------
--     -- !-1 !0  |
--     -- Prev pitch is from the previous sequence call.
--     equal (run DeriveTest.e_nns $
--             note ++ [("t-nn | gamak", [(0, 2, "!-1"), (2, 2, "!0")])])
--         ([[(0, 60), (1, 59.5), (2, 59), (3, 59.5), (4, 60)]], [])
--     -- Same for dyn.
--     equal (run DeriveTest.e_dyn_literal $
--             note ++ [("dyn | dyn", [(0, 2, "!<4"), (2, 0, "!>")])])
--         ([[(0, 0), (1, 0.37), (2, 0.44), (3, 0.37), (4, 0)]], [])

test_parse_pitch_sequence :: Test
test_parse_pitch_sequence = do
    let f = Gamakam.parse_pitch_sequence
    equal (f "P10") $ Right [CallArg 'P' "1", CallArg '0' ""]
    equal (f "p1") $ Right [CallArg 'p' "", CallArg '1' ""]
    -- '-' will include the next character so I can pass a negative digit.
    equal (f "P-10") $ Right [CallArg 'P' "-1", CallArg '0' ""]
    equal (f "p-1") $ Right [CallArg 'p' "", CallArg '-' "1"]
    equal (f "1[01]") $
        Right [CallArg '1' "", PitchGroup [CallArg '0' "", CallArg '1' ""]]

test_postfix :: Test
test_postfix = do
    let run = derive_tracks False DeriveTest.e_nns_rounded
            . make_2notes (4, "--|")
    strings_like (snd $ run (4, "!_==1_"))
        (replicate 2 "postfix call with no preceding call")
    equal (run (4, "!T0==1_"))
        ( [ [(0, 60), (4, 60)]
          , [(4, 62), (5, 62), (6, 62), (6, 62), (7, 63), (8, 64)]
          , [(8, 64)]
          ]
        , []
        )

test_resolve_postfix :: Test
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

test_prev_pitch_above :: Test
test_prev_pitch_above = do
    let run = from_cur_prev_next
    equal (run [(0, "4c"), (1, "4d")] [(0, 1), (1, 1)])
        ( [NN.c4, NN.d4, NN.c4, NN.d4]
        , []
        )
    equal (run [(0, "4c"), (1, "4d")] [(0, 2)])
        ( [NN.c4, NN.d4, NN.c4, NN.d4]
        , []
        )
    equal (run [(0, "4c"), (1, "4d"), (2, "4e")] [(0, 1), (1, 2)])
        ( [NN.c4, NN.d4, NN.c4, NN.e4]
        , []
        )

from_cur_prev_next :: [(ScoreTime, Text)] -> [(ScoreTime, ScoreTime)]
    -> ([Pitch.NoteNumber], [Text])
from_cur_prev_next pitches notes = (nns, concat logs)
    where
    nns = map (last . map snd . filter ((==1) . fst) . concat) sigs
    (sigs, logs) = unzip $ map run_with ["!=", "!^=", "!<=", "!&="]
    run_with call = derive_tracks True DeriveTest.e_nns $
        pitch_gamakam_note pitches [(1, call)] notes

test_sequence_above :: Test
test_sequence_above = do
    let run pitches gamakams notes =
            derive_tracks True e_clipped_nns $
                pitch_gamakam_note pitches gamakams notes
    -- -- Final event goes only to block end.
    -- equal (first (map (map fst)) $
    --         run [(0, "4c"), (1, "4d")] [(1, "!0")] [(0, 2)])
    --     ([Seq.range 0 32 1], [])

    -- Zero after note.
    equal (first (drop 1) $
            run [(0, "4c"), (1, "4d"), (2, "4e")] [(0, "!b")] [(0, 1), (1, 1)])
        ([[(1, NN.d4), (2, NN.d4), (2, NN.e4)]], [])
    -- But not if there is a next gamakam.
    equal (first (drop 1) $
            run [(0, "4c"), (1, "4d"), (2, "4e")] [(0, "!a"), (1, "!0")]
                [(0, 1), (1, 1)])
        ([[(1, NN.b3), (2, NN.d4), (2, NN.e4)]], [])

    -- equal (run [(0, "4c"), (1, "4d")] [(0, "!a"), (1, "!0")] [(0, 2)])
    --     ([[(1, NN.b3), (2, NN.c4), (2, NN.d4)]], [])

e_clipped_nns :: Score.Event -> [(RealTime, Pitch.NoteNumber)]
e_clipped_nns e =
    DeriveTest.e_nns_rounded $
        e { Score.event_pitch = clip (Score.event_pitch e) }
    where clip = PSignal.clip_before (Score.event_start e)

-- get_state :: Derive.Generator Derive.Control
-- get_state = CallTest.generator $ \args -> do
--     state <- Derive.require "initial_pitch_state" $
--         Gamakam.initial_pitch_state 0 args
--     Call.placed_note args

-- broken by 'gamakam: set t-nn to 0 when there isn't a following gamakam'
-- But I don't care much.
-- test_prev_pitch2 = do
--     let run ns ps gs = derive_tracks False DeriveTest.e_nns $
--             note_pitch_gamakam0 ns ps gs
--     equal (run [(0, 1), (1, 1)] [(0, "4c"), (1, "4d")] [(1, "!=")])
--         ([[(0, NN.c4), (1, NN.c4)], [(1, NN.c4)]], [])
--     -- broken, but should work
--     -- equal (run [(0, 2)] [(0, "4c"), (1, "4d")] [(1, "!=")])
--     --     ([[(0, NN.c4), (1, NN.c4)], [(1, NN.c4)]], [])

-- test_prev_pitch = do
--     let run = derive_tracks False DeriveTest.e_nns
--     -- Prev pitch comes from previous call.
--     equal (run $ note_pitch_gamakam
--             [(0, 2, "")]
--             [(0, 0, "4c")]
--             [(0, 0, "!T1="), (1, 0, "!=")])
--         ([[(0, NN.cs4), (1, NN.cs4)]], [])
--     -- Prev pitch comes from prev event.
--     equal (run (make_2notes (4, "--|") (2, "!-1")))
--         ([[(0, NN.c4), (4, NN.c4)], [(4, NN.c4), (6, NN.c4)], [(6, NN.e4)]], [])
--     -- Prev pitch comes from the parent pitch track.
--     equal (run $ note_pitch_gamakam
--             [(0, 1, ""), (1, 2, "")]
--             [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4e")]
--             [(0, 0, "!T1="), (2, 0, "!0")])
--         -- T1 is 4c#, then set to 4d, and come from 4d to 4e.
--         ( [ [(0, NN.cs4), (1, NN.cs4)]
--           , [(1, NN.d4), (2, NN.d4), (2, NN.e4), (3, NN.e4)]
--           ]
--         , []
--         )

    -- TODO
    -- equal (run $ note_pitch_gamakam
    --         [(0, 3, ""), (3, 2, "")]
    --         [(0, 0, "4d"), (1, 0, "4e"), (2, 0, "4d"),
    --             (3, 0, "4c"), (4, 0, "4d")]
    --         [(1, 0, "!="), (2, 0, "--|"), (4, 0, "!=")])
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

test_resolve_pitch_calls :: Test
test_resolve_pitch_calls = do
    let f = fmap (map (fmap extract)) . Gamakam.resolve_pitch_calls
            <=< Gamakam.parse_pitch_sequence
        extract (call, name) = (name, Gamakam.pcall_duration call)
    equal (f "10") $ Right [Call ('1', 1) "", Call ('0', 1) ""]
    equal (f "[10]") $ Right [Call ('1', 0.5) "", Call ('0', 0.5) ""]

-- broken by 'gamakam: set t-nn to 0 when there isn't a following gamakam'
-- But I don't care much.
-- test_note_end = do
--     let run = DeriveTest.derive_tracks_setup with_tsig "import india.gamakam5"
--         with_tsig = DeriveTest.with_tsig_tracknums [4]
--     let result = run
--             [ (">", [(0, 4, ""), (4, 4, "")])
--             , ("*", [(0, 0, "4c")])
--             , ("t-nn | gamak | transition=1", [(0, 0, "!1"), (8, 0, "--|")])
--             , ("dyn | dyn", [(0, 0, "!<")])
--             ]
--     -- Transition is up to 4, not 8, because the note ends at 4.
--     equal (DeriveTest.extract DeriveTest.e_nns result)
--         ( [ [(0, 60), (1, 60.5), (2, 61), (3, 61.5), (4, 62)]
--           , [(0, 60), (4, 60), (4, 62)]
--           ]
--         , []
--         )
--     -- Track signals come out right.
--     let round_vals = map $ second $ map $ second $ Num.roundDigits 2
--     equal (round_vals (DeriveTest.e_tsigs result))
--         [((UiTest.default_block_id, UiTest.mk_tid 4),
--             [(0, 0), (1, 0.58), (2, 0.84), (3, 0.96), (4, 1)])]

test_sequence_interleave :: Test
test_sequence_interleave = do
    let run c = derive_tracks False extract $ make_2notes (4, "--|") (6, c)
        extract = DeriveTest.e_nns
    equal (run "!=")
        ( [[(0, NN.c4), (4, NN.c4)], [(4, NN.c4), (10, NN.c4)], [(10, NN.e4)]]
        , []
        )

test_alias :: Test
test_alias = do
    let run dur g = derive_tracks False DeriveTest.e_nns $
            make_tracks [(0, dur, "4c", g)]
    equal (run 2 "!0-1") (run 2 "!0a")
    equal (run 2 "!0[e0]") (run 2 "!0n")

-- * dyn

test_parse_dyn_sequence :: Test
test_parse_dyn_sequence = do
    let f = Gamakam.parse_dyn_sequence
    equal (f "==") $ Right [Call '=' "", Call '=' ""]
    equal (f "<=") $ Right [Call '<' "", Call '=' ""]
    equal (f "<3=") $ Right [Call '<' "3", Call '=' ""]
    equal (f "T9>") $ Right [Call 'T' "9", Call '>' ""]

test_dyn_sequence :: Test
test_dyn_sequence = do
    let run call1 call2 = derive_tracks False DeriveTest.e_dyn_rounded $
            make_dyn_tracks (4, call1) (4, call2)
    equal (run ".5" "!=") ([[(0, 0.5)], [(4, 0.5)], [(4, 0.5)]], [])
    equal (run ".5" "!=>")
        ([ [(0, 0.5)]
         , [(4, 0.5), (6, 0.5), (7, 0.42), (8, 0)]
         , [(8, 0)]
         ], [])
    equal (run ".5" "!<=")
        ([ [(0, 0.5)]
         , [(4, 0), (5, 0.84), (6, 1)]
         , [(6, 1)]
         ], [])
    equal (run ".5" "!T0=") ([[(0, 0.5)], [(6, 0)], [(6, 0)]], [])

make_dyn_tracks :: (ScoreTime, Text) -> (ScoreTime, Text) -> [UiTest.TrackSpec]
make_dyn_tracks (dur1, call1) (dur2, call2) =
    [ (">", [(0, dur1, ""), (dur1, dur2, ""), (dur1 + dur2, 2, "")])
    , ("*", [(0, 0, "4c"), (dur1, 0, "4d"), (dur1 + dur2, 0, "4e")])
    , ("dyn | dyn", [(0, dur1, call1), (dur1, dur2, call2)])
    ]

-- * util

derive_tracks :: Bool -> (Score.Event -> a) -> [UiTest.TrackSpec]
    -> ([a], [Text])
derive_tracks pitch_above extract = DeriveTest.extract extract
    . DeriveTest.derive_tracks
        ("import india.gamakam5 | transition=1 | dyn-transition=1"
            <> if pitch_above then " | gamakam-above=t" else "")

note_pitch_gamakam :: [UiTest.EventSpec] -> [UiTest.EventSpec]
    -> [UiTest.EventSpec] -> [UiTest.TrackSpec]
note_pitch_gamakam notes pitches gamakams =
    [(">", notes), ("*", pitches), ("t-nn | gamak", gamakams)]

pitch_gamakam_note :: [(ScoreTime, Text)] -> [(ScoreTime, Text)]
    -> [(ScoreTime, ScoreTime)] -> [UiTest.TrackSpec]
pitch_gamakam_note pitches gamakams notes =
    [ ("*", [(s, 0, p) | (s, p) <- pitches])
    , ("t-nn | gamak", [(s, 0, p) | (s, p) <- gamakams])
    , (">", [(s, d, "") | (s, d) <- notes])
    ]

note_pitch_gamakam0 :: [(ScoreTime, ScoreTime)] -> [(ScoreTime, Text)]
    -> [(ScoreTime, Text)] -> [UiTest.TrackSpec]
note_pitch_gamakam0 notes pitches gamakams =
    [ (">", [(s, d, "") | (s, d) <- notes])
    , ("*", [(s, 0, p) | (s, p) <- pitches])
    , ("t-nn | gamak", [(s, 0, p) | (s, p) <- gamakams])
    ]

make_tracks :: [(ScoreTime, ScoreTime, Text, Text)]
    -- ^ (start, dur, pitch, gamakam)
    -> [UiTest.TrackSpec]
make_tracks notes =
    [ (">", [(start, dur, "") | (start, dur, _, _) <- notes])
    , ("*", [(start, 0, pitch) | (start, _, pitch, _) <- notes, pitch /= ""])
    , ("t-nn | gamak",
        [(start, 0, call) | (start, _, _, call) <- notes, call /= ""])
    , ("dyn | dyn", [(0, 0, "1")])
    ]

make_2notes :: (ScoreTime, Text) -> (ScoreTime, Text) -> [UiTest.TrackSpec]
make_2notes (dur1, call1) (dur2, call2) = make_tracks
    [ (0, dur1, "4c", call1)
    , (dur1, dur2, "4d", call2)
    , (dur1 + dur2, 2, "4e", "")
    ]
