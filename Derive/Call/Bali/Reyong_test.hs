-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Call.Bali.Reyong_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Gangsa_test as Gangsa_test
import qualified Derive.Call.Bali.Reyong as Reyong
import Derive.Call.Bali.Reyong (Hand(..))
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


title :: String
title = "import bali.reyong | scale=legong | inst = >i1"

title_cancel :: String
title_cancel = title <> " | cancel-kotekan 5"

title_damp :: Double -> String
title_damp dur = title_cancel <> " | infer-damp >i1 " <> show dur

title_realize :: String
title_realize = title <> " | realize-reyong >i1"

test_articulation = do
    let run = e_voice 1 DeriveTest.e_start_note
            . DeriveTest.derive_tracks title_cancel . UiTest.note_track
    equal (run [(0, 0, "X --"), (1, 0, "O --"), (2, 0, "+ --")])
        (Just [(0, "4u"), (1, "4e"), (1, "4a"), (2, "4e"), (2, "4a")], [])
    equal (run [(0, 0, "XX --")]) (Just [(0, "4u"), (0, "4u")], [])
    -- Not affected by transposition.
    equal (run [(0, 0, "%t-dia=1 | O --")]) (Just [(0, "4e"), (0, "4a")], [])

test_norot = do
    let run = first (lookup 2) . e_pattern 0
            . DeriveTest.derive_tracks title_cancel . UiTest.note_track
    equal (run [(0, 4, "nt -- 4i")]) (Just "ioioi", [])
    equal (run [(0, 8, "nt -- 4i"), (8, 4, "nt -- 4o")])
        (Just "ioioiooeoeoeo", [])

test_kilitan_prepare = do
    let run = e_voice 1 DeriveTest.e_start_note
            . DeriveTest.derive_tracks title_cancel . UiTest.note_track
    equal (run [(0, 4, "initial=f | >kilit -- 3u"), (4, 4, "kilit -- 3u")])
        (Just
            [ (1, "4u"), (2, "4u"), (3, "4a"), (4, "4u")
            , (5, "4a"), (6, "4u"), (7, "4a"), (8, "4u")
            ], [])

test_kilitan_random_start = do
    let run = e_by_voice extract
            . DeriveTest.derive_tracks (title <> " | reyong-voices = (list 1 3)\
                \ | apply-start-offset | %start-s = (cf-rnd-a .5)")
            . UiTest.note_track
        extract e = (Score.event_start e, DeriveTest.e_pitch e)
    -- Notes all created by the same call all have unique randomization.
    -- This indirectly tests 'Derive.state_event_serial', also directly tested
    -- in "Derive.EvalTrack_test".
    let (notes, logs) = run [(0, 4, "kilit -- 4i")]
    equal logs []
    let [voice1, voice3] = map snd notes
    equal (map snd voice1) ["4a", "4u", "4a"]
    equal (map snd voice3) ["5a", "5u", "5a"]
    forM_ (zip (map fst voice1) (map fst voice3)) $ \(start1, start3) ->
        not_equal start1 start3

test_kotekan_regular = do
    let run voice = first (lookup voice) . e_pattern 0
            . DeriveTest.derive_tracks title_cancel . UiTest.note_track
    equal (run 1 [(0, 8, "k k-12_1-21 -- 4i")]) (Just "ueu--ue-u", [])
    equal (run 2 [(0, 8, "k k-12_1-21 -- 4i")]) (Just "i-io-i-oi", [])
    equal (run 1 [(0, 8, "k// -- 4e")]) (Just "e-eu-eu-e", [])
    equal (run 2 [(0, 8, "k// -- 4e")]) (Just "-o-io-io-", [])

    -- Cancelling favors the end note.
    equal (run 2 [(0, 8, "k k-12-1-21 -- 4i"), (8, 8, "k// -- 4e")])
        (Just "i-io-i-oio-io-io-", [])
    equal (run 3 [(0, 8, "k k-12-1-21 -- 4i"), (8, 8, "k// -- 4e")])
        (Just "ueu-eue-u-eu-eu-e", [])

test_kotekan_irregular = do
    let run voice = first (lookup voice) . e_pattern 0
            . DeriveTest.derive_tracks title_cancel . UiTest.note_track
    equal (run 1 [(0, 16, "k//\\\\ -- 6i")])
        (Just "--ua-ua-au-au-ua-", [])
    equal (run 1 [(16, -16, "k//\\\\ -- 6i")])
        (Just "--ua-ua-au-au-ua-", [])

test_kotekan_negative_slice = do
    -- This actually ensures that negative slices include the final pitch
    -- sample even when it's under an orphan slice.
    let run voice = first (lookup voice) . e_pattern 0
            . derive_tracks_ruler title_cancel
    equal (run 1
            [(">", []), (">", [(16, -16, "k//\\\\")]), ("*", [(16, -0, "4i")])])
        (Just "--ua-ua-au-au-ua-", [])

derive_tracks_ruler :: String -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_ruler title tracks =
    DeriveTest.derive_blocks_setup DeriveTest.with_linear
        [(UiTest.default_block_name <> "=ruler -- " <> title, tracks)]

test_cancel_kotekan = do
    let run = e_pattern 0
            . DeriveTest.derive_tracks (title_cancel <> " | reyong-voices=2")
            . UiTest.note_track
    equal (run [(0, 8, "k k-12-1-21 -- 4i")]) ([(2, "i-io-i-oi")], [])
    equal (run [(0, 8, "k_\\ -- 4i"), (8, 8, "k//\\\\ -- 4o")])
        ([(2, "i-ii-i-oi-e-oe-o")], [])

e_pattern :: RealTime -- ^ expect the first note at this time
    -> Derive.Result -> ([(Reyong.Voice, String)], [String])
e_pattern start = first (Gangsa_test.convert_to_pattern pitch_digit start)
    . e_by_voice DeriveTest.e_start_note

pitch_digit :: String -> String
pitch_digit p = drop 1 p

e_by_voice :: (Score.Event -> a) -> Derive.Result
    -> ([(Reyong.Voice, [a])], [String])
e_by_voice extract =
    first Seq.group_fst . DeriveTest.extract (\e -> (event_voice e, extract e))

event_voice :: Score.Event -> Reyong.Voice
event_voice = fromMaybe 0 . Env.maybe_val EnvKey.voice . Score.event_environ

e_voice :: Int -> (Score.Event -> a) -> Derive.Result -> (Maybe [a], [String])
e_voice voice extract = group_voices . DeriveTest.extract ex
    where
    ex e = (DeriveTest.e_environ_val EnvKey.voice e :: Maybe Int, extract e)
    group_voices = first (lookup (Just voice) . Seq.group_fst)

-- Force all the positions because of partial functions in there.
test_positions = forM_ Reyong.reyong_positions $ \pos -> equal pos pos

test_assign_positions = do
    let f pattern dest = extract $ Reyong.assign_positions pattern 5 dest
            (map Reyong.pos_cek Reyong.reyong_positions)
        extract = map (unwords . map show_pitch)
        p1 = Reyong.parse_kotekan "4-3" "12-"
    equal (f p1 0) ["4u 4a -", "5o - 5i", "5u 5a -", "6o - 6i"]
    equal (f p1 1) ["4a 5i -", "5e - 5o", "5a 6i -", "6e - 6o"]
    equal (f p1 2) ["4u - 4e", "5i 5o -", "5u - 5e", "6i 6o -"]
    equal (f p1 3) ["4a - 4u", "5o 5e -", "5a - 5u", "6o 6e -"]
    equal (f p1 4) ["4e 4u -", "5i - 4a", "5e 5u -", "6i - 5a"]

-- * tumpuk

test_tumpuk = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks title . UiTest.note_track
        extract e = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    equal (run [(1, 4, "t xo 1 -- 4i")])
        ([((1, 1, "4i"), "+mute"), ((2, 3, "4i"), "+")], [])
    equal (run [(1, 4, "place=0 | t xo 1 -- 4i")])
        ([((0, 1, "4i"), "+mute"), ((1, 4, "4i"), "+")], [])
    strings_like (snd $ run [(1, 4, "t px0o 1 -- 4i")]) ["no prev pitch"]
    equal (run [(0, 1, "4o"), (1, 4, "t px0o 1 -- 4i")])
        ([((0, 1, "4o"), "+"), ((1, 1, "4o"), "+mute"), ((2, 3, "4i"), "+")],
            [])

test_tumpuk_patterns = do
    -- Force partial function.
    equal Reyong.tumpuk_patterns Reyong.tumpuk_patterns

test_select_pattern = do
    let f = Reyong.select_pattern
        rnds = [0, 1/16 .. 1]
        permutations dur = [f dur rnd1 rnd2 | rnd1 <- rnds, rnd2 <- rnds]
    let dur_of (notes, dur) = fromIntegral (length notes) * dur
        all_ok xs = (xs, replicate (length xs) (Right True))
        desired = (* (2/3))
    -- If there is lots of time, then patterns never exceed 2/3.
    uncurry equal
        (all_ok (map ((<= desired 16) . dur_of <$>) (permutations 16)))
    -- If there is not much time, patterns are either <=1 or Left.
    -- TODO test that
    left_like (f 0.1 0 0) "no patterns fit"

-- * c_byong

test_c_byong = do
    let run voice = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks (title <> " | v=" <> show voice)
            . UiTest.note_track
    equal (run 1 [(1, 2, "o --")]) ([(1, 2, "4e"), (1, 2, "4a")], [])
    equal (run 2 [(1, 2, "o --")]) ([(1, 2, "5i"), (1, 2, "5e")], [])
    equal (run 0 [(0, 2, "o --")]) ([], ["Error: unknown position: 0"])
    equal (run 1 [(1, 2, ": --")]) ([(1, 2, "4e"), (1, 2, "5i")], [])

-- * damp

test_c_infer_damp = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks (title_damp 1 <> " | %damp=.5")
            . UiTest.note_track
        extract e = (Score.event_start e, DeriveTest.e_pitch e, e_damp_dyn e)
    -- +undamped prevents damping.
    equal (run [(0, 1, "4i"), (1, 1, "+undamped -- 4o")])
        ([(0, "4i", Nothing), (1, "4o", Nothing), (1, "4i", Just 0.5)], [])
    -- Too fast, no damp for 4i.
    equal (run [(0, 0.5, "4i"), (0.5, 0.5, "4o"), (1, 1, "4i")])
        ( [ (0, "4i", Nothing), (0.5, "4o", Nothing)
          , (1, "4i", Nothing), (1, "4o", Just 0.5), (2, "4i", Just 0.5)
          ]
        , []
        )
    -- Repeated notes, only the second is damped.
    equal (run [(0, 1, "4i"), (1, 1, "4i")])
        ([(0, "4i", Nothing), (1, "4i", Nothing), (2, "4i", Just 0.5)], [])

test_c_infer_damp_kotekan = do
    let run = e_voice 2 extract . DeriveTest.derive_tracks (title_damp 1.5)
            . UiTest.note_track
        extract e = (Score.event_start e, DeriveTest.e_pitch e, e_damp_dyn e)
    -- The first 5i is too fast, so no damp.
    equal (run [(0, 4, "k k-12-1-21 -- 4i")])
        ( Just
            [ (0, "5i", Nothing)
            , (2, "5i", Nothing), (3, "5o", Nothing)
            , (3, "5i", Just 1), (4, "5o", Just 1)
            ]
        , []
        )

test_c_infer_damp_cek = do
    let run = e_voice 1 extract
            . DeriveTest.derive_tracks (title_damp 0.5 <> " | v=1")
            . UiTest.note_track
        extract e = (Score.event_start e, DeriveTest.e_pitch e,
            DeriveTest.e_attributes e)
    -- Ceks and byongs don't get damps.
    equal (run [(0, 0, "X --")]) (Just [(0, "4u", "+cek")], [])
    equal (run [(0, 0, "O --"), (1, 1, "4i"), (3, 0, "+ --")])
        ( Just
            [ (0, "4e", "+"), (0, "4a", "+")
            , (1, "4i", "+"), (2, "4i", "+mute")
            , (3, "4e", "+mute"), (3, "4a", "+mute")
            ]
        , []
        )

e_damp_dyn :: Score.Event -> Maybe Signal.Y
e_damp_dyn e
    | Score.has_attribute Attrs.mute e = Just (Score.initial_dynamic e)
    | otherwise = Nothing

test_infer_damp = do
    let f dur = Reyong.infer_damp (const dur) . mkevents
    -- Damp with the other hand.
    equal (f 1 [(0, "4c"), (1, "4d"), (2, "4e")]) [1, 1, 1]
    -- 4e can't be damped because both hands are busy.
    equal (f 1 [(0, "4d"), (1, "4e"), (2, "4f"), (2, "4d")]) [1, 0, 1, 1]
    -- First 4d can't damp because the same hand is busy, and the other
    -- hand is blocked by the same hand.
    equal (f 1.1 [(0, "4c"), (1, "4d"), (3, "4d"), (4, "4c")]) [1, 0, 1, 1]
    -- But give a bit more time and all is possible.
    equal (f 0.75 [(0, "4c"), (1, "4d"), (3, "4d"), (4, "4c")]) [1, 1, 1, 1]

test_assign_hands = do
    let f = map fst . Reyong.assign_hands . mkevents
    equal (f [(0, "4c"), (1, "4c"), (2, "4d"), (3, "4d"), (4, "4c")])
        [L, L, R, R, L]
    equal (f [(0, "4c"), (1, "4d"), (2, "4e")]) [L, R, R]
    equal (f [(0, "4d"), (1, "4e"), (2, "4f"), (2, "4d")]) [L, R, R, L]

mkevents :: [(RealTime, String)] -> [Score.Event]
mkevents = map $
    DeriveTest.mkevent . (\(t, p) -> (t, 1, p, [], Score.empty_instrument))

show_pitch :: Maybe Pitch.Pitch -> String
show_pitch Nothing = "-"
show_pitch (Just (Pitch.Pitch oct (Pitch.Degree d _))) =
    show oct ++ ("ioeua" !! d) : ""

-- * ngoret

test_ngoret = do
    let run = DeriveTest.extract (\e -> (e_damp_dyn e, DeriveTest.e_note e))
            . DeriveTest.derive_tracks title_realize
            . UiTest.note_track
    let tracks = [(0, 2, "4i"), (2, 2, "' .25 -- 4e")]
    let e_notes ns = [n | (Nothing, n) <- ns]
    equal (first e_notes $ run tracks)
        ([(0, 2, "4i"), (1.75, 0.25, "4o"), (2, 2, "4e")], [])
    let e_damps ns = [(s, p) | (Just _, (s, _, p)) <- ns]
    equal (first e_damps $ run tracks) ([(2, "4i"), (4, "4e")], [])

-- * octave transposition

test_lower_octave_note = do
    let run = first e_notes
            . DeriveTest.extract (\e -> (e_damp_dyn e, DeriveTest.e_note e))
            . DeriveTest.derive_tracks title_realize
            . UiTest.note_track
        e_notes ns = [n | (Nothing, n) <- ns]
    equal (run [(0, 1, "4i"), (1, 1, "vv | -- 4o"), (2, 1, "4e")])
        ([(0, 1, "4i"), (1, 1, "4o"), (1, 1, "3o"), (2, 1, "4e")], [])
    equal (run [(0, 1, "4i"), (1, 1, "vv | ' .5 -- 4e")])
        ([(0, 1, "4i"), (0.5, 0.5, "4o"), (1, 1, "4e"), (1, 1, "3e")], [])

test_upper_voice = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks (title <> " | v=1 | upper")
            . UiTest.note_track
        extract e = (DeriveTest.e_note e, DeriveTest.e_environ EnvKey.voice e)
    equal (run [(0, 1, "3i"), (1, 1, "3o")])
        ( [ ((0, 1, "3i"), Just "1"), ((0, 1, "4i"), Just "3")
          , ((1, 1, "3o"), Just "1"), ((1, 1, "4o"), Just "3")
          ]
        , []
        )
