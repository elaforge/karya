-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.C.Bali.Reyong_test where
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Bali.Gangsa_test as Gangsa_test
import qualified Derive.C.Bali.Reyong as Reyong
import           Derive.Call (Hand(..))
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


title :: Text
title = "import bali.reyong | scale=legong | inst = i1"

titleCancel :: Text
titleCancel = title <> " | cancel-kotekan 5"

titleDamp :: Double -> Text
titleDamp dur = titleDampAll dur 0

titleDampAll :: Double -> Double -> Text
titleDampAll dur early =
    titleCancel <> " | infer-damp i1 "
        <> Text.unwords [ShowVal.showVal dur, ShowVal.showVal early]

titleRealize :: Text
titleRealize = title <> " | infer-damp-early=0 | realize-reyong i1"

test_articulation :: Test
test_articulation = do
    let run = eVoice 1 DeriveTest.eStartNote
            . DeriveTest.deriveTracks titleCancel . UiTest.noteTrack
    equal (run [(0, 0, "X --"), (1, 0, "O --"), (2, 0, "+ --")])
        (Just [(0, "4u"), (1, "4e"), (1, "4a"), (2, "4e"), (2, "4a")], [])
    equal (run [(0, 0, "XX --")]) (Just [(0, "4u"), (0, "4u")], [])
    -- Not affected by transposition.
    equal (run [(0, 0, "t-dia=1 | O --")]) (Just [(0, "4e"), (0, "4a")], [])

test_norot :: Test
test_norot = do
    let run = first (lookup 2) . ePattern 0
            . DeriveTest.deriveTracks titleCancel . UiTest.noteTrack
    equal (run [(0, 4, "nt -- 4i")]) (Just "ioioi", [])
    equal (run [(0, 8, "nt -- 4i"), (8, 4, "nt -- 4o")])
        (Just "ioioiooeoeoeo", [])

test_kilitan_prepare :: Test
test_kilitan_prepare = do
    let run = eVoice 1 DeriveTest.eStartNote
            . DeriveTest.deriveTracks titleCancel . UiTest.noteTrack
    equal (run [(0, 4, "initial=f | >kilit -- 3u"), (4, 4, "kilit -- 3u")])
        (Just
            [ (1, "4u"), (2, "4u"), (3, "4a"), (4, "4u")
            , (5, "4a"), (6, "4u"), (7, "4a"), (8, "4u")
            ], [])

test_kilitan_random_start :: Test
test_kilitan_random_start = do
    let run = eByVoice extract
            . DeriveTest.deriveTracks (title <> " | reyong-voices = (list 1 3)\
                \ | apply-start-offset | start-s = (cf-rnd-a .5)")
            . UiTest.noteTrack
        extract e = (Score.eventStart e, DeriveTest.ePitch e)
    -- Notes all created by the same call all have unique randomization.
    -- This indirectly tests 'Derive.state_event_serial', also directly tested
    -- in "Derive.EvalTrack_test".
    let (notes, logs) = run [(0, 4, "kilit -- 4i")]
    equal logs []
    let [voice1, voice3] = map snd notes
    equal (map snd voice1) ["4a", "4u", "4a"]
    equal (map snd voice3) ["5a", "5u", "5a"]
    forM_ (zip (map fst voice1) (map fst voice3)) $ \(start1, start3) ->
        notEqual start1 start3

test_kotekan_regular :: Test
test_kotekan_regular = do
    let run1 text = first (map snd . take 2) . ePattern 0
            . DeriveTest.deriveTracks titleCancel . UiTest.noteTrack $
                [(0, 8, text)]
    equal (run1 "k k-12_1-21 -- 4i") (["ueu--ue-u", "i-io-i-oi"], [])
    equal (run1 "k -- 4i") (["ueu-eue-u", "i-io-i-oi"], [])
    equal (run1 "k -- 4e") (["e-eu-e-ue", "iai-aia-i"], [])
    equal (run1 "k -- 4u") (["u-ua-u-au", "oio-ioi-o"], [])
    -- inferred part plays the below pattern, though voice 1 is always below
    equal (run1 "k _ d -- 4i") (["-a-ua-au-", "i-io-i-oi"], [])
    equal (run1 "k^ -- 4i") (["-o-eo-oe-", "i-ia-i-ai"], [])
    equal (run1 "k^ _ d -- 4i") (["eue-ueu-e", "i-ia-i-ai"], [])
    equal (run1 "k// -- 4e") (["e-eu-eu-e", "-o-io-io-"], [])

    let run voice = first (lookup voice) . ePattern 0
            . DeriveTest.deriveTracks titleCancel . UiTest.noteTrack
    -- Cancelling favors the end note.
    equal (run 2 [(0, 8, "k -- 4i"), (8, 8, "k// -- 4e")])
        (Just "i-io-i-oio-io-io-", [])
    equal (run 3 [(0, 8, "k -- 4i"), (8, 8, "k// -- 4e")])
        (Just "ueu-eue-u-eu-eu-e", [])

test_kotekan_irregular :: Test
test_kotekan_irregular = do
    let run voice = first (lookup voice) . ePattern 0
            . DeriveTest.deriveTracks titleCancel . UiTest.noteTrack
    equal (run 1 [(0, 16, "k//\\\\ -- 6i")])
        (Just "--ua-ua-au-au-ua-", [])
    equal (run 1 [(16, -16, "k//\\\\ -- 6i")])
        (Just "--ua-ua-au-au-ua-", [])

test_kotekan_negative_slice :: Test
test_kotekan_negative_slice = do
    -- This actually ensures that negative slices include the final pitch
    -- sample even when it's under an orphan slice.
    let run voice = first (lookup voice) . ePattern 0
            . deriveTracksRuler titleCancel
    equal (run 1
            [(">", []), (">", [(16, -16, "k//\\\\")]), ("*", [(16, -0, "4i")])])
        (Just "--ua-ua-au-au-ua-", [])

deriveTracksRuler :: Text -> [UiTest.TrackSpec] -> Derive.Result
deriveTracksRuler title tracks =
    DeriveTest.deriveBlocksSetup DeriveTest.withLinear
        [(UiTest.defaultBlockName <> "=ruler -- " <> title, tracks)]

test_cancel_kotekan :: Test
test_cancel_kotekan = do
    let run = ePattern 0
            . DeriveTest.deriveTracks (titleCancel <> " | voices=2")
            . UiTest.noteTrack
    equal (run [(0, 8, "k k-12-1-21 -- 4i")]) ([(2, "i-io-i-oi")], [])
    equal (run [(0, 8, "k_\\ -- 4i"), (8, 8, "k//\\\\ -- 4o")])
        ([(2, "i-ii-i-oi-e-oe-o")], [])

ePattern :: RealTime -- ^ expect the first note at this time
    -> Derive.Result -> ([(Reyong.Voice, Text)], [Text])
ePattern start = first (Gangsa_test.convertToPattern pitchDigit start)
    . eByVoice DeriveTest.eStartNote

pitchDigit :: Text -> Text
pitchDigit = Text.drop 1

eByVoice :: (Score.Event -> a) -> Derive.Result
    -> ([(Reyong.Voice, [a])], [Text])
eByVoice extract =
    first Lists.groupFst
    . DeriveTest.extract (\e -> (eventVoice e, extract e))

eventVoice :: Score.Event -> Reyong.Voice
eventVoice = fromMaybe 0 . Env.maybeVal EnvKey.voice . Score.eventEnviron

eVoice :: Int -> (Score.Event -> a) -> Derive.Result -> (Maybe [a], [Text])
eVoice voice extract = groupVoices . DeriveTest.extract ex
    where
    ex e = (DeriveTest.eEnvironVal EnvKey.voice e :: Maybe Int, extract e)
    groupVoices = first (lookup (Just voice) . Lists.groupFst)

-- Force all the positions because of partial functions in there.
test_positions :: Test
test_positions = forM_ Reyong.reyongPositions $ \pos -> equal pos pos

test_assign_positions :: Test
test_assign_positions = do
    let f pattern dest = extract $ Reyong.assignPositions pattern 5 dest
            (map Reyong.posCek Reyong.reyongPositions)
        extract = map (Text.unwords . map showPitch)
        p1 = Reyong.parseKotekan "4-3" "12-"
    equal (f p1 0) ["4u 4a -", "5o - 5i", "5u 5a -", "6o - 6i"]
    equal (f p1 1) ["4a 5i -", "5e - 5o", "5a 6i -", "6e - 6o"]
    equal (f p1 2) ["4u - 4e", "5i 5o -", "5u - 5e", "6i 6o -"]
    equal (f p1 3) ["4a - 4u", "5o 5e -", "5a - 5u", "6o 6e -"]
    equal (f p1 4) ["4e 4u -", "5i - 4a", "5e 5u -", "6i - 5a"]

-- * tumpuk

test_tumpuk :: Test
test_tumpuk = do
    let run = DeriveTest.extract extract
            . DeriveTest.deriveTracks title . UiTest.noteTrack
        extract e = (DeriveTest.eNote e, DeriveTest.eAttributes e)
    equal (run [(1, 4, "t xo 1 -- 4i")])
        ([((1, 1, "4i"), "+mute"), ((2, 3, "4i"), "+")], [])
    equal (run [(1, 4, "place=0 | t xo 1 -- 4i")])
        ([((0, 1, "4i"), "+mute"), ((1, 4, "4i"), "+")], [])
    stringsLike (snd $ run [(1, 4, "t px0o 1 -- 4i")]) ["no prev pitch"]
    equal (run [(0, 1, "4o"), (1, 4, "t px0o 1 -- 4i")])
        ([((0, 1, "4o"), "+"), ((1, 1, "4o"), "+mute"), ((2, 3, "4i"), "+")],
            [])

test_tumpuk_patterns :: Test
test_tumpuk_patterns = do
    -- Force partial function.
    equal Reyong.tumpukPatterns Reyong.tumpukPatterns

test_select_pattern :: Test
test_select_pattern = do
    let f = Reyong.selectPattern
        rnds = [0, 1/16 .. 1]
        permutations dur = [f dur rnd1 rnd2 | rnd1 <- rnds, rnd2 <- rnds]
    let durOf (notes, dur) = fromIntegral (length notes) * dur
        allOk xs = (xs, replicate (length xs) (Right True))
        desired = (* (2/3))
    -- If there is lots of time, then patterns never exceed 2/3.
    uncurry equal
        (allOk (map ((<= desired 16) . durOf <$>) (permutations 16)))
    -- If there is not much time, patterns are either <=1 or Left.
    -- TODO test that
    leftLike (f 0.1 0 0) "no patterns fit"

-- * c_byong

test_c_byong :: Test
test_c_byong = do
    let run voice = DeriveTest.extract DeriveTest.eNote
            . DeriveTest.deriveTracks (title <> " | v=" <> showt voice)
            . UiTest.noteTrack
    equal (run 1 [(1, 2, "o --")]) ([(1, 2, "4e"), (1, 2, "4a")], [])
    equal (run 2 [(1, 2, "o --")]) ([(1, 2, "5i"), (1, 2, "5e")], [])
    equal (run 0 [(0, 2, "o --")]) ([], ["unknown position: 0"])
    equal (run 1 [(1, 2, ": --")]) ([(1, 2, "4e"), (1, 2, "5i")], [])

-- * damp

test_c_infer_damp :: Test
test_c_infer_damp = do
    let run = DeriveTest.extract eDampedEvent
            . DeriveTest.deriveTracks (titleDamp 1 <> " | damp=.5")
            . UiTest.noteTrack
    -- +undamped prevents damping.
    equal (run [(0, 1, "4i"), (1, 1, "+undamped -- 4o")])
        ([(0, "4i", '-'), (1, "4i", '+'), (1, "4o", '-')], [])
    -- Too fast, no damp for 4i.
    equal (run [(0, 0.5, "4i"), (0.5, 0.5, "4o"), (1, 1, "4i")])
        ( [ (0, "4i", '-'), (0.5, "4o", '-')
          , (1, "4o", '+'), (1, "4i", '-'), (2, "4i", '+')
          ]
        , []
        )
    -- Repeated notes, only the second is damped.
    equal (run [(0, 1, "4i"), (1, 1, "4i")])
        ([(0, "4i", '-'), (1, "4i", '-'), (2, "4i", '+')], [])
    equal (run [(0, 1, "4i")]) ([(0, "4i", '-'), (1, "4i", '+')], [])

{-
test_infer_damp_integrate :: Test
test_infer_damp_integrate = do
    -- let run = extract $ DeriveTest.derive_tracks "" $
    --         UiTest.note_spec
    --             ( " | < | voices=1 | " <> title_damp 1.5
    --             , [(0, 1, "4i"), (3, 1, "4o")]
    --             , []
    --             )
    let run integrate = extract $ DeriveTest.derive_tracks ""
            [ ("tempo", [(0, 0, ".5")])
            , ( "> " <> (if integrate then "| < " else "")
                <> "| voices=1 | " <> title_damp 1.2
              , [(0, 8, "k")]
              )
            , ("*", [(0, 0, "4i")])
            ]
        extract r = (map e_integrated $ Derive.r_integrated r, es, logs)
            where (es, logs) = DeriveTest.extract e_damped_event2 r
        e_integrated = map e_damped_event2 . Stream.events_of
            . Derive.integrated_events
        -- e_integrated (Derive.Integrated source events) =
        --     (source, map e_note (Stream.events_of events))
        -- e_note e = (DeriveTest.e_start_note e, DeriveTest.e_attributes e)
        -- e_note e =
        --     ( Score.event_start e
        --     , DeriveTest.e_pitch e <> if mute then "+" else ""
        --     )
        --     where mute = Score.has_attribute Attrs.mute e
    equal (run False)
        ( []
        , [ (0, "4u"), (1, "4e"), (2, "4u")
          , (2, "4e+"), (3, "4u+")
          , (4, "4e"), (5, "4u"), (6, "4e")
          , (6, "4u+"), (7, "4e+")
          , (8, "4u"), (9, "4u+")
          ]
        , []
        )
    equal (run True)
        ( [[ (0, "4u"), (1, "4e"), (2, "4u")
          , (2, "4e+"), (3, "4u+")
          , (4, "4e"), (5, "4u"), (6, "4e")
          , (6, "4u+"), (7, "4e+")
          , (8, "4u"), (9, "4u+")
          ]]
        , []
        , []
        )
-}

test_c_infer_damp_early :: Test
test_c_infer_damp_early = do
    let run = DeriveTest.extract eDampedEvent
            . DeriveTest.deriveTracks (titleDampAll 1 0.5)
            . UiTest.noteTrack
    equal (run [(0, 1, "4i"), (1, 1, "4o")])
        ([(0, "4i", '-'), (0.5, "4i", '+'), (1, "4o", '-'), (2, "4o", '+')], [])
    equal (run [(0, 1, "4i"), (2, 1, "4i")])
        ([(0, "4i", '-'), (1, "4i", '+'), (2, "4i", '-'), (3, "4i", '+')], [])

test_c_infer_damp_ngoret :: Test
test_c_infer_damp_ngoret = do
    let run = DeriveTest.extract eDampedEvent
            . DeriveTest.deriveTracks titleRealize
            . UiTest.noteTrack
    -- First note is muted, but the grace note is too quick.
    equal (run [(0, 1, "4i"), (1, 1, "' -- 4e")])
        ( [ (0, "4i", '-'), (0.9, "4o", '-')
          , (1, "4i", '+'), (1, "4e", '-')
          , (2, "4e", '+')
          ]
        , []
        )

-- TODO oops, is this a duplicate with the above?
test_ngoret :: Test
test_ngoret = do
    let run = DeriveTest.extract (\e -> (eDampDyn e, DeriveTest.eNote e))
            . DeriveTest.deriveTracks titleRealize
            . UiTest.noteTrack
    let tracks = [(0, 2, "4i"), (2, 2, "' .25 -- 4e")]
    let eNotes ns = [n | (Nothing, n) <- ns]
    equal (first eNotes $ run tracks)
        ([(0, 2, "4i"), (1.75, 0.25, "4o"), (2, 2, "4e")], [])
    let eDamps ns = [(s, p) | (Just _, (s, _, p)) <- ns]
    equal (first eDamps $ run tracks) ([(2, "4i"), (4, "4e")], [])

test_c_infer_damp_kotekan :: Test
test_c_infer_damp_kotekan = do
    let run = eVoice 2 eDampedEvent
            . DeriveTest.deriveTracks (titleDamp 1.5) . UiTest.noteTrack
    -- The first 5i is too fast, so no damp.
    equal (run [(0, 4, "k k-12-1-21 -- 4i")])
        ( Just
            [ (0, "5i", '-')
            , (2, "5i", '-'), (3, "5i", '+')
            , (3, "5o", '-'), (4, "5o", '+')
            ]
        , []
        )

test_c_infer_damp_cek :: Test
test_c_infer_damp_cek = do
    let run = eVoice 1 extract
            . DeriveTest.deriveTracks (titleDamp 0.5 <> " | v=1")
            . UiTest.noteTrack
        extract e = (Score.eventStart e, DeriveTest.ePitch e,
            DeriveTest.eAttributes e)
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

eDampedEvent :: Score.Event -> (RealTime, Text, Char)
eDampedEvent e = (Score.eventStart e, DeriveTest.ePitch e, eDamped e)

eDampedEvent2 :: Score.Event -> (RealTime, Text)
eDampedEvent2 e =
    (Score.eventStart e, DeriveTest.ePitch e <> if mute then "+" else "")
    where mute = Score.hasAttribute Attrs.mute e

eDamped :: Score.Event -> Char
eDamped e = if Score.hasAttribute Attrs.mute e then '+' else '-'

eDampDyn :: Score.Event -> Maybe Signal.Y
eDampDyn e
    | Score.hasAttribute Attrs.mute e = Just (Score.initialDynamic e)
    | otherwise = Nothing

test_infer_damp :: Test
test_infer_damp = do
    let f dur = map (toChar . snd) . fst . Reyong.inferDamp (const dur)
            . mkevents
        toChar b = if b then '1' else '0'
    -- Damp with the other hand.
    equal (f 1 [(0, "4c"), (1, "4d"), (2, "4e")]) "111"
    -- 4e can't be damped because both hands are busy.
    equal (f 1 [(0, "4d"), (1, "4e"), (2, "4f"), (2, "4d")]) "1011"
    -- First 4d can't damp because the same hand is busy, and the other
    -- hand is blocked by the same hand.
    equal (f 1.1 [(0, "4c"), (1, "4d"), (3, "4d"), (4, "4c")]) "1011"
    -- But give a bit more time and all is possible.
    equal (f 0.75 [(0, "4c"), (1, "4d"), (3, "4d"), (4, "4c")]) "1111"

test_assign_hands :: Test
test_assign_hands = do
    let f = map fst . fst . Reyong.assignHands . mkevents
    equal (f [(0, "4c"), (1, "4c"), (2, "4d"), (3, "4d"), (4, "4c")])
        [L, L, R, R, L]
    equal (f [(0, "4c"), (1, "4d"), (2, "4e")]) [L, R, R]
    equal (f [(0, "4d"), (1, "4e"), (2, "4f"), (2, "4d")]) [L, R, R, L]

mkevents :: [(RealTime, Text)] -> [Score.Event]
mkevents = map $
    DeriveTest.mkevent . (\(t, p) -> (t, 1, p, [], ScoreT.emptyInstrument))

showPitch :: Maybe Pitch.Pitch -> Text
showPitch Nothing = "-"
showPitch (Just (Pitch.Pitch oct (Pitch.Degree d _))) =
    showt oct <> Text.singleton ("ioeua" !! d)

-- * octave transposition

test_lower_octave_note :: Test
test_lower_octave_note = do
    let run = first eNotes
            . DeriveTest.extract (\e -> (eDampDyn e, DeriveTest.eNote e))
            . DeriveTest.deriveTracks titleRealize
            . UiTest.noteTrack
        eNotes ns = [n | (Nothing, n) <- ns]
    equal (run [(0, 1, "4i"), (1, 1, "vv | -- 4o"), (2, 1, "4e")])
        ([(0, 1, "4i"), (1, 1, "4o"), (1, 1, "3o"), (2, 1, "4e")], [])
    equal (run [(0, 1, "4i"), (1, 1, "vv | ' .5 -- 4e")])
        ([(0, 1, "4i"), (0.5, 0.5, "4o"), (1, 1, "4e"), (1, 1, "3e")], [])

test_upper :: Test
test_upper = do
    let run tracks skel = DeriveTest.extract extract $
            DeriveTest.deriveTracksSetup (DeriveTest.withSkel skel) title
                tracks
        extract e = (DeriveTest.eNote e,
            fromMaybe "" $ DeriveTest.eEnviron EnvKey.voice e)
    equal (run
            [ (">", [(0, 2, "upper")])
            , ("> | v=+1", [(0, 1, "")]), ("*", [(0, 0, "4i")])
            , ("> | v=+2", [(0, 1, ""), (1, 1, "")])
            ,        ("*", [(0, 0, "4o"), (1, 0, "4e")])
            ] [(1, 2), (1, 4), (2, 3), (4, 5)])
        ( [ ((0, 1, "4i"), "1"), ((0, 1, "5i"), "3")
          , ((0, 1, "4o"), "2"), ((0, 1, "5o"), "4")
          , ((1, 1, "4e"), "2"), ((1, 1, "5e"), "4")
          ]
        , []
        )

-- * solkattu

test_solkattu_note :: Test
test_solkattu_note = do
    let run = DeriveTest.extract DeriveTest.eNote
            . DeriveTest.deriveTracks
                (title <> " | import bali.reyong.solkattu")
            . UiTest.noteTrack
    equal (run [(0, 1, "n1 -- 4i"), (1, 1, "n2 --"), (2, 1, "n14 --"),
            (4, 0, "-- 3i")])
        ([(0, 1, "4i"), (1, 1, "4o"), (2, 2, "4i"), (2, 2, "4u"), (4, 0, "3i")],
            [])
