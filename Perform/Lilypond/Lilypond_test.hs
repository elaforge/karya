module Perform.Lilypond.Lilypond_test where
import qualified Data.Char as Char
import qualified System.Process as Process

import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Args as Args
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Lily as Lily
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Perform.Lilypond.Meter as Meter
import Perform.Lilypond.Types (Duration(..))

import Types


test_convert_measures = do
    let f = LilypondTest.convert_measures [] . map simple_event
    equal (f [(0, 1, "a"), (1, 1, "b")]) $ Right ["a4 b4 r2"]
    equal (f [(1, 1, "a"), (2, 8, "b")]) $ Right
        ["r4 a4 b2~", "b1~", "b2 r2"]
    -- Rests are not dotted, even when they could be.
    -- I also get r8 r4, instead of r4 r8, see comment on 'allowed_time'.
    equal (f [(0, 1, "a"), (1.5, 1, "b")]) $ Right
        ["a4 r8 b8~ b8 r8 r4"]
    equal (f [(0, 2, "a"), (3.5, 0.25, "b"), (3.75, 0.25, "c")]) $ Right
        ["a2 r4 r8 b16 c16"]
    equal (f [(0, 0.5, "a"), (0.5, 1, "b"), (1.5, 0.5, "c")]) $ Right
        ["a8 b4 c8 r2"]

test_full_measure_rest = do
    let f = LilypondTest.convert_measures [] . map simple_event
    equal (f [(5, 1, "a")]) $ Right ["R1", "r4 a4 r2"]
    equal (f [(9, 1, "a")]) $ Right ["R1", "R1", "r4 a4 r2"]

test_dotted_rests = do
    let f meter = LilypondTest.convert_measures []
            . (meter_event (0, meter) :) . map simple_event
    -- Rests are allowed to be dotted when the meter isn't duple.
    equal (f "3+3/8" [(1.5, 0.5, "a")]) $ Right ["r4. a8 r4"]
    equal (f "4/4" [(3, 1, "a")]) $ Right ["r2 r4 a4"]

test_change_meter = do
    let f meters events = LilypondTest.convert_measures ["time"] $
            map meter_event meters ++ map simple_event events
    -- Change meter on the measure boundary.
    equal (f [(0, "2/4"), (2, "4/4")] [(0, 2, "a"), (2, 2, "b")]) $ Right
        ["\\time 2/4 a2", "\\time 4/4 b2 r2"]
    -- Meter changes round up to the next barline.
    equal (f [(0, "2/4"), (1, "4/4")] [(0, 2, "a"), (2, 2, "b")]) $ Right
        ["\\time 2/4 a2", "\\time 4/4 b2 r2"]
    -- Meter changes during a note.
    equal (f [(0, "2/4"), (2, "4/4")] [(0, 1, "a"), (1, 5, "b")]) $ Right
        ["\\time 2/4 a4 b4~", "\\time 4/4 b1"]
    -- Unparseable meter.
    left_like (f [(0, "2/4"), (2, "q4/4")] [(0, 2, "a"), (2, 2, "b")])
        "can't parse"

test_parse_error = do
    let f = LilypondTest.convert_measures [] . map environ_event
    left_like (f [(0, 1, "a", [(TrackLang.v_key, "oot-greet")])]) "unknown key"

test_chords = do
    let f = LilypondTest.convert_measures [] . map simple_event
    -- Homogenous durations.
    equal (f [(0, 1, "a"), (0, 1, "c")]) $ Right ["<a c>4 r4 r2"]
    -- Starting at the same time.
    equal (f [(0, 2, "a"), (0, 1, "c")]) $ Right ["<a c>4~ a4 r2"]
    -- Starting at different times.
    equal (f [(0, 2, "a"), (1, 1, "c")]) $ Right ["a4~ <a c>4 r2"]
    equal (f [(0, 2, "a"), (1, 2, "c"), (2, 2, "e")]) $ Right
        ["a4~ <a c>4~ <c e>4~ e4"]

test_convert_duration = do
    let f meter pos = Lilypond.to_lily $ head $
            Lilypond.convert_duration meter True False pos (whole - pos)
    equal (map (f (mkmeter "4/4")) [0, 8 .. 127])
        [ "1", "8.", "4.", "16", "2.", "8.", "8", "16"
        -- mid-measure
        , "2", "8.", "4.", "16", "4", "8.", "8", "16"
        ]
    equal (map (f (mkmeter "2/4")) [0, 8 .. 127]) $
        concat $ replicate 2 ["2", "8.", "4.", "16", "4", "8.", "8", "16"]

test_make_ly = do
    let run = LilypondTest.derive_staves []
    let (events, logs) = run $ concatMap UiTest.note_spec
            -- complicated rhythm
            [ ("s/i1", [(0, 1, "4c"), (1.5, 2, "4d#")], [])
            -- rhythm starts after 0, long multi measure note
            , ("s/i2", [(1, 1, "4g"), (2, 12, "3a")], [])
            ]
    equal logs []
    -- Shorter staff is padded out to the length of the longer one.
    equal events $ Right
        [ ("i1", [["c'4 r8 ds'8~ ds'4. r8", "R1", "R1", "R1"]])
        , ("i2", [["r4 g'4 a2~", "a1~", "a1~", "a2 r2"]])
        ]
    -- putStrLn $ LilypondTest.make_ly events
    -- compile_ly events

test_hands = do
    let run = LilypondTest.derive_staves [] . concatMap UiTest.note_spec
    let (events, logs) = run
            [ (">s/1 | hand = 'right'", [(0, 4, "4c")], [])
            , (">s/1 | hand = 'left'", [(0, 4, "4d")], [])
            , (">s/2", [(0, 4, "4e")], [])
            ]
    equal logs []
    -- Right hand goes in first.
    equal events $ Right
        [ ("1", [["c'1"], ["d'1"]])
        , ("2", [["e'1"]])
        ]

test_clefs = do
    let f = LilypondTest.derive_measures ["clef"]
    equal (f
            [ (">s/1 | clef = 'bass'", [(0, 2, ""), (2, 6, "clef = 'alto' |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right ["\\clef bass c'2 \\clef alto c'2~", "c'1"], [])

    -- test annotation promote
    -- The first clef and key are moved ahead of rests.
    equal (f $ UiTest.note_spec ("s/1", [(1, 3, "4c")], []))
        (Right ["\\clef treble r4 c'2."], [])

    -- Even if there are measures of rests.
    equal (f $ UiTest.note_spec ("s/1", [(5, 3, "4c")], []))
        (Right ["\\clef treble R1", "r4 c'2."], [])

test_key = do
    let f = LilypondTest.derive_measures ["key"]
    equal (f
            [ (">s/1 | key = 'a-mixo'", [(0, 2, ""), (2, 2, "key = 'c-maj' |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right ["\\key a \\mixolydian c'2 \\key c \\major c'2"], [])

test_ly_prepend_append = do
    let f env = LilypondTest.convert_measures [] $
            map environ_event [(0, 12, "a", env)]
    equal (f []) $ Right ["a1~", "a1~", "a1"]
    equal (f [(Lilypond.v_ly_append_first, "x")]) $
        Right ["a1~x", "a1~", "a1"]
    equal (f [(Lilypond.v_ly_append_last, "x")]) $
        Right ["a1~", "a1~", "a1x"]
    equal (f [(Lilypond.v_ly_append_all, "x")]) $
        Right ["a1~x", "a1~x", "a1x"]
    equal (f [(Lilypond.v_ly_prepend, "x")]) $
        Right ["xa1~", "a1~", "a1"]

test_ly_code = do
    -- Test stand-alone zero-dur code fragments.
    let f = LilypondTest.measures []
            . LilypondTest.derive_tracks_with_ui calls id
    -- prepend
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        (Right ["<a' c'>4 pre <b' d'>4 r2"], [])
    -- append
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        (Right ["<a' c'>4 <b' d'>4 post r2"], [])
    -- prepend and append alone
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (2, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.note_track [(0, 1, "4c"), (2, 1, "4d")])
        (Right ["<a' c'>4 pre post r4 <b' d'>4 r4"], [])
    where
    calls = CallTest.with_note_call "pre" c_pre
        . CallTest.with_note_call "post" c_post
    c_pre = CallTest.generator $ \args ->
        Lily.code0 (Args.start args) (Lily.Prefix, "pre")
    c_post = CallTest.generator $ \args ->
        Lily.code0 (Args.start args) (Lily.SuffixAll, "post")

test_allowed_time_greedy = do
    let f meter = extract_rhythms
            . map (Lilypond.allowed_time_greedy True (mkmeter meter))
        t = Lilypond.dur_to_time

    -- 4/4, being duple, is liberal about spanning beats, since it uses rank-2.
    equal (f "4/4" [0, t D4 .. 4 * t D4]) "1 2. 2 4 1"
    equal (f "4/4" [0, t D8 .. 8 * t D8])
        "1 4. 2. 8 2 4. 4 8 1"

    -- 6/8 is more conservative.
    equal (f "3+3/8" [0, t D8 .. 6 * t D8])
        "2. 4 8 4. 4 8 2."

    -- Irregular meters don't let you break the middle dividing line no matter
    -- what, because 'Meter.find_rank' always stops at rank 0.
    equal (f "3+2/4" [0, t D8 .. 10 * t D8])
        "2. 8 2 8 4 8 2 8 4 8 2."
        -- This has 8 after the middle 2 instead of 4. like 4/4 would have.
        -- That's because it's not duple, so it's more conservative.
        -- I guess that's probably ok.

test_allowed_time_best = do
    let f use_dot meter = extract_rhythms
            . map (Lilypond.allowed_time_best use_dot (mkmeter meter))
        t = Lilypond.dur_to_time
    equal (f False "4/4" [0, t D4 .. 4 * t D4])
        "1 4 2 4 1"
    equal (f False "4/4" [0, t D8 .. 8 * t D8])
        "1 8 4 8 2 8 4 8 1"
    equal (f True "3+3/8" [0, t D8 .. 6 * t D8])
        "2. 4 8 4. 4 8 2."

extract_rhythms :: [Lilypond.Time] -> String
extract_rhythms = unwords
        . map (Lilypond.to_lily . expect1 . Lilypond.time_to_note_durs)
    where
    expect1 [x] = x
    expect1 xs = error $ "expected only one element: " ++ show xs

-- * test lilypond derivation

-- These actually test derivation in lilypond mode.  So maybe they should go
-- in derive, but if I put them here I can test all the way to lilypond score.

test_meter = do
    let run meter notes = LilypondTest.derive_measures ["time"] $
            (">ly-global", meter) : UiTest.note_track notes
    equal (run [(0, 0, "meter '2/4'"), (2, 0, "meter '4/4'")]
            [(0, 3, "4a"), (3, 3, "4b")])
        (Right ["\\time 2/4 a'2~", "\\time 4/4 a'4 b'2."], [])
    -- Meter change is rounded up to the next barline, as usual.
    equal (run [(0, 0, "meter '2/4'"), (1, 0, "meter '4/4'")]
            [(0, 3, "4a"), (3, 3, "4b")])
        (Right ["\\time 2/4 a'2~", "\\time 4/4 a'4 b'2."], [])

test_enharmonics = do
    let (events, logs) = LilypondTest.derive_measures [] $ UiTest.note_track
            [(0, 1, "4c#"), (1, 1, "4db"), (2, 1, "4cx")]
    equal logs []
    equal events $ Right ["cs'4 df'4 css'4 r4"]

test_tempo = do
    -- Lilypond derivation is unaffected by the tempo.
    let (events, logs) = LilypondTest.extract extract $ LilypondTest.derive
            [ ("tempo", [(0, 0, "3")])
            , (">s/1", [(0, 4, ""), (4, 4, "")])
            , ("*", [(0, 0, "4c")])
            ]
        extract e = (Lilypond.event_start e, Lilypond.event_duration e)
    equal logs []
    equal events [(0, whole), (whole, whole)]

test_attributes = do
    -- Test the attribute-adding calls and 'Lilypond.attrs_to_code'.
    let f = LilypondTest.derive_measures []
    equal (f
        [ (">", [(0, 1, "+mute"), (1, 1, "o"), (2, 1, "")])
        , ("*", [(0, 0, "4a"), (1, 0, "4b"), (2, 0, "4c")])
        ])
        (Right ["a'4-+ b'4-\\flageolet c'4 r4"], [])

test_modal_attributes = do
    let f = LilypondTest.derive_measures []
    equal (f
        [ (">", [(0, 1, "+pizz"), (1, 1, "+pizz"), (2, 1, "")])
        , ("*", [(0, 0, "4c")])
        ])
        (Right ["c'4^\"pizz.\" c'4 c'4^\"arco\" r4"], [])

test_prepend_append = do
    let f = LilypondTest.derive_measures ["p", "mf"]
    equal (f $
            (">", [(0, 0, "dyn p"), (2, 0, "dyn mf")]) : UiTest.note_track
            [(0, 1, "4a"), (1, 1, "4b"), (2, 1, "4c"), (3, 1, "4d")])
        (Right ["a'4 \\p b'4 c'4 \\mf d'4"], [])

-- * util

compile_ly :: [Lilypond.Event] -> IO ()
compile_ly events = do
    writeFile "build/test/test.ly" (LilypondTest.make_ly events)
    void $ Process.rawSystem
        "lilypond" ["-o", "build/test/test", "build/test/test.ly"]

read_note :: String -> Lilypond.Note
read_note text
    | pitch == "r" = Lilypond.make_rest False dur
    | otherwise = Lilypond.Note
        { Lilypond._note_pitch = [pitch]
        , Lilypond._note_full_measure = False
        , Lilypond._note_duration = dur
        , Lilypond._note_tie = tie == "~"
        , Lilypond._note_prepend = ""
        , Lilypond._note_append = ""
        , Lilypond._note_stack = Nothing
        }
    where
    (pitch, rest) = break Char.isDigit text
    (dur_text, tie) = break (=='~') rest
    Just dur = flip Lilypond.NoteDuration False <$>
        Lilypond.read_duration dur_text

simple_event :: (RealTime, RealTime, String) -> Lilypond.Event
simple_event (start, dur, pitch) = make_event start dur pitch default_inst []

environ_event :: (RealTime, RealTime, String, [(TrackLang.ValName, String)])
    -> Lilypond.Event
environ_event (start, dur, pitch, env) =
    make_event start dur pitch default_inst env

meter_event :: (RealTime, String) -> Lilypond.Event
meter_event (start, meter) =
    make_event start 0 "" Lilypond.ly_global [(Lilypond.v_meter, meter)]

make_event :: RealTime -> RealTime -> String -> Score.Instrument
    -> [(TrackLang.ValName, String)] -> Lilypond.Event
make_event start dur pitch inst env = Lilypond.Event
    { Lilypond.event_start = Lilypond.real_to_time 1 start
    , Lilypond.event_duration = Lilypond.real_to_time 1 dur
    , Lilypond.event_pitch = pitch
    , Lilypond.event_instrument = inst
    , Lilypond.event_environ = TrackLang.make_environ
        [(name, TrackLang.to_val val) | (name, val) <- env]
    , Lilypond.event_stack = UiTest.mkstack (1, 0, 1)
    , Lilypond.event_clipped = False
    }

default_inst :: Score.Instrument
default_inst = Score.Instrument "test"

mkmeter :: String -> Meter.Meter
mkmeter s = case Meter.parse_meter s of
    Left err -> error $ "can't parse " ++ show s ++ ": " ++ err
    Right val -> val

whole :: Lilypond.Time
whole = Lilypond.time_per_whole
