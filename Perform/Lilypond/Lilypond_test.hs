module Perform.Lilypond.Lilypond_test where
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified System.Process as Process

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import Types


default_config :: Lilypond.Config
default_config = Lilypond.Config
    { Lilypond.config_dotted_rests = False
    , Lilypond.config_dynamics = []
    }

test_convert_measures = do
    let f = convert_staves [] . map simple_event

    equal (f [(0, 1, "a"), (1, 1, "b")]) $ Right [["a4", "b4", "r2"]]
    equal (f [(1, 1, "a"), (2, 8, "b")]) $ Right
        [["r4", "a4", "b2~"], ["b1~"], ["b2", "r2"]]
    -- Rests are not dotted, even when they could be.
    equal (f [(0, 1, "a"), (1.5, 1, "b")]) $ Right
        [["a4", "r8", "b8~", "b8", "r8", "r4"]]
    equal (f [(0, 2, "a"), (3.5, 0.25, "b"), (3.75, 0.25, "c")]) $ Right
        [["a2", "r4", "r8", "b16", "c16"]]
    equal (f [(0, 0.5, "a"), (0.5, 1, "b"), (1.5, 0.5, "c")]) $ Right
        [["a8", "b4", "c8", "r2"]]

test_change_time_signature = do
    let f = convert_staves ["time"] . map time_sig_event
    equal (f [(0, 5, "a", "4/4"), (6, 2, "b", "4/4")]) $ Right
        [["\\time 4/4", "a1~"], ["a4", "r4", "b2"]]
    -- Change signature on the measure boundary.
    equal (f [(0, 2, "a", "2/4"), (2, 2, "b", "4/4")]) $ Right
        [["\\time 2/4", "a2"], ["\\time 4/4", "b2", "r2"]]

    -- Time signature changes during a note.
    equal (f [(0, 3, "a", "2/4"), (3, 4, "b", "4/4")]) $ Right
        [["\\time 2/4", "a2~"], ["a4", "b4~"], ["\\time 4/4", "b2.", "r4"]]
    equal (f [(0, 3, "a", "2/4"), (4, 4, "b", "4/4")]) $ Right
        [["\\time 2/4", "a2~"], ["a4", "r4"], ["\\time 4/4", "b1"]]

    -- Inconsistent time signatures cause an error.
    let run = convert_staves [] . fst . derive . concatMap UiTest.note_spec
    left_like (run
            [ ("s/i1 | time-sig = '2/4'", [(0, 4, "4a")], [])
            , ("s/i2 | time-sig = '4/4'", [(0, 4, "4b")], [])
            ])
        "staff for >s/i2: inconsistent time signatures"

test_parse_error = do
    let f = convert_staves [] . map environ_event
    left_like (f [(0, 1, "a", [(TrackLang.v_key, "oot-greet")])]) "unknown key"
    left_like (f [(0, 1, "a", [(Lilypond.v_time_signature, "oot-greet")])])
        "signature must be"

test_chords = do
    let f = convert_staves [] . map simple_event
    -- Homogenous durations.
    equal (f [(0, 1, "a"), (0, 1, "c")]) $ Right
        [["<a c>4", "r4", "r2"]]
    -- Starting at the same time.
    equal (f [(0, 2, "a"), (0, 1, "c")]) $ Right
        [["<a c>4~", "a4", "r2"]]
    -- Starting at different times.
    equal (f [(0, 2, "a"), (1, 1, "c")]) $ Right
        [["a4~", "<a c>4", "r2"]]
    equal (f [(0, 2, "a"), (1, 2, "c"), (2, 2, "e")]) $ Right
        [["a4~", "<a c>4~", "<c e>4~", "e4"]]

test_extract_time_signatures = do
    let f = fmap (map Pretty.pretty) . Lilypond.extract_time_signatures
            . map (\(s, d, tsig) -> time_sig_event (s, d, "a", tsig))
    equal (f [(0, 1, "4/4")]) $ Right ["4/4"]
    equal (f [(0, 5, "4/4")]) $ Right ["4/4", "4/4"]
    equal (f [(5, 5, "4/4")]) $ Right ["4/4", "4/4", "4/4"]
    equal (f [(5, 5, "6/4")]) $ Right ["6/4", "6/4"]
    equal (f [(0, 2, "4/4"), (2, 4, "6/4")]) $ Right ["4/4", "6/4"]
    equal (f [(0, 2, "4/4"), (1, 2, "4/4"), (2, 2, "4/4")]) $ Right ["4/4"]

test_convert_duration = do
    let f sig pos = Lilypond.to_lily $ head $
            Lilypond.convert_duration sig True pos (whole - pos)
    equal (map (f (sig 4 4)) [0, 8 .. 127])
        [ "1", "8.", "4.", "16", "2.", "8.", "8", "16"
        -- mid-measure
        , "2", "8.", "4.", "16", "4", "8.", "8", "16"
        ]
    equal (map (f (sig 2 4)) [0, 8 .. 127]) $
        concat $ replicate 2 ["2", "8.", "4.", "16", "4", "8.", "8", "16"]

test_make_ly = do
    let (events, logs) = derive $ concatMap UiTest.note_spec
            -- complicated rhythm
            [ ("s/i1", [(0, 1, "4c"), (1.5, 2, "4d#")], [])
            -- rhythm starts after 0, long multi measure note
            , ("s/i2", [(1, 1, "4g"), (2, 12, "3a")], [])
            ]
    equal logs []
    -- Shorter staff is padded out to the length of the longer one.
    equal (convert_events [] events) $ Right
        [ ("i1",
            [[["c'4", "r8", "ds'8~", "ds'4.", "r8"], ["r1"], ["r1"], ["r1"]]])
        , ("i2",
            [[["r4", "g'4", "a2~"], ["a1~"], ["a1~"], ["a2", "r2"]]])
        ]
    -- putStrLn $ make_ly events
    -- compile_ly events

test_hands = do
    let (events, logs) = derive $ concatMap UiTest.note_spec
            [ (">s/1 | hand = 'right'", [(0, 4, "4c")], [])
            , (">s/1 | hand = 'left'", [(0, 4, "4d")], [])
            , (">s/2", [(0, 4, "4e")], [])
            ]
    equal logs []
    -- Right hand goes in first.
    equal (convert_events [] events) $ Right
        [ ("1", [[["c'1"]], [["d'1"]]])
        , ("2", [[["e'1"]]])
        ]

test_clefs = do
    let f = first (convert_staves ["clef"]) . derive
    equal (f
            [ (">s/1 | clef = 'bass'", [(0, 2, ""), (2, 6, "clef = 'alto' |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right [["\\clef bass", "c'2", "\\clef alto", "c'2~"], ["c'1"]], [])

    -- test annotation promote
    -- The first clef and key are moved ahead of rests.
    equal (f $ UiTest.note_spec ("s/1", [(1, 3, "4c")], []))
        (Right [["\\clef treble", "r4", "c'2."]], [])

    -- Even if there are measures of rests.
    equal (f $ UiTest.note_spec ("s/1", [(5, 3, "4c")], []))
        (Right [["\\clef treble", "r1"], ["r4", "c'2."]], [])

test_key = do
    let f = first (convert_staves ["key"]) . derive
    equal (f
            [ (">s/1 | key = 'a-mixo'", [(0, 2, ""), (2, 2, "key = 'c-maj' |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right [["\\key a \\mixolydian", "c'2", "\\key c \\major", "c'2"]], [])

-- * test lilypond derivation

-- These actually test derivation in lilypond mode.  So maybe they should go
-- in derive, but if I put them here I can test all the way to lilypond score.

test_tempo = do
    -- Lilypond derivation is unaffected by the tempo.
    let (events, logs) = derive
            [ ("tempo", [(0, 0, "3")])
            , (">s/1", [(0, 4, ""), (4, 4, "")])
            , ("*", [(0, 0, "4c")])
            ]
        extract e = (Lilypond.event_start e, Lilypond.event_duration e)
    equal logs []
    equal (map extract events) [(0, whole), (whole, whole)]

test_trill = do
    let (events, logs) = derive
            [ (">s/1", [(0, 2, "tr"), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ]
        extract e = (Lilypond.event_start e, Lilypond.event_pitch e,
            Lilypond.event_attributes e)
    equal logs []
    equal (map extract events) [(0, "c'", Attrs.trill), (64, "d'", mempty)]
    putStrLn $ make_ly events

test_slur = do
    let (events, logs) = derive_linear True $
            (">", [(1, 2, "(")])
            : UiTest.note_spec ("s/1", [(n, 1, p) | (n, p) <-
                    zip (Seq.range_ 0 1) ["4a", "4b", "4c", "4d"]], [])
    equal logs []
    equal (map Lilypond.event_attributes events)
        [Score.no_attrs, Attrs.legato, Attrs.legato, Score.no_attrs]
    equal (fmap (map unwords) $ convert_staves [] events) $
        Right ["a'4 b'4( c'4) d'4"]

-- * util

compile_ly :: [Lilypond.Event] -> IO ()
compile_ly events = do
    writeFile "build/test/test.ly" (make_ly events)
    void $ Process.rawSystem
        "lilypond" ["-o", "build/test/test", "build/test/test.ly"]

read_note :: String -> Lilypond.Note
read_note text
    | pitch == "r" = Lilypond.rest dur
    | otherwise = Lilypond.Note
        { Lilypond._note_pitch = [pitch]
        , Lilypond._note_duration = dur
        , Lilypond._note_tie = tie == "~"
        , Lilypond._note_legato = False
        , Lilypond._note_code = ""
        , Lilypond._note_stack = Nothing
        }
    where
    (pitch, rest) = break Char.isDigit text
    (dur_text, tie) = break (=='~') rest
    Just dur = flip Lilypond.NoteDuration False <$>
        Lilypond.read_duration dur_text

simple_event :: (RealTime, RealTime, String) -> Lilypond.Event
simple_event (start, dur, pitch) = make_event start dur pitch "" []

environ_event :: (RealTime, RealTime, String, [(TrackLang.ValName, String)])
    -> Lilypond.Event
environ_event (start, dur, pitch, env) = make_event start dur pitch "" env

time_sig_event :: (RealTime, RealTime, String, String) -> Lilypond.Event
time_sig_event (s, d, p, tsig) =
    environ_event (s, d, p, [(Lilypond.v_time_signature, tsig)])

make_event :: RealTime -> RealTime -> String -> String
    -> [(TrackLang.ValName, String)] -> Lilypond.Event
make_event start dur pitch inst env = Lilypond.Event
    { Lilypond.event_start = Convert.real_to_time 1 start
    , Lilypond.event_duration = Convert.real_to_time 1 dur
    , Lilypond.event_pitch = pitch
    , Lilypond.event_instrument = Score.Instrument inst
    , Lilypond.event_dynamic = 0.5
    , Lilypond.event_environ = TrackLang.make_environ
        [(name, TrackLang.VString val) | (name, val) <- env]
    , Lilypond.event_stack = UiTest.mkstack (1, 0, 1)
    }

sig :: Int -> Int -> Lilypond.TimeSignature
sig num denom = Lilypond.TimeSignature num dur
    where Just dur = Lilypond.read_duration (show denom)

whole :: Lilypond.Time
whole = Lilypond.time_per_whole

-- * derive

-- | (title, [Staff]) where Staff = [Measure] where Measure = [String]
type StaffGroup = (String, [[[String]]])

-- | Like 'convert_events', but extract the converted staves to lists of
-- measures.
convert_staves :: [String] -> [Lilypond.Event] -> Either String [[String]]
convert_staves wanted = fmap (concatMap (concat . snd)) . convert_events wanted

-- | Convert events to lilypond score.
convert_events ::
    [String] -- ^ only include lilypond backslash commands listed here
    -> [Lilypond.Event] -> Either String [StaffGroup]
convert_events wanted events =
    map extract_staves <$> Lilypond.convert_staff_groups default_config events
    where
    extract_staves (Lilypond.StaffGroup inst staves) =
        (Lilypond.inst_name inst, map extract_staff staves)
    extract_staff (Lilypond.Staff measures) =
        map (filter is_wanted . map Lilypond.to_lily) measures
    is_wanted ('\\':note) = takeWhile (/=' ') note `elem` wanted
    is_wanted _ = True

derive :: [UiTest.TrackSpec] -> ([Lilypond.Event], [String])
derive = derive_linear False

derive_linear :: Bool -> [UiTest.TrackSpec] -> ([Lilypond.Event], [String])
derive_linear linear tracks = (ly_events, extract_logs logs)
    where
    (ly_events, logs) = LEvent.partition $ Convert.convert 1 $
        Derive.r_events (derive_ly linear tracks)
    extract_logs = map DeriveTest.show_log . DeriveTest.trace_low_prio

derive_ly :: Bool -> [UiTest.TrackSpec] -> Derive.Result
derive_ly linear tracks =
    DeriveTest.derive_tracks_with_ui
        (Derive.with_val TrackLang.v_lilypond_derive "true")
        (with_linear . (State.config#State.default_#State.tempo #= 1))
        tracks
    where
    with_linear = if linear then DeriveTest.linear_skel tracks else id

make_ly :: [Lilypond.Event] -> String
make_ly events = Text.unpack $ Text.strip $ Text.concat $ fst $
    expect_right "make_ly" $
        Lilypond.make_ly Lilypond.default_config "title" events
