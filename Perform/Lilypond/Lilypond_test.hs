module Perform.Lilypond.Lilypond_test where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text

import qualified System.Process as Process

import Util.Control
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Convert as Convert
import qualified Perform.Lilypond.Lilypond as Lilypond
import Types


config :: Lilypond.Config
config = Lilypond.Config False []

test_convert_notes = do
    let f sig = convert_notes sig
        s44 = sig 4 4
    equal (f s44 [(0, 1, "a"), (1, 1, "b")]) ["a4", "b4", "r2"]

    -- Rests are not dotted, even when they could be.
    equal (f s44 [(0, 1, "a"), (1.5, 1, "b")])
        ["a4", "r8", "b8~", "b8", "r8", "r4"]
    equal (f s44 [(0, 2, "a"), (3.5, 0.25, "b"), (3.75, 0.25, "c")])
        ["a2", "r4", "r8", "b16", "c16"]
    equal (f s44 [(0, 0.5, "a"), (0.5, 1, "b"), (1.5, 0.5, "c")])
        ["a8", "b4", "c8", "r2"]

test_chords = do
    let f = convert_notes (sig 4 4)
    -- Homogenous durations.
    equal (f [(0, 1, "a"), (0, 1, "c")])
        ["<a c>4", "r4", "r2"]
    -- Starting at the same time.
    equal (f [(0, 2, "a"), (0, 1, "c")])
        ["<a c>4~", "a4", "r2"]
    -- Starting at different times.
    equal (f [(0, 2, "a"), (1, 1, "c")])
        ["a4~", "<a c>4", "r2"]
    equal (f [(0, 2, "a"), (1, 2, "c"), (2, 2, "e")])
        ["a4~", "<a c>4~", "<c e>4~", "e4"]

convert_notes :: Lilypond.TimeSignature -> [(RealTime, RealTime, String)]
    -> [String]
convert_notes sig = filter (not . (=="\\") . take 1) . map Lilypond.to_lily
    . Lilypond.convert_notes config sig whole . map mkevent

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
    equal (event_staves_pitches events)
        [ ("i1",
            [[["c'4", "r8", "ds'8~", "ds'4.", "r8"], ["r1"], ["r1"], ["r1"]]])
        , ("i2",
            [[["r4", "g'4", "a2~"], ["a1~"], ["a1~"], ["a2", "r2"]]])
        ]

test_hands = do
    let (events, logs) = derive $ concatMap UiTest.note_spec
            [ (">s/1 | hand = 'right'", [(0, 4, "4c")], [])
            , (">s/1 | hand = 'left'", [(0, 4, "4d")], [])
            , (">s/2", [(0, 4, "4e")], [])
            ]
    equal logs []
    -- Right hand goes in first.
    equal (event_staves_pitches events)
        [ ("1", [[["c'1"]], [["d'1"]]])
        , ("2", [[["e'1"]]])
        ]

test_clefs = do
    let (events, logs) = derive
            [ (">s/1 | clef = 'bass'", [(0, 2, ""), (2, 6, "clef = 'alto' |")])
            , ("*", [(0, 0, "4c")])
            ]
    equal logs []
    equal (event_staves events)
        [("1", [[["\\clef bass", "c'2", "\\clef alto", "c'2~"], ["c'1"]]])]

    -- The first clef is moved ahead of rests.
    let staves = event_staves $ fst $ derive
            [(">s/1", [(1, 3, "")]), ("*", [(0, 0, "4c")])]
    equal staves [("1", [[["\\clef treble", "r4", "c'2."]]])]

test_key = do
    let (events, logs) = derive
            [ (">s/1 | key = 'a-mixo'", [(0, 2, ""), (2, 2, "key = 'c-maj' |")])
            , ("*", [(0, 0, "4c")])
            ]
    equal logs []
    equal (event_staves events)
        [("1", [[["\\clef treble", "\\key a \\mixolydian", "c'2",
            "\\key c \\major", "c'2"]]])]

test_tempo = do
    -- Lilypond derivation is unaffected by the tempo.
    let (events, logs) = derive
            [ ("tempo", [(0, 0, "3")])
            , (">s/1", [(0, 4, ""), (4, 4, "")])
            , ("*", [(0, 0, "4c")])
            ]
        extract e = (Lilypond.event_start e, Lilypond.event_duration e)
    equal logs []
    equal (map extract events)
        [(0, whole), (whole, whole)]

test_trill = do
    let (events, logs) = derive
            [ (">s/1", [(0, 2, "tr"), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4d")])
            ]
        extract e = (Lilypond.event_start e, Lilypond.event_pitch e,
            Lilypond.event_attributes e)
    equal logs []
    equal (map extract events)
        [(0, "c'", Attrs.trill), (64, "d'", mempty)]
    -- putStrLn $ fst $ make_ly default_score events

default_score :: Lilypond.Score
default_score = make_score "4/4"

-- * util

compile_ly :: [Lilypond.Event] -> IO ()
compile_ly events = do
    writeFile "build/test/test.ly" (fst (make_ly default_score events))
    void $ Process.rawSystem
        "lilypond" ["-o", "build/test/test", "build/test/test.ly"]

read_note :: String -> Lilypond.Note
read_note text
    | pitch == "r" = Lilypond.rest dur
    | otherwise = Lilypond.Note [pitch] dur (tie == "~") "" Nothing
    where
    (pitch, rest) = break Char.isDigit text
    (dur_text, tie) = break (=='~') rest
    Just dur = flip Lilypond.NoteDuration False <$>
        Lilypond.read_duration dur_text

make_score :: String -> Lilypond.Score
make_score time_sig = either (error . ("make_score: " ++)) id $ do
    tsig <- Lilypond.parse_time_signature time_sig
    return $ Lilypond.Score
        { Lilypond.score_title = "title"
        , Lilypond.score_time = tsig
        }

mkevent :: (RealTime, RealTime, String) -> Lilypond.Event
mkevent (start, dur, pitch) = mkevent_inst (start, dur, pitch, "")

mkevent_inst :: (RealTime, RealTime, String, String) -> Lilypond.Event
mkevent_inst (start, dur, pitch, inst) = Lilypond.Event
    { Lilypond.event_start = Convert.real_to_time 1 start
    , Lilypond.event_duration = Convert.real_to_time 1 dur
    , Lilypond.event_pitch = pitch
    , Lilypond.event_instrument = Score.Instrument inst
    , Lilypond.event_dynamic = 0.5
    , Lilypond.event_environ = mempty
    , Lilypond.event_stack = UiTest.mkstack (1, 0, 1)
    }

sig :: Int -> Int -> Lilypond.TimeSignature
sig num denom = Lilypond.TimeSignature num dur
    where Just dur = Lilypond.read_duration (show denom)

whole :: Lilypond.Time
whole = Lilypond.time_per_whole

-- * derive

derive :: [UiTest.TrackSpec] -> ([Lilypond.Event], [String])
derive tracks = (ly_events, extract_logs logs)
    where
    (ly_events, logs) = LEvent.partition $ Convert.convert 1 $
        Derive.r_events (derive_ly tracks)
    extract_logs = map DeriveTest.show_log . DeriveTest.trace_low_prio

-- TODO use Cmd.Lilypond.derive instead?
-- derive :: (Cmd.M m) => BlockId -> m Derive.Result
derive_ly :: [UiTest.TrackSpec] -> Derive.Result
derive_ly = DeriveTest.derive_tracks_with_ui
    (Derive.with_val TrackLang.v_lilypond_derive "true")
    (State.config#State.default_#State.tempo #= 1)

make_ly :: Lilypond.Score -> [Lilypond.Event] -> (String, Cmd.StackMap)
make_ly score events = (strip texts, stack_map)
    where
    strip = Text.unpack . Text.strip . mconcat
    (texts, stack_map) = Lilypond.make_ly Lilypond.default_config score events

-- ** staves

event_staves :: [Lilypond.Event] -> [(String, [[[String]]])]
event_staves = map extract_staff . derive_staves default_score

event_staves_pitches :: [Lilypond.Event] -> [(String, [[[String]]])]
event_staves_pitches = map (second filter_clefs) . event_staves
    where
    filter_clefs = map $ map $ filter (not . ("\\clef " `List.isPrefixOf`))

extract_staff :: Lilypond.StaffGroup -> (String, [[[String]]])
extract_staff (Lilypond.StaffGroup inst staves) =
    (Lilypond.inst_name inst, map extract staves)
    where
    extract (Lilypond.Staff measures) =
        map (map Lilypond.to_lily) measures

derive_staves :: Lilypond.Score -> [Lilypond.Event] -> [Lilypond.StaffGroup]
derive_staves score events = Lilypond.split_staves config
    (Lilypond.score_time score) events
