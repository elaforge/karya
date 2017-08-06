-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Process_test where
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Perform.Lilypond.Process as Process
import Perform.Lilypond.Process (Voice(..))
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Duration(..))

import Global


test_rests_until = do
    let f meters =
            either (Left . untxt)
                (Right . Text.unwords . map Types.to_lily . fst)
            . Process.run_convert (LilypondTest.mkstate meters)
            . Process.rests_until . sum . map Types.dur_to_time
    equal (f ["4/4"] [D2]) (Right "r2")
    equal (f ["4/4"] [D2, D4]) (Right "r2 r4")
    equal (f ["4/4"] [D1]) (Right "R4*4")
    left_like (f ["4/4"] [D1, D1]) "out of meters"
    equal (f ["4/4", "4/4"] [D1, D1]) (Right "R4*4 | R4*4")
    -- Meters change.
    left_like (f ["2/4", "4/4"] [D1, D1]) "out of meters"
    equal (f ["2/4", "4/4"] [D1]) (Right "R4*2 | \\time 4/4 r2")

    -- Test full-measure rests.
    equal (f ["6/4", "6/4"] [D1, D1, D1]) $ Right "R4*6 | R4*6"
    equal (f (replicate 3 "4/4") [D1, D1, D2]) $ Right "R4*4 | R4*4 | r2"

test_process = do
    let f wanted meters = LilypondTest.extract_simple wanted
            . LilypondTest.process_meters meters . map LilypondTest.simple_event
        simple = LilypondTest.process_simple [] . map LilypondTest.simple_event

    equal (simple [(1, 1, "a"), (2, 8, "b")]) $
        Right "r4 a4 b2~ | b1~ | b2 r2"
    -- Rests are not dotted, even when they could be.
    -- I also get r8 r4, instead of r4 r8, see comment on 'allowed_time'.
    equal (simple [(0, 1, "a"), (1.5, 1, "b"), (4, 1, "c")]) $
        Right "a4 r8 b8~ b8 r8 r4 | c4 r4 r2"
    equal (simple [(0, 2, "a"), (3.5, 0.25, "b"), (3.75, 0.25, "c")]) $
        Right "a2 r4 r8 b16 c16"
    equal (simple [(0, 0.5, "a"), (0.5, 1, "b"), (1.5, 0.5, "c")]) $
        Right "a8 b4 c8 r2"
    -- Skip a couple of measures.
    equal (simple [(7, 1, "a"), (8, 1, "b")]) $
        Right "R4*4 | r2 r4 a4 | b4 r4 r2"
    equal (simple [(8, 1, "a"), (9, 1, "b")]) $
        Right "R4*4 | R4*4 | a4 b4 r2"
    equal (f [] ["4/4"] []) $ Right "R4*4"
    equal (f [] ["4/4", "4/4"] []) $ Right "R4*4 | R4*4"

    equal (f ["time", "key"] ["4/4"] [(0, 1, "a")]) $
        Right "\\time 4/4 \\key c \\major a4 r4 r2"
    equal (f ["time", "key"] ["4/4", "4/4"] [(0, 8, "a")]) $
        Right "\\time 4/4 \\key c \\major a1~ | a1"

    -- Key and meter are still at the beginning.
    equal (f ["time", "key"] ["4/4"] [(1, 1, "a")]) $
        Right "\\time 4/4 \\key c \\major r4 a4 r2"

    -- Meter change.
    equal (f ["time"] ["2/4", "4/4"] [(0, 6, "a")]) $
        Right "\\time 2/4 a2~ | \\time 4/4 a1"

test_meters = do
    let f wanted meters = LilypondTest.extract_simple wanted
            . LilypondTest.process_meters meters . map LilypondTest.simple_event
    equal (f ["time", "bar"] ["4/4", "3/4", "4/4"] []) $
        Right "\\time 4/4 R4*4 | \\time 3/4 R4*3 | \\time 4/4 R4*4 \\bar \"|.\""

test_dotted_rests = do
    let f meter = LilypondTest.extract_simple []
            . LilypondTest.process_meters [meter]
            . map LilypondTest.simple_event
    -- Rests are allowed to be dotted when the meter isn't duple.
    equal (f "3+3/8" [(1.5, 0.5, "a")]) $ Right "r4. a8 r4"
    equal (f "4/4" [(3, 1, "a")]) $ Right "r2 r4 a4"

test_chords = do
    let f = LilypondTest.process_simple [] . map LilypondTest.simple_event
    -- Homogenous durations.
    equal (f [(0, 1, "a"), (0, 1, "c")]) $ Right "<a c>4 r4 r2"
    -- Starting at the same time.
    equal (f [(0, 2, "a"), (0, 1, "c")]) $ Right "<a~ c>4 a4 r2"
    -- Starting at different times.
    equal (f [(0, 2, "a"), (1, 1, "c")]) $ Right "a4~ <a c>4 r2"
    equal (f [(0, 2, "a"), (1, 2, "c"), (2, 2, "e")]) $
        Right "a4~ <a c~>4 <c e~>4 e4"
    -- Only some notes in the chord are tied.
    equal (f [(0, 8, "a"), (0, 4, "b"), (4, 4, "b")]) $
        Right "<a~ b>1 | <a b>1"

test_code_events = do
    let f = LilypondTest.process_simple ["a", "b"]
            . map LilypondTest.environ_event
        prepend = [(Constants.v_ly_prepend, Typecheck.to_val ("a" :: Text))]
        append = [(Constants.v_ly_append_all, Typecheck.to_val ("b" :: Text))]
    -- Code that falls in the middle of rests.
    equal (f [(0, 0, "?", append), (0, 0, "?", prepend)])
        (Right "a b")
    equal (f [(0, 0, "?", append), (1, 0, "?", prepend), (2, 2, "a", [])])
        (Right "a r2 b a2")

    -- Code that falls in the middle of notes.
    equal (f [(0, 1, "a", []), (0.5, 0, "?", append), (0.5, 0, "?", prepend),
            (1, 1, "b", [])])
        (Right "a a4 b b4 r2")
    equal (f [(0, 8, "a", []), (2, 0, "?", append), (6, 0, "?", prepend),
            (8, 4, "b", [])])
        (Right "a1~ b | a a1 | b1")

    -- Code with duration.
    equal (f [(0, 1, "a", []), (2, 1, "", append), (4, 1, "b", [])])
        (Right "a4 r4 b r4 | b4 r4 r2")

test_voices = do
    let f wanted meters = LilypondTest.extract_lys (Just wanted)
            . LilypondTest.process_meters meters
            . map LilypondTest.voice_event

    equal (f [] ["4/4"]
            [ (0, 1, "a", Nothing)
            , (1, 1, "b", Just 1), (1, 1, "c", Just 2)
            , (2, 1, "d", Nothing)
            ]) $
        Right [Right "a4", Left [(VoiceOne, "b4"), (VoiceTwo, "c4")],
            Right "d4 r4"]
    -- Voices padded out to the longest one.
    equal (f [] ["4/4"]
            [(0, 2, "b", Just 1), (0, 1, "c", Just 2)]) $
        Right [Left [(VoiceOne, "b2"), (VoiceTwo, "c4 r4")], Right "r2"]

    -- Starting and ending in the middle of a measure works.
    equal (f [] ["4/4", "4/4"]
            [ (0, 2, "a", Nothing)
            , (2, 4, "b", Just 1), (2, 2, "c", Just 2)
            , (6, 2, "d", Nothing)
            ]) $
        Right
            [ Right "a2"
            , Left [(VoiceOne, "b2~ | b2"), (VoiceTwo, "c2 | r2")]
            , Right "d2"
            ]

    -- Changing meter in the middle works.
    equal (f ["time"] ["2/4", "4/4"]
            [(0, 4, "a", Just 2), (0, 4, "b", Just 1)]) $
        Right
            [ Right "\\time 2/4"
            , Left
                [ (VoiceOne, "b2~ | \\time 4/4 b2")
                , (VoiceTwo, "a2~ | \\time 4/4 a2")
                ]
            , Right "r2"
            ]

test_simplify_voices = do
    let f = fmap LilypondTest.unwords_right . LilypondTest.extract_lys (Just [])
            . LilypondTest.process . map LilypondTest.voice_event
    -- No voices.
    equal (f [(0, 4, "a", Nothing)]) $
        Right [Right "a1"]
    -- Simple voices.
    equal (f [(0, 4, "a", Just 1), (0, 4, "b", Just 2)]) $
        Right [Left [(VoiceOne, "a1"), (VoiceTwo, "b1")]]
    -- Just one voice is omitted entirely.
    equal (f [(0, 2, "a", Just 1), (8, 4, "b", Just 1)]) $
        Right [Right "a2 r2 | R4*4 | b1"]
    -- Empty measures are stripped, and the single voice is then flattened.
    equal (f [(0, 2, "a", Just 1), (8, 4, "b", Just 1), (8, 4, "c", Just 2)]) $
        Right
            [ Right "a2 r2 | R4*4 |"
            , Left [(VoiceOne, "b1"), (VoiceTwo, "c1")]
            ]
    -- Filter out non-present voices.
    equal (f
            [ (0, 4, "a", Just 1), (0, 4, "b", Just 2), (0, 4, "c", Just 3)
            , (4, 4, "d", Just 1), (4, 4, "e", Just 2)
            ]) $
        Right
            [ Left [(VoiceOne, "a1 |"), (VoiceTwo, "b1 |")
                , (VoiceThree, "c1 |")]
            , Left [(VoiceOne, "d1"), (VoiceTwo, "e1")]
            ]

test_voices_and_code = do
    let f wanted = LilypondTest.extract_lys (Just wanted)
            . LilypondTest.process . map LilypondTest.environ_event
        v n = (EnvKey.voice, BaseTypes.num n)
        append text = (Constants.v_ly_append_all, BaseTypes.str text)

    -- Code events are assigned to the first voice.
    equal (f ["mf"]
            [ (0, 0, "", [append "\\mf"])
            , (0, 1, "b", [v 1])
            , (0, 1, "c", [v 2])
            ]) $
        Right
            [ Left [(VoiceOne, "b4 \\mf"), (VoiceTwo, "c4")]
            , Right "r4 r2"
            ]

    -- But code afterwards doesn't get included.
    equal (f ["mf"]
            [ (0, 1, "b", [v 1])
            , (0, 1, "c", [v 2])
            , (1, 0, "", [append "\\mf"])
            , (1, 1, "d", [])
            ]) $
        Right
            [ Left [(VoiceOne, "b4"), (VoiceTwo, "c4")]
            , Right "d4 \\mf r2"
            ]

    -- Code isn't lost if the voice is simplified away.
    equal (f ["mf"]
            [ (0, 0, "", [append "\\mf"])
            , (0, 4, "a", [v 2])
            , (4, 4, "b", [v 1])
            ]) $
        Right [Right "a1 \\mf | b1"]

test_attrs_to_code = do
    let f = Process.attrs_to_code
    equal (f (Just Attrs.nv) Attrs.accent) (["->", "^\"vib\""], Attrs.accent)
    equal (f (Just Attrs.nv) Attrs.staccato)
        (["-."], (Attrs.staccato <> Attrs.nv))

    let run es = LilypondTest.process_simple []
            [ LilypondTest.attrs_event (t, 1, Text.singleton p, attrs)
            | (t, p, attrs) <- zip3 (Seq.range_ 0 1) "abcdefg" es
            ]
    equal (run [Attrs.staccato, Attrs.accent]) (Right "a4-. b4-> r2")
    equal (run [Attrs.nv, Attrs.nv, mempty])
        (Right "a4^\"nv\" b4 c4^\"vib\" r4")
    -- Even though staccato doesn't have +nv, I don't bother turning it off.
    equal (run [Attrs.nv, Attrs.staccato, mempty])
        (Right "a4^\"nv\" b4-. c4^\"vib\" r4")
