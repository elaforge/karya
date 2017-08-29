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
import Perform.Lilypond.LilypondTest (a3, b3, c3, d3, e3, f3)
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import Perform.Lilypond.Process (Voice(..))
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Duration(..))

import Global


test_rests_until = do
    let f meters = second (Text.unwords . map Types.to_lily . fst)
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

    equal (simple [(1, 1, a3), (2, 8, b3)]) $
        Right "r4 a4 b2~ | b1~ | b2 r2"
    -- Rests are not dotted, even when they could be.
    -- I also get r8 r4, instead of r4 r8, see comment on 'allowed_time'.
    equal (simple [(0, 1, a3), (1.5, 1, b3), (4, 1, c3)]) $
        Right "a4 r8 b8~ b8 r8 r4 | c4 r4 r2"
    equal (simple [(0, 2, a3), (3.5, 0.25, b3), (3.75, 0.25, c3)]) $
        Right "a2 r4 r8 b16 c16"
    equal (simple [(0, 0.5, a3), (0.5, 1, b3), (1.5, 0.5, c3)]) $
        Right "a8 b4 c8 r2"
    -- Skip a couple of measures.
    equal (simple [(7, 1, a3), (8, 1, b3)]) $
        Right "R4*4 | r2 r4 a4 | b4 r4 r2"
    equal (simple [(8, 1, a3), (9, 1, b3)]) $
        Right "R4*4 | R4*4 | a4 b4 r2"
    equal (f [] ["4/4"] []) $ Right "R4*4"
    equal (f [] ["4/4", "4/4"] []) $ Right "R4*4 | R4*4"

    equal (f ["time", "key"] ["4/4"] [(0, 1, a3)]) $
        Right "\\time 4/4 \\key c \\major a4 r4 r2"
    equal (f ["time", "key"] ["4/4", "4/4"] [(0, 8, a3)]) $
        Right "\\time 4/4 \\key c \\major a1~ | a1"

    -- Key and meter are still at the beginning.
    equal (f ["time", "key"] ["4/4"] [(1, 1, a3)]) $
        Right "\\time 4/4 \\key c \\major r4 a4 r2"

    -- Meter change.
    equal (f ["time"] ["2/4", "4/4"] [(0, 6, a3)]) $
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
    equal (f "3+3/8" [(1.5, 0.5, a3)]) $ Right "r4. a8 r4"
    equal (f "4/4" [(3, 1, a3)]) $ Right "r2 r4 a4"

test_chords = do
    let f = LilypondTest.process_simple [] . map LilypondTest.simple_event
    -- Homogenous durations.  Also, winds up as <c a> since a is higher than c.
    equal (f [(0, 1, a3), (0, 1, c3)]) $ Right "<c a>4 r4 r2"
    -- Starting at the same time.
    equal (f [(0, 2, a3), (0, 1, c3)]) $ Right "<c a~>4 a4 r2"
    -- Starting at different times.
    equal (f [(0, 2, a3), (1, 1, c3)]) $ Right "a4~ <c a>4 r2"
    equal (f [(0, 2, a3), (1, 2, c3), (2, 2, e3)]) $
        Right "a4~ <c~ a>4 <c e~>4 e4"
    -- Only some notes in the chord are tied.
    equal (f [(0, 8, a3), (0, 4, b3), (4, 4, b3)]) $
        Right "<a~ b>1 | <a b>1"

test_code_events = do
    let f = LilypondTest.process_simple ["a", "b"]
            . map LilypondTest.environ_event
        prepend = [(Constants.v_prepend, Typecheck.to_val ("a" :: Text))]
        append = [(Constants.v_append_all, Typecheck.to_val ("b" :: Text))]
    -- Code that falls in the middle of rests.
    equal (f [(0, 0, Nothing, append), (0, 0, Nothing, prepend)])
        (Right "a b")
    equal (f [(0, 0, Nothing, append), (1, 0, Nothing, prepend),
            (2, 2, Just a3, [])])
        (Right "a r2 b a2")

    -- Code that falls in the middle of notes.
    equal (f [(0, 1, Just a3, []), (0.5, 0, Nothing, append),
            (0.5, 0, Nothing, prepend), (1, 1, Just b3, [])])
        (Right "a a4 b b4 r2")
    equal (f [(0, 8, Just a3, []), (2, 0, Nothing, append),
            (6, 0, Nothing, prepend), (8, 4, Just b3, [])])
        (Right "a1~ b | a a1 | b1")

    -- Code with duration.
    equal (f [(0, 1, Just a3, []), (2, 1, Nothing, append),
            (4, 1, Just b3, [])])
        (Right "a4 r4 b r4 | b4 r4 r2")

test_voices = do
    let f wanted meters = LilypondTest.extract_lys (Just wanted)
            . LilypondTest.process_meters meters
            . map LilypondTest.voice_event

    equal (f [] ["4/4"]
            [ (0, 1, a3, Nothing)
            , (1, 1, b3, Just 1), (1, 1, c3, Just 2)
            , (2, 1, d3, Nothing)
            ]) $
        Right [Right "a4", Left [(VoiceOne, "b4"), (VoiceTwo, "c4")],
            Right "d4 r4"]
    -- Voices padded out to the longest one.
    equal (f [] ["4/4"]
            [(0, 2, b3, Just 1), (0, 1, c3, Just 2)]) $
        Right [Left [(VoiceOne, "b2"), (VoiceTwo, "c4 r4")], Right "r2"]

    -- Starting and ending in the middle of a measure works.
    equal (f [] ["4/4", "4/4"]
            [ (0, 2, a3, Nothing)
            , (2, 4, b3, Just 1), (2, 2, c3, Just 2)
            , (6, 2, d3, Nothing)
            ]) $
        Right
            [ Right "a2"
            , Left [(VoiceOne, "b2~ | b2"), (VoiceTwo, "c2 | r2")]
            , Right "d2"
            ]

    -- Changing meter in the middle works.
    equal (f ["time"] ["2/4", "4/4"]
            [(0, 4, a3, Just 2), (0, 4, b3, Just 1)]) $
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
    equal (f [(0, 4, c3, Nothing)]) $
        Right [Right "c1"]
    -- Simple voices.
    equal (f [(0, 4, c3, Just 1), (0, 4, d3, Just 2)]) $
        Right [Left [(VoiceOne, "c1"), (VoiceTwo, "d1")]]
    -- Just one voice is omitted entirely.
    equal (f [(0, 2, c3, Just 1), (8, 4, d3, Just 1)]) $
        Right [Right "c2 r2 | R4*4 | d1"]
    -- Empty measures are stripped, and the single voice is then flattened.
    equal (f [(0, 2, c3, Just 1), (8, 4, d3, Just 1), (8, 4, e3, Just 2)]) $
        Right
            [ Right "c2 r2 | R4*4 |"
            , Left [(VoiceOne, "d1"), (VoiceTwo, "e1")]
            ]
    -- Filter out non-present voices.
    equal (f
            [ (0, 4, a3, Just 1), (0, 4, b3, Just 2), (0, 4, c3, Just 3)
            , (4, 4, d3, Just 1), (4, 4, e3, Just 2)
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
        append text = (Constants.v_append_all, BaseTypes.str text)

    -- Code events are assigned to the first voice.
    equal (f ["mf"]
            [ (0, 0, Nothing, [append "\\mf"])
            , (0, 1, Just c3, [v 1])
            , (0, 1, Just d3, [v 2])
            ]) $
        Right
            [ Left [(VoiceOne, "c4 \\mf"), (VoiceTwo, "d4")]
            , Right "r4 r2"
            ]

    -- But code afterwards doesn't get included.
    equal (f ["mf"]
            [ (0, 1, Just c3, [v 1])
            , (0, 1, Just d3, [v 2])
            , (1, 0, Nothing, [append "\\mf"])
            , (1, 1, Just e3, [])
            ]) $
        Right
            [ Left [(VoiceOne, "c4"), (VoiceTwo, "d4")]
            , Right "e4 \\mf r2"
            ]

    -- Code isn't lost if the voice is simplified away.
    equal (f ["mf"]
            [ (0, 0, Nothing, [append "\\mf"])
            , (0, 4, Just c3, [v 2])
            , (4, 4, Just d3, [v 1])
            ]) $
        Right [Right "c1 \\mf | d1"]

test_attrs_to_code = do
    let f = Process.attrs_to_code
    equal (f Attrs.nv Attrs.accent) (["->", "^\"vib\""], Attrs.accent)
    equal (f Attrs.nv Attrs.staccato) (["-."], (Attrs.staccato <> Attrs.nv))

    let run es = LilypondTest.process_simple []
            [ LilypondTest.attrs_event (t, 1, p, attrs)
            | (t, p, attrs) <- zip3 (Seq.range_ 0 1) [c3, d3, e3] es
            ]
    equal (run [Attrs.staccato, Attrs.accent]) (Right "c4-. d4-> r2")
    equal (run [Attrs.nv, Attrs.nv, mempty])
        (Right "c4^\"nv\" d4 e4^\"vib\" r4")
    -- Even though staccato doesn't have +nv, I don't bother turning it off.
    equal (run [Attrs.nv, Attrs.staccato, mempty])
        (Right "c4^\"nv\" d4-. e4^\"vib\" r4")

test_convert_tuplet = do
    let run = second extract
            . Process.process Types.default_config 0
                (replicate 2 Meter.default_meter)
        tuplet start score_dur real_dur =
            (LilypondTest.mkevent start 0 Nothing LilypondTest.default_inst [])
            { Types.event_environ = Constants.set_tuplet score_dur real_dur }
        e = LilypondTest.simple_event
        extract = Text.unwords . map Types.to_lily . strip_ly . map expect_right

    equal (run $ tuplet 0 3 4 : map e [(0, 1, c3), (1, 1, d3), (2, 1, e3)])
        (Right "\\tuplet 3/2 { c2 d2 e2 } | R4*4")
    -- leading and trailing rests
    equal (run [tuplet 0 3 4, e (1, 1, d3)])
        (Right "\\tuplet 3/2 { r2 d2 r2 } | R4*4")
    -- can't go past a barline
    left_like (run [tuplet 3 3 4, e (3, 1, c3)]) "tuplet: * past barline"

    -- Spell with a 6 because there are 6 notes:
    equal (run $ tuplet 0 3 4 :
            [ e (s, 0.5, p)
            | (s, p) <- zip (Seq.range_ 0 0.5) [a3, b3, c3, d3, e3, f3]
            ])
        (Right "\\tuplet 6/4 { a4 b4 c4 d4 e4 f4 } | R4*4")

    -- Nested tuplets.
    -- 0   .   1   .   2   .   3   .   4   .   |
    -- 0   .   .25 .   .5  .   .75 .   1   .   |
    -- t------------------------------>
    -- c------>
    --         t-------------->
    --         d-->e-->f-->

    -- TODO This is broken, because when I get to the inner tuplet, its notes
    -- have been stretched * 2 so its score_dur doesn't encompass the child
    -- notes any more.  I might be able to fix it by stretching
    -- ly-tuplet-score-dur inside the outer tuplet's notes, but I'm going to
    -- leave it be for now.  I've already spent too much time trying to figure
    -- this out, and tracklang can't generate nested tuplets properly anyway,
    -- as documented in Parent_test.test_tuplet_ly.
    -- let nested =
    --         [ tuplet 0 3 4, e (0, 1, "c")
    --         , tuplet 1 1.5 2
    --         , e (1, 0.5, "d"), e (1.5, 0.5, "e"), e (2, 0.5, "f")
    --         ]
    -- equal (run nested)
    --     (Right "\\tuplet 3/2 { c2 \\tuplet 3/2 { d2 e2 f2 } } | R4*4")

strip_ly :: [Process.Ly] -> [Process.Ly]
strip_ly lys
    | null post = pre
    | otherwise = Seq.rdrop_while is_bar $ drop 1 post
    where
    (pre, post) = break is_major lys
    is_major (Process.Code "\\key c \\major") = True
    is_major _ = False
    is_bar (Process.Code "\\bar \"|.\"") = True
    is_bar _ = False


-- | 1 bar of 4/4.
make_state :: Types.Time -> Process.State
make_state start =
    Process.make_state Types.default_config start [Meter.default_meter]
        Process.default_key
