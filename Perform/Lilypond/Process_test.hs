-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Process_test where
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import Util.Test

import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import Perform.Lilypond.Constants (Attach(..), Position(..), Distribution(..))
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import Perform.Lilypond.LilypondTest (a3, b3, c3, d3, e3, f3)
import qualified Perform.Lilypond.Meter as Meter
import qualified Perform.Lilypond.Process as Process
import Perform.Lilypond.Process (Voice(..))
import qualified Perform.Lilypond.Types as Types

import Global
import Types


test_simple = do
    let run = process_simple . map LilypondTest.simple_event
    equal (run [(0, 1, c3)]) $ Right "c4 r4 r2"
    equal (run [(1, 1, a3), (2, 8, b3)]) $ Right "r4 a4 b2~ | b1~ | b2 r2"
    -- Rests are not dotted, even when they could be.
    equal (run [(0, 1, a3), (1.5, 1, b3), (4, 1, c3)]) $
        Right "a4 r8 b8~ b8 r8 r4 | c4 r4 r2"
    equal (run [(0, 2, a3), (3.5, 0.25, b3), (3.75, 0.25, c3)]) $
        Right "a2 r4 r8 b16 c16"
    equal (run [(0, 0.5, a3), (0.5, 1, b3), (1.5, 0.5, c3)]) $
        Right "a8 b4 c8 r2"
    -- Skip a couple of measures.
    equal (run [(7, 1, a3), (8, 1, b3)]) $
        Right "R4*4 | r2 r4 a4 | b4 r4 r2"
    equal (run [(8, 1, a3), (9, 1, b3)]) $
        Right "R4*4 | R4*4 | a4 b4 r2"

test_chords = do
    let run = process_simple . map LilypondTest.simple_event
    -- Homogenous durations.  Also, winds up as <c a> since a is higher than c.
    equal (run [(0, 1, a3), (0, 1, c3)]) $ Right "<c a>4 r4 r2"
    -- Starting at the same time.
    equal (run [(0, 2, a3), (0, 1, c3)]) $ Right "<c a~>4 a4 r2"
    -- Starting at different times.
    equal (run [(0, 2, a3), (1, 1, c3)]) $ Right "a4~ <c a>4 r2"
    equal (run [(0, 2, a3), (1, 2, c3), (2, 2, e3)]) $
        Right "a4~ <c~ a>4 <c e~>4 e4"
    -- Only some notes in the chord are tied.
    equal (run [(0, 8, a3), (0, 4, b3), (4, 4, b3)]) $
        Right "<a~ b>1 | <a b>1"
    equal (run [(0, 4, c3), (1, 1, d3)]) $
        Right "c4~ <c~ d>4 c2"

test_meters = do
    let run wanted meters = LilypondTest.extract_simple wanted
            . process (map LilypondTest.parse_meter meters)
            . map LilypondTest.simple_event
    equal (run [] ["4/4"] []) $ Right "R4*4"
    equal (run [] ["4/4", "4/4"] []) $ Right "R4*4 | R4*4"
    equal (run ["time", "key"] ["4/4"] [(0, 1, a3)]) $
        Right "\\time 4/4 \\key c \\major a4 r4 r2"
    equal (run ["time", "key"] ["4/4", "4/4"] [(0, 8, a3)]) $
        Right "\\time 4/4 \\key c \\major a1~ | a1"
    -- Key and meter are still at the beginning.
    equal (run ["time", "key"] ["4/4"] [(1, 1, a3)]) $
        Right "\\time 4/4 \\key c \\major r4 a4 r2"
    -- Meter change.
    equal (run ["time"] ["2/4", "4/4"] [(0, 6, a3)]) $
        Right "\\time 2/4 a2~ | \\time 4/4 a1"
    equal (run ["time", "bar"] ["4/4", "3/4", "4/4"] []) $
        Right "\\time 4/4 R4*4 | \\time 3/4 R4*3 | \\time 4/4 R4*4 \\bar \"|.\""

test_keys = do
    let run = LilypondTest.extract_simple ["key"]
            . process_44 . map key_event
        key_event (s, d, p, key) = LilypondTest.environ_event
            (s, d, Just p, [(EnvKey.key, Typecheck.to_val (key :: Text))])
    equal (run [(0, 4, c3, "c-min"), (4, 4, d3, "d-min")]) $
        Right "\\key c \\minor c1 | \\key d \\minor d1"
    -- Not interrupted by rests.
    equal (run [(0, 2, c3, "c-min"), (4, 4, d3, "c-min")]) $
        Right "\\key c \\minor c2 r2 | d1"

test_dotted_rests = do
    let run meter = LilypondTest.extract_simple []
            . process [LilypondTest.parse_meter meter]
            . map LilypondTest.simple_event
    -- Rests are allowed to be dotted when the meter isn't duple.
    equal (run "3+3/8" [(1.5, 0.5, a3)]) $ Right "r4. a8 r4"
    equal (run "4/4" [(3, 1, a3)]) $ Right "r2 r4 a4"

free_code :: Constants.FreeCodePosition -> Text -> [(Env.Key, BaseTypes.Val)]
free_code pos code = [(Constants.free_code_key pos, Typecheck.to_val code)]

mk_free_code :: RealTime -> Constants.FreeCodePosition -> Text -> Types.Event
mk_free_code start pos code =
    LilypondTest.environ_event (start, 0, Nothing, free_code pos code)

test_free_code = do
    let run = process_simple . map LilypondTest.environ_event
        prepend = free_code Constants.FreePrepend "pre"
        append = free_code Constants.FreeAppend "post"
    -- Nothing to attach to.
    equal (run [(0, 0, Nothing, append), (0, 0, Nothing, prepend)])
        (Right "post pre")
    equal (run [(0, 0, Nothing, append), (1, 0, Nothing, prepend),
            (2, 2, Just a3, [])])
        (Right "pre r2 post a2")

    -- Code that falls in the middle of notes.
    equal (run [(0, 1, Just a3, []), (0.5, 0, Nothing, append),
            (0.5, 0, Nothing, prepend), (1, 1, Just b3, [])])
        (Right "pre a4 post b4 r2")
    equal (run [(0, 8, Just a3, []), (2, 0, Nothing, append),
            (6, 0, Nothing, prepend), (8, 4, Just b3, [])])
        (Right "a1~ post | pre a1 | b1")

test_free_code_tuplet = do
    let run = LilypondTest.extract_simple ["tuplet"] . process_44
    let e = LilypondTest.simple_event
        append start = mk_free_code start Constants.FreeAppend "post"
    let triplet = mk_tuplet 0 3 4 : map e [(0, 1, c3), (1, 1, d3), (2, 1, e3)]
    equal (run $ triplet ++ [append 4, e (4, 4, f3)]) $
        Right "\\tuplet 3/2 { c2 d2 e2 } | f1 post"

note_code :: Constants.CodePosition -> Text -> [(Env.Key, BaseTypes.Val)]
note_code pos code = [(Constants.position_key pos, Typecheck.to_val code)]

test_note_code = do
    let run = process_simple . map LilypondTest.environ_event
        c a p d = note_code (Constants.CodePosition a p d) "x"
        notes e1 e2 = [(0, 8, Just c3, e1), (0, 8, Just d3, e2)]
    equal (run (notes (c Chord Prepend First) [])) (Right "x <c~ d~>1 | <c d>1")
    equal (run (notes (c Chord Append First) [])) (Right "<c~ d~>1 x | <c d>1")
    equal (run (notes (c Note Prepend First) [])) (Right "<x c~ d~>1 | <c d>1")
    equal (run (notes (c Note Append First) [])) (Right "<c~x d~>1 | <c d>1")
    equal (run (notes (c Chord Append Last) [])) (Right "<c~ d~>1 | <c d>1 x")
    equal (run (notes (c Chord Append All) [])) (Right "<c~ d~>1 x | <c d>1 x")

    -- Rests are split by code events.
    equal (run [(0, 0, Nothing, c Chord Prepend First), (4, 4, Just c3, [])])
        (Right "x R4*4 | c1")
    equal (run [(0, 0, Nothing, c Chord Append First), (4, 4, Just c3, [])])
        (Right "R4*4 x | c1")
    equal (run [(1, 0, Nothing, c Chord Append First), (4, 4, Just c3, [])])
        (Right "r4 r4 x r2 | c1")

    equal (run [(0, 0, Nothing, c Chord Append All), (0, 8, Just c3, [])])
        (Right "c1~ x | c1 x")
    equal (run [(0, 8, Just c3, []), (0, 0, Nothing, c Chord Append All)])
        (Right "c1~ x | c1 x")

    -- -- TODO implement
    -- left_like (run [(0, 8, Just c3, []), (1, 0, Nothing, c Chord Append All)])
    --     "note code without a note to attach"

test_modal_articulations = do
    let run = process_simple . map LilypondTest.attrs_event
    -- Not interrupted by rests.
    equal (run [(0, 1, c3, Attrs.pizz), (2, 1, d3, Attrs.pizz),
            (3, 1, e3, mempty)]) $
        Right "c4 ^\"pizz.\" r4 d4 e4 ^\"arco\""

test_attrs_to_code = do
    let f = Process.attrs_to_code
    equal (f Attrs.nv Attrs.accent) (["->", "^\"vib\""], Attrs.accent)
    equal (f Attrs.nv Attrs.staccato) (["-."], (Attrs.staccato <> Attrs.nv))

    let run es = process_simple
            [ LilypondTest.attrs_event (t, 1, p, attrs)
            | (t, p, attrs) <- zip3 (Seq.range_ 0 1) [c3, d3, e3] es
            ]
    equal (run [Attrs.staccato, Attrs.accent]) (Right "c4 -. d4 -> r2")
    equal (run [Attrs.nv, Attrs.nv, mempty])
        (Right "c4 ^\"nv\" d4 e4 ^\"vib\" r4")
    -- Even though staccato doesn't have +nv, I don't bother turning it off.
    equal (run [Attrs.nv, Attrs.staccato, mempty])
        (Right "c4 ^\"nv\" d4 -. e4 ^\"vib\" r4")

test_voices = do
    let run = LilypondTest.extract_lys [] . process_44
            . map LilypondTest.voice_event

    equal (run
            [ (0, 1, a3, Nothing)
            , (1, 1, b3, Just 1), (1, 1, c3, Just 2)
            , (2, 1, d3, Nothing)
            ]) $
        Right [Right "a4", Left [(VoiceOne, "b4"), (VoiceTwo, "c4")],
            Right "d4 r4"]

    -- Voices padded out to the longest one.
    equal (run [(0, 2, b3, Just 1), (0, 1, c3, Just 2)]) $
        Right [Left [(VoiceOne, "b2"), (VoiceTwo, "c4 r4")], Right "r2"]

    -- Starting and ending in the middle of a measure works.
    equal (run
            [ (0, 2, a3, Nothing)
            , (2, 4, b3, Just 1), (2, 2, c3, Just 2)
            , (6, 2, d3, Nothing)
            ]) $
        Right
            [ Right "a2"
            , Left [(VoiceOne, "b2~ | b2"), (VoiceTwo, "c2 | r2")]
            , Right "d2"
            ]

    -- Error if voice and non-voice overlap.
    left_like (run [(0, 2, a3, Nothing), (1, 3, b3, Just 1)])
        "last non-voice * overlaps first voice"
    left_like (run [(0, 2, a3, Just 1), (1, 3, b3, Nothing)])
        "last voice * overlaps first non-voice"

    equal (run
            [ (0, 1, a3, Just 1), (0, 1, b3, Just 2)
            , (1, 1, c3, Nothing)
            , (2, 1, e3, Just 1), (2, 1, f3, Just 2)
            ]) $
        Right
            [ Left [(VoiceOne, "a4"), (VoiceTwo, "b4")]
            , Right "c4"
            , Left [(VoiceOne, "e4"), (VoiceTwo, "f4")]
            , Right "r4"
            ]


test_voices_meter = do
    let run wanted meters = LilypondTest.extract_lys wanted
            . process (map LilypondTest.parse_meter meters)
            . map LilypondTest.voice_event
    -- Changing meter in the middle works.
    equal (run ["time"] ["2/4", "4/4"]
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
    let run = fmap LilypondTest.unwords_right . LilypondTest.extract_lys []
            . process_44 . map LilypondTest.voice_event
    -- No voices.
    equal (run [(0, 4, c3, Nothing)]) $
        Right [Right "c1"]
    -- Simple voices.
    equal (run [(0, 4, c3, Just 1), (0, 4, d3, Just 2)]) $
        Right [Left [(VoiceOne, "c1"), (VoiceTwo, "d1")]]
    -- Just one voice is omitted entirely.
    equal (run [(0, 2, c3, Just 1), (8, 4, d3, Just 1)]) $
        Right [Right "c2 r2 | R4*4 | d1"]
    -- Empty measures are stripped, and the single voice is then flattened.
    equal (run [(0, 2, c3, Just 1), (8, 4, d3, Just 1), (8, 4, e3, Just 2)]) $
        Right
            [ Right "c2 r2 | R4*4 |"
            , Left [(VoiceOne, "d1"), (VoiceTwo, "e1")]
            ]
    -- Filter out non-present voices.
    equal (run
            [ (0, 4, a3, Just 1), (0, 4, b3, Just 2), (0, 4, c3, Just 3)
            , (4, 4, d3, Just 1), (4, 4, e3, Just 2)
            ]) $
        Right
            [ Left [(VoiceOne, "a1 |"), (VoiceTwo, "b1 |")
                , (VoiceThree, "c1 |")]
            , Left [(VoiceOne, "d1"), (VoiceTwo, "e1")]
            ]

test_free_code_voices = do
    let run = LilypondTest.extract_lys []
            . process_44 . map LilypondTest.environ_event
        v n = (EnvKey.voice, BaseTypes.num n)
        append = free_code Constants.FreeAppend "post"
    -- Code events are assigned to the first voice.
    equal (run
            [ (0, 0, Nothing, append)
            , (0, 1, Just c3, [v 1])
            , (0, 1, Just d3, [v 2])
            ]) $
        Right
            [ Left [(VoiceOne, "c4 post"), (VoiceTwo, "d4")]
            , Right "r4 r2"
            ]
    -- But code afterwards doesn't get included.
    equal (run
            [ (0, 1, Just c3, [v 1])
            , (0, 1, Just d3, [v 2])
            , (1, 0, Nothing, append)
            , (1, 1, Just e3, [])
            ]) $
        Right
            [ Left [(VoiceOne, "c4"), (VoiceTwo, "d4")]
            , Right "e4 post r2"
            ]
    -- Code isn't lost if the voice is simplified away.
    equal (run
            [ (0, 0, Nothing, append)
            , (0, 4, Just c3, [v 2])
            , (4, 4, Just d3, [v 1])
            ]) $
        Right [Right "c1 post | d1"]

    equal (run
            [ (0, 2, Just c3, []), (2, 0, Nothing, append)
            , (2, 2, Just d3, [v 1]), (2, 2, Just e3, [v 2])
            ]) $
        Right [Right "c2", Left [(VoiceOne, "d2 post"), (VoiceTwo, "e2")]]

mk_tuplet :: RealTime -> RealTime -> RealTime -> Types.Event
mk_tuplet start score_dur real_dur =
    (LilypondTest.mkevent start real_dur Nothing LilypondTest.default_inst [])
    { Types.event_environ = Constants.set_tuplet score_dur real_dur }

test_convert_tuplet = do
    let run = second extract . process (replicate 2 Meter.default_meter)
        e = LilypondTest.simple_event
        extract = Text.unwords . map Types.to_lily . strip_ly . map expect_right

    equal (run $ mk_tuplet 0 3 4 : map e [(0, 1, c3), (1, 1, d3), (2, 1, e3)])
        (Right "\\tuplet 3/2 { c2 d2 e2 } | R4*4")
    -- leading and trailing rests
    equal (run [mk_tuplet 0 3 4, e (1, 1, d3)])
        (Right "\\tuplet 3/2 { r2 d2 r2 } | R4*4")
    -- can't go past a barline
    left_like (run [mk_tuplet 3 3 4, e (3, 1, c3)]) "tuplet: * past barline"

    -- Spell with a 6 because there are 6 notes:
    equal (run $ mk_tuplet 0 3 4 :
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
    --         [ mk_tuplet 0 3 4, e (0, 1, "c")
    --         , mk_tuplet 1 1.5 2
    --         , e (1, 0.5, "d"), e (1.5, 0.5, "e"), e (2, 0.5, "f")
    --         ]
    -- equal (run nested)
    --     (Right "\\tuplet 3/2 { c2 \\tuplet 3/2 { d2 e2 f2 } } | R4*4")

test_convert_tremolo = do
    let run = second extract . process_44 . map LilypondTest.environ_event
        tremolo start dur =
            ( start, dur, Nothing
            , [(Constants.v_tremolo, Typecheck.to_val ("" :: Text))]
            )
        attr a = [(EnvKey.attributes, BaseTypes.VAttributes a)]
        extract = Text.unwords . map Types.to_lily . strip_ly . map expect_right
    equal (run [tremolo 0 4, (0, 4, Just c3, []), (0, 4, Just d3, [])]) $
        Right "\\repeat tremolo 16 { c32( d32) }"
    equal (run [tremolo 0 8, (0, 8, Just c3, []), (0, 8, Just d3, [])]) $
        Right "\\repeat tremolo 16 { c32( d32) }\
            \ | \\repeat tremolo 16 { c32( d32) }"

    equal (run [tremolo 0 4,
            (0, 4, Just c3, attr Attrs.accent), (0, 4, Just d3, [])]) $
        Right "\\repeat tremolo 16 { c32( -> d32) }"
    let pre = note_code (Constants.CodePosition Chord Prepend First) "pre"
    equal (run [tremolo 0 4, (0, 4, Just c3, pre), (0, 4, Just d3, [])]) $
        Right "\\repeat tremolo 16 { pre c32( d32) }"
    let free_pre = free_code Constants.FreePrepend "pre"
    equal (run [tremolo 0 4, (0, 0, Nothing, free_pre),
            (0, 4, Just c3, []), (0, 4, Just d3, [])]) $
        Right "\\repeat tremolo 16 { pre c32( d32) }"

-- * util

strip_ly :: [Process.Ly] -> [Process.Ly]
strip_ly lys
    | null post = pre
    | otherwise = Seq.rdrop_while is_bar $ drop 1 post
    where
    (pre, post) = break is_major lys
    is_major (Process.LyCode "\\key c \\major") = True
    is_major _ = False
    is_bar (Process.LyCode "\\bar \"|.\"") = True
    is_bar _ = False

process_simple :: [Types.Event] -> Either Text Text
process_simple = LilypondTest.extract_simple [] . process_44

process_44 :: [Types.Event] -> Either Text [Either Process.Voices Process.Ly]
process_44 events = process
    (replicate (event_measures events) (LilypondTest.parse_meter "4/4"))
    events

event_measures :: [Types.Event] -> Int
event_measures = ceiling . LilypondTest.time_to_wholes . fromMaybe 0
    . Seq.maximum . map Types.event_end

process :: [Meter.Meter] -> [Types.Event]
    -> Either Text [Either Process.Voices Process.Ly]
process meters =
    first Log.msg_text . Process.process LilypondTest.default_config 0 meters
