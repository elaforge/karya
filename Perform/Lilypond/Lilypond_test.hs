-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Lilypond_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Ly as Ly
import qualified Derive.Score as Score
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Perform.Lilypond.Types as Types

import Global


test_convert_sections = do
    let run = LilypondTest.derive_staves []
    let (events, logs) = run $ concatMap UiTest.note_spec
            -- complicated rhythm
            [ ("i1", [(0, 1, "4c"), (1.5, 2, "4d#")], [])
            -- rhythm starts after 0, long multi measure note
            , ("i2", [(1, 1, "4g"), (2, 12, "3a")], [])
            ]
    equal logs []
    -- Shorter staff is padded out to the length of the longer one, and then to
    -- the end of the bar.
    equal events $ Right
        [ ("i1", ["c'4 r8 ds'8~ ds'4. r8 | R4*4 | R4*4 | R4*4"])
        , ("i2", ["r4 g'4 a2~ | a1~ | a1~ | a2 r2"])
        ]

test_staff_configs = do
    let piano = Types.empty_staff_config
            { Types.staff_long = "piano"
            , Types.staff_short = ""
            , Types.staff_code = ["piano code"]
            }
        viola = Types.empty_staff_config
            { Types.staff_long = "viola"
            , Types.staff_short = "vla"
            , Types.staff_code = ["viola code"]
            }
        staves =
            [(Score.Instrument "i2", viola), (Score.Instrument "i1", piano)]
        config = Types.default_config { Types.config_staves = staves }
    let (text, logs) = make_ly config
            [ (">i1", [(0, 1, "")]), ("*", [(0, 0, "3c")])
            , (">i2", [(0, 1, "")]), ("*", [(0, 0, "3d")])
            ]
    equal logs []
    -- Viola goes first, has correct long and short names and code.
    match text
        "instrumentName = \"viola\"*shortInstrumentName = \"vla\"*viola code\
        \*instrumentName = \"piano\"*piano code"

test_add_bass_staff = do
    let config = Types.default_config
            { Types.config_staves = [(Score.Instrument "i1", staff_config)] }
        staff_config = Types.empty_staff_config
            { Types.staff_add_bass_staff = True }
    let (text, logs) = make_ly config $ UiTest.note_spec
            ("i1", [(0, 1, "clef treble | -- 4c"), (1, 3, "4d")], [])
    equal logs []
    -- Key and meter are in, but the clef is replaced.
    match text
        "new Staff = \"up\"\
        \*new Staff = \"down\" \\RemoveEmptyStaves\
        \*\\clef bass \\time 4/4 \\key c \\major s4 s2. \\bar \"|.\""

    -- FullMeasure rests are correctly converted to an empty track with hidden
    -- rests.
    let text = fst $ make_ly config $
            UiTest.note_spec ("i1", [(4, 1, "4c")], [])
    match text "*major R4\\*4 *major s1"

    -- -- Tuple becomes a full measure rest.
    -- -- TODO not fixed
    -- let run_linear = first (LilypondTest.make_ly config)
    --         . LilypondTest.partition_logs . LilypondTest.derive_tracks_linear
    -- let (text, logs) = run_linear $
    --         (">i1", [(0, 4, "t")]) : UiTest.note_track
    --             [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (4, 4, "4f")]
    -- equal logs []
    -- match text "major s1 |*s1"

test_hands = do
    let run = LilypondTest.derive_staves
    let (events, logs) = run [] $ concatMap UiTest.note_spec
            [ ("i1 | hand = r", [(0, 4, "4c")], [])
            , ("i1 | hand = l", [(0, 4, "4d")], [])
            , ("i2", [(0, 4, "4e")], [])
            ]
    equal logs []
    -- Right hand goes in first.
    equal events $ Right
        [ ("i1", ["c'1", "d'1"])
        , ("i2", ["e'1"])
        ]
    -- If there are code events for the hand, they get emitted.
    equal (run ["clef"]
            [ (">i1 | hand = r", [(0, 4, "")])
            , ("*", [(0, 0, "3c")])
            , (">i1 | hand = l", [(0, 0, "clef bass")])
            ])
        (Right [("i1", ["c1", "\\clef bass R4*4"])], [])

test_clefs = do
    let f = LilypondTest.derive_measures ["clef"]
    equal (f
            [ (">i1 | clef bass", [(0, 2, ""), (2, 6, "clef alto |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right "\\clef bass c'2 \\clef alto c'2~ | c'1", [])

test_key = do
    let f = LilypondTest.derive_measures ["key"]
    equal (f
            [ (">i1 | key = a-mixolydian",
                [(0, 2, ""), (2, 2, "key = c-maj |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right "\\key a \\mixolydian c'2 \\key c \\major c'2", [])

test_ly_prepend_append = do
    let f env = LilypondTest.convert_measures [] $
            map LilypondTest.environ_event [(0, 12, Just LilypondTest.a3, env)]
        str :: BaseTypes.Key -> Text -> [(BaseTypes.Key, BaseTypes.Val)]
        str key val = [(key, Typecheck.to_val val)]
    equal (f []) $ Right "a1~ | a1~ | a1"
    equal (f (str Constants.v_append_first "x")) $ Right "a1~x | a1~ | a1"
    equal (f (str Constants.v_append_last "x")) $ Right "a1~ | a1~ | a1x"
    equal (f (str Constants.v_append_all "x")) $ Right "a1~x | a1~x | a1x"
    equal (f (str Constants.v_prepend "x")) $ Right "x a1~ | a1~ | a1"

test_ly_code = do
    -- Test stand-alone zero-dur code fragments.
    let f = LilypondTest.measures [] . LilypondTest.derive_tracks_setup calls
    -- prepend
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        (Right "<c' a'>4 pre <d' b'>4 r2", [])
    -- append
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        (Right "<c' a'>4 <d' b'>4 post r2", [])
    -- prepend and append alone
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (2, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.note_track [(0, 1, "4c"), (2, 1, "4d")])
        (Right "<c' a'>4 pre r4 post <d' b'>4 r4", [])
    where
    calls = CallTest.with_note_generator "pre" c_pre
        <> CallTest.with_note_generator "post" c_post
    c_pre = CallTest.generator $ \args ->
        Ly.code0 (Args.start args) (Ly.Prepend, "pre")
    c_post = CallTest.generator $ \args ->
        Ly.code0 (Args.start args) (Ly.AppendAll, "post")

-- * test lilypond derivation

-- These actually test derivation in lilypond mode.  So maybe they should go
-- in derive, but if I put them here I can test all the way to lilypond score.

test_clip_block = do
    let run = LilypondTest.extract LilypondTest.e_note
            . LilypondTest.derive_blocks
        q = Types.dur_to_time Types.D4
    equal (run
            [ ("b1", [(">", [(0, 4, "sub")])])
            , ("sub=ruler", UiTest.regular_notes 4)
            ])
        ([(0, q, "c"), (q, q, "d"), (q*2, q, "e"), (q*3, q, "f")], [])
    equal (run
            [ ("b1", [(">", [(0, 2.5, "clip | sub")])])
            , ("sub=ruler", UiTest.regular_notes 4)
            ])
        ([(0, q, "c"), (q, q, "d"), (q*2, Types.dur_to_time Types.D8, "e")], [])

test_meter = do
    let run meter notes = LilypondTest.derive_measures ["time"] $
            (">", meter) : UiTest.note_track notes
    equal (run [(0, 0, "meter '2/4'"), (2, 0, "meter '4/4'")]
            [(0, 3, "4a"), (3, 3, "4b")])
        (Right "\\time 2/4 a'2~ | \\time 4/4 a'4 b'2.", [])
    -- Meter change is rounded up to the next barline, as usual.
    equal (run [(0, 0, "meter '2/4'"), (1, 0, "meter '4/4'")]
            [(0, 3, "4a"), (3, 3, "4b")])
        (Right "\\time 2/4 a'2~ | \\time 4/4 a'4 b'2.", [])

test_enharmonics = do
    let (events, logs) = LilypondTest.derive_measures [] $ UiTest.note_track
            [(0, 1, "4c#"), (1, 1, "4db"), (2, 1, "4cx")]
    equal logs []
    equal events $ Right "cs'4 df'4 css'4 r4"

test_tempo = do
    -- Lilypond derivation is unaffected by the tempo.
    let (events, logs) = LilypondTest.extract extract $
            LilypondTest.derive_tracks
                [ ("tempo", [(0, 0, "3")])
                , (">i1", [(0, 4, ""), (4, 4, "")])
                , ("*", [(0, 0, "4c")])
                ]
        extract e = (Types.event_start e, Types.event_duration e)
        whole = Types.time_per_whole
    equal logs []
    equal events [(0, whole), (whole, whole)]

test_attributes = do
    let f = LilypondTest.derive_measures []
    equal (f
        [ (">", [(0, 1, "+mute"), (1, 1, "+harm"), (2, 1, "")])
        , ("*", [(0, 0, "4a"), (1, 0, "4b"), (2, 0, "4c")])
        ])
        (Right "a'4-+ b'4-\\flageolet c'4 r4", [])

test_modal_attributes = do
    let f = LilypondTest.derive_measures [] . (:[("*", [(0, 0, "3c")])])
    equal (f (">", [(0, 1, "+pizz"), (1, 1, "+pizz"), (2, 1, "")]))
        (Right "c4^\"pizz.\" c4 c4^\"arco\" r4", [])

test_prepend_append = do
    let f = LilypondTest.derive_measures ["p", "mf"]
    equal (f $
            (">", [(0, 0, "dyn p"), (2, 0, "dyn mf")]) : UiTest.note_track
            [(0, 1, "4a"), (1, 1, "4b"), (2, 1, "4c"), (3, 1, "4d")])
        (Right "a'4 \\p b'4 c'4 \\mf d'4", [])

test_append_pitch = do
    let f = LilypondTest.derive_measures []
    -- Append pitch works even on pitches in chords.
    equal (f $ concatMap UiTest.note_track
            [ [(0, 4, "ly-? -- 4c")]
            , [(0, 2, "4eb"), (2, 2, "ly-? -- 4e")]
            ])
        (Right "<c'?~ ef'>2 <c' e'?>2", [])

test_voices = do
    -- let f meters = LilypondTest.extract_lys Nothing
    --         . LilypondTest.process meters . map LilypondTest.voice_event
    -- pprint (f ["4/4"]
    --         [ (0, 1, "a", Nothing)
    --         , (1, 2, "b", Just 1), (1, 1, "c", Just 2)
    --         , (
    --         ]
    let f = LilypondTest.derive_measures []
    let tracks = concatMap UiTest.note_track
            [ [(0, 1, "4a"), (1, 2, "v = 1 | -- 4b"), (3, 1, "4c")]
            , [(1, 1, "v = 2 | -- 4d")]
            ]
    equal (f tracks)
        (Right "a'4 << { VoiceOne: b'2 } { VoiceTwo: d'4 r4 } >> c'4", [])

    let (text, logs) = make_ly Types.default_config tracks
    equal logs []
    match text "voiceOne*voiceTwo*oneVoice"

test_movements = do
    let (text, logs) = make_ly Types.default_config $
            (">", [(4, 0, "movement 'number 2'")])
            : UiTest.regular_notes 6
    equal logs []
    -- \score, first movement, movement title, \score, second movement
    match text "score * c4 d4 e4 f4 *number 2*score * g4 a4 r2"

make_ly :: Types.Config -> [UiTest.TrackSpec] -> (Text, [Text])
make_ly config = first (LilypondTest.make_ly config)
    . LilypondTest.partition_logs . LilypondTest.derive_tracks
