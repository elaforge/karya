module Perform.Lilypond.Lilypond_test where
import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Args as Args
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Lily as Lily
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Perform.Lilypond.Types as Types


test_make_ly = do
    let run = LilypondTest.derive_staves []
    let (events, logs) = run $ concatMap UiTest.note_spec
            -- complicated rhythm
            [ ("s/i1", [(0, 1, "4c"), (1.5, 2, "4d#")], [])
            -- rhythm starts after 0, long multi measure note
            , ("s/i2", [(1, 1, "4g"), (2, 12, "3a")], [])
            ]
    equal logs []
    -- Shorter staff is padded out to the length of the longer one, and then to
    -- the end of the bar.
    equal events $ Right
        [ ("i1", ["c'4 r8 ds'8~ ds'4. r8 | R1 | R1 | R1"])
        , ("i2", ["r4 g'4 a2~ | a1~ | a1~ | a2 r2"])
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
        [ ("1", ["c'1", "d'1"])
        , ("2", ["e'1"])
        ]

test_clefs = do
    let f = LilypondTest.derive_measures ["clef"]
    equal (f
            [ (">s/1 | clef bass", [(0, 2, ""), (2, 6, "clef alto |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right "\\clef bass c'2 \\clef alto c'2~ | c'1", [])

test_key = do
    let f = LilypondTest.derive_measures ["key"]
    equal (f
            [ (">s/1 | key = 'a-mixo'", [(0, 2, ""), (2, 2, "key = 'c-maj' |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right "\\key a \\mixolydian c'2 \\key c \\major c'2", [])

test_ly_prepend_append = do
    let f env = LilypondTest.convert_measures [] $
            map LilypondTest.environ_event [(0, 12, "a", env)]
        str key val = [(key, TrackLang.to_val val)]
    equal (f []) $ Right "a1~ | a1~ | a1"
    equal (f (str Constants.v_ly_append_first "x")) $
        Right "a1~x | a1~ | a1"
    equal (f (str Constants.v_ly_append_last "x")) $
        Right "a1~ | a1~ | a1x"
    equal (f (str Constants.v_ly_append_all "x")) $
        Right "a1~x | a1~x | a1x"
    equal (f (str Constants.v_ly_prepend "x")) $
        Right "xa1~ | a1~ | a1"

test_ly_code = do
    -- Test stand-alone zero-dur code fragments.
    let f = LilypondTest.measures []
            . LilypondTest.derive_tracks_with_ui calls id
    -- prepend
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        (Right "<a' c'>4 pre <b' d'>4 r2", [])
    -- append
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")])
        (Right "<a' c'>4 <b' d'>4 post r2", [])
    -- prepend and append alone
    equal (f $
            UiTest.note_track [(0, 1, "4a"), (2, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.note_track [(0, 1, "4c"), (2, 1, "4d")])
        (Right "<a' c'>4 pre r4 post <b' d'>4 r4", [])
    where
    calls = CallTest.with_note_call "pre" c_pre
        . CallTest.with_note_call "post" c_post
    c_pre = CallTest.generator $ \args ->
        Lily.code0 (Args.start args) (Lily.Prefix, "pre")
    c_post = CallTest.generator $ \args ->
        Lily.code0 (Args.start args) (Lily.SuffixAll, "post")

-- * test lilypond derivation

-- These actually test derivation in lilypond mode.  So maybe they should go
-- in derive, but if I put them here I can test all the way to lilypond score.

test_meter = do
    let run meter notes = LilypondTest.derive_measures ["time"] $
            (">ly-global", meter) : UiTest.note_track notes
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
    let (events, logs) = LilypondTest.extract extract $ LilypondTest.derive
            [ ("tempo", [(0, 0, "3")])
            , (">s/1", [(0, 4, ""), (4, 4, "")])
            , ("*", [(0, 0, "4c")])
            ]
        extract e = (Types.event_start e, Types.event_duration e)
        whole = Types.time_per_whole
    equal logs []
    equal events [(0, whole), (whole, whole)]

test_attributes = do
    let f = LilypondTest.derive_measures []
    equal (f
        [ (">", [(0, 1, "+mute"), (1, 1, "o"), (2, 1, "")])
        , ("*", [(0, 0, "4a"), (1, 0, "4b"), (2, 0, "4c")])
        ])
        (Right "a'4-+ b'4-\\flageolet c'4 r4", [])

test_modal_attributes = do
    let f = LilypondTest.derive_measures []
    equal (f
        [ (">", [(0, 1, "+pizz"), (1, 1, "+pizz"), (2, 1, "")])
        , ("*", [(0, 0, "4c")])
        ])
        (Right "c'4^\"pizz.\" c'4 c'4^\"arco\" r4", [])

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
            [ [(0, 1, "4a"), (1, 2, "voice = 1 | -- 4b"), (3, 1, "4c")]
            , [(1, 1, "voice = 2 | -- 4d")]
            ]
    equal (f tracks)
        (Right "a'4 << { VoiceOne: b'2 } { VoiceTwo: d'4 r4 } >> c'4", [])

    let make_ly = first LilypondTest.make_ly . LilypondTest.partition
            . LilypondTest.derive
    putStrLn (fst $ make_ly tracks)
