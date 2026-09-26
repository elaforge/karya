-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Lilypond.Lilypond_test where
import qualified Derive.Args as Args
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Ly as Ly
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Perform.Lilypond.Types as Types

import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_convert_sections :: Test
test_convert_sections = do
    let run = LilypondTest.deriveStaves []
    let (events, logs) = run $ concatMap UiTest.noteSpec
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

test_staff_configs :: Test
test_staff_configs = do
    let piano = Types.emptyStaffConfig
            { Types.staffLong = "piano"
            , Types.staffShort = ""
            , Types.staffCode = ["piano code"]
            }
        viola = Types.emptyStaffConfig
            { Types.staffLong = "viola"
            , Types.staffShort = "vla"
            , Types.staffCode = ["viola code"]
            }
        staves = [("i2", viola), ("i1", piano)]
        config = Types.defaultConfig { Types.configStaves = staves }
    let (text, logs) = makeLy config
            [ (">i1", [(0, 1, "")]), ("*", [(0, 0, "3c")])
            , (">i2", [(0, 1, "")]), ("*", [(0, 0, "3d")])
            ]
    equal logs []
    -- Viola goes first, has correct long and short names and code.
    match text
        "instrumentName = \"viola\"*shortInstrumentName = \"vla\"*viola code\
        \*instrumentName = \"piano\"*piano code"

test_add_bass_staff :: Test
test_add_bass_staff = do
    let config = Types.defaultConfig
            { Types.configStaves = [("i1", staffConfig)] }
        staffConfig = Types.emptyStaffConfig
            { Types.staffAddBassStaff = True }
    let (text, logs) = makeLy config $ UiTest.noteSpec
            ("i1", [(0, 1, "clef treble | -- 4c"), (1, 3, "4d")], [])
    equal logs []
    -- Key and meter are in, but the clef is replaced.
    match text
        "new Staff = \"up\"\
        \*new Staff = \"down\" * \\RemoveAllEmptyStaves\
        \*\\clef bass \\time 4/4 \\key c \\major s4 s2. \\bar \"|.\""

    -- FullMeasure rests are correctly converted to an empty track with hidden
    -- rests.
    let text = fst $ makeLy config $
            UiTest.noteSpec ("i1", [(4, 1, "4c")], [])
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

test_hands :: Test
test_hands = do
    let run = LilypondTest.deriveStaves
    let (events, logs) = run [] $ concatMap UiTest.noteSpec
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

test_clefs :: Test
test_clefs = do
    let f = LilypondTest.deriveMeasures ["clef"]
    equal (f
            [ (">i1 | clef bass", [(0, 2, ""), (2, 6, "clef alto |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right "\\clef bass c'2 \\clef alto c'2~ | c'1", [])

test_key :: Test
test_key = do
    let f = LilypondTest.deriveMeasures ["key"]
    equal (f
            [ (">i1 | key = a-mixolydian",
                [(0, 2, ""), (2, 2, "key = c-maj |")])
            , ("*", [(0, 0, "4c")])
            ])
        (Right "\\key a \\mixolydian c'2 \\key c \\major c'2", [])

test_ly_prepend_append :: Test
test_ly_prepend_append = do
    let f env = LilypondTest.convertMeasures [] $
            map LilypondTest.environEvent [(0, 12, Just LilypondTest.a3, env)]
        str :: EnvKey.Key -> Text -> [(EnvKey.Key, DeriveT.Val)]
        str key val = [(key, Typecheck.toVal val)]
    let key pos dist = Constants.positionKey $
            Constants.CodePosition Constants.Chord pos dist
    equal (f []) $ Right "a1~ | a1~ | a1"
    equal (f (str (key Constants.Append Constants.First) "x")) $
        Right "a1~ x | a1~ | a1"
    equal (f (str (key Constants.Append Constants.Last) "x")) $
        Right "a1~ | a1~ | a1 x"
    equal (f (str (key Constants.Append Constants.All) "x")) $
        Right "a1~ x | a1~ x | a1 x"
    equal (f (str (key Constants.Prepend Constants.First) "x")) $
        Right "x a1~ | a1~ | a1"

test_ly_code :: Test
test_ly_code = do
    -- Test stand-alone zero-dur code fragments.
    let f = LilypondTest.measures [] . LilypondTest.deriveTracksSetup calls
    -- prepend
    equal (f $
            UiTest.noteTrack [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
        (Right "<c' a'>4 pre <d' b'>4 r2", [])
    -- append
    equal (f $
            UiTest.noteTrack [(0, 1, "4a"), (1, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ UiTest.noteTrack [(0, 1, "4c"), (1, 1, "4d")])
        (Right "<c' a'>4 <d' b'>4 post r2", [])
    -- prepend and append alone
    equal (f $
            UiTest.noteTrack [(0, 1, "4a"), (2, 1, "4b")]
            ++ [(">", [(1, 0, "post")])]
            ++ [(">", [(1, 0, "pre")])]
            ++ UiTest.noteTrack [(0, 1, "4c"), (2, 1, "4d")])
        (Right "<c' a'>4 pre r4 post <d' b'>4 r4", [])
    where
    calls = CallTest.withNoteGenerator "pre" cPre
        <> CallTest.withNoteGenerator "post" cPost
    cPre = CallTest.generator $ \args ->
        Ly.code0 (Args.start args) (Ly.Position Constants.FreePrepend, "pre")
    cPost = CallTest.generator $ \args ->
        Ly.code0 (Args.start args) (Ly.Position Constants.FreeAppend, "post")

-- * test lilypond derivation

-- These actually test derivation in lilypond mode.  So maybe they should go
-- in derive, but if I put them here I can test all the way to lilypond score.

test_clip_block :: Test
test_clip_block = do
    let run = LilypondTest.extract LilypondTest.eNote
            . LilypondTest.deriveBlocks
        q = Types.durToTime Types.D4
    equal (run
            [ ("b1", [(">", [(0, 4, "sub")])])
            , ("sub=ruler", UiTest.regularNotes 4)
            ])
        ([(0, q, "c"), (q, q, "d"), (q*2, q, "e"), (q*3, q, "f")], [])
    equal (run
            [ ("b1", [(">", [(0, 2.5, "clip | sub")])])
            , ("sub=ruler", UiTest.regularNotes 4)
            ])
        ([(0, q, "c"), (q, q, "d"), (q*2, Types.durToTime Types.D8, "e")], [])

test_ly_global :: Test
test_ly_global = do
    let run global tracks = LilypondTest.deriveStaves ["pre", "tempo"] $
            (">ly-global", global) : concatMap UiTest.noteTrack tracks
    equal (run [(0, 0, "")] [])
        (Right [], ["a note without pitch must have code"])
    equal (run [(0, 0, "ly-emit pre prepend")]
            [[(0, 1, "inst=i1 | -- 3c")], [(0, 1, "inst=i2 | -- 3d")]])
        (Right [("i1", ["pre c4 r4 r2"]), ("i2", ["pre d4 r4 r2"])], [])
    equal (run [(0, 0, "tempo slow")]
            [[(0, 1, "inst=i1 | -- 3c")], [(0, 1, "inst=i2 | -- 3d")]])
        (Right [("i1", ["\\tempo slow c4 r4 r2"]),
            ("i2", ["\\tempo slow d4 r4 r2"])], [])

test_meter :: Test
test_meter = do
    let run meter notes = LilypondTest.deriveMeasures ["time"] $
            (">", meter) : UiTest.noteTrack notes
    equal (run [(0, 0, "meter '2/4'"), (2, 0, "meter '4/4'")]
            [(0, 3, "4a"), (3, 3, "4b")])
        (Right "\\time 2/4 a'2~ | \\time 4/4 a'4 b'2.", [])
    -- Meter change is rounded up to the next barline, as usual.
    equal (run [(0, 0, "meter '2/4'"), (1, 0, "meter '4/4'")]
            [(0, 3, "4a"), (3, 3, "4b")])
        (Right "\\time 2/4 a'2~ | \\time 4/4 a'4 b'2.", [])

test_enharmonics :: Test
test_enharmonics = do
    let (events, logs) = LilypondTest.deriveMeasures [] $ UiTest.noteTrack
            [(0, 1, "4c#"), (1, 1, "4db"), (2, 1, "4cx")]
    equal logs []
    equal events $ Right "cs'4 df'4 css'4 r4"

test_tempo :: Test
test_tempo = do
    -- Lilypond derivation is unaffected by the tempo.
    let (events, logs) = LilypondTest.extract extract $
            LilypondTest.deriveTracks
                [ ("tempo", [(0, 0, "3")])
                , (">i1", [(0, 4, ""), (4, 4, "")])
                , ("*", [(0, 0, "4c")])
                ]
        extract e = (Types.eventStart e, Types.eventDuration e)
        whole = Types.timePerWhole
    equal logs []
    equal events [(0, whole), (whole, whole)]

test_attributes :: Test
test_attributes = do
    let f = LilypondTest.deriveMeasures []
    equal (f
        [ (">", [(0, 1, "+mute"), (1, 1, "+stac"), (2, 1, "")])
        , ("*", [(0, 0, "4a"), (1, 0, "4b"), (2, 0, "4c")])
        ])
        (Right "a'4 -+ b'4 -. c'4 r4", [])

test_modal_attributes :: Test
test_modal_attributes = do
    let f = LilypondTest.deriveMeasures [] . (:[("*", [(0, 0, "3c")])])
    equal (f (">", [(0, 1, "+pizz"), (1, 1, "+pizz"), (2, 1, "")]))
        (Right "c4 ^\"pizz.\" c4 c4 ^\"arco\" r4", [])

test_prepend_append :: Test
test_prepend_append = do
    let f = LilypondTest.deriveMeasures ["p", "mf"]
    equal (f $
            (">", [(0, 0, "dyn p"), (2, 0, "dyn mf")]) : UiTest.noteTrack
            [(0, 1, "4a"), (1, 1, "4b"), (2, 1, "4c"), (3, 1, "4d")])
        (Right "a'4 \\p b'4 c'4 \\mf d'4", [])

test_append_pitch :: Test
test_append_pitch = do
    let f = LilypondTest.deriveMeasures []
    -- Append pitch works even on pitches in chords.
    equal (f $ concatMap UiTest.noteTrack
            [ [(0, 4, "ly-? -- 4c")]
            , [(0, 2, "4eb"), (2, 2, "ly-? -- 4e")]
            ])
        (Right "<c'?~ ef'>2 <c' e'?>2", [])

test_voices :: Test
test_voices = do
    let f = LilypondTest.deriveMeasures []
    let tracks = concatMap UiTest.noteTrack
            [ [(0, 1, "4a"), (1, 2, "v = 1 | -- 4b"), (3, 1, "4c")]
            , [(1, 1, "v = 2 | -- 4d")]
            ]
    equal (f tracks)
        (Right "a'4 << { VoiceOne: b'2 } { VoiceTwo: d'4 r4 } >> c'4", [])

    let (text, logs) = makeLy Types.defaultConfig tracks
    equal logs []
    match text "voiceOne*voiceTwo*oneVoice"

test_movements :: Test
test_movements = do
    let run = makeLy Types.defaultConfig
    let (text, logs) = run $
            (">", [(4, 0, "movement 'number 2'")]) : UiTest.regularNotes 6
    equal logs []
    -- \score, first movement, movement title, \score, second movement
    match text "score * c4 d4 e4 f4 *number 2*score * g4 a4 r2"

test_parse_meters :: Test
test_parse_meters = do
    let f end = fmap (bimap (map pretty) (map pretty))
            . Lilypond.parseMeters 0 (end * Types.timePerWhole)
            . map LilypondTest.environEvent
        meter m = (Constants.vMeter, Typecheck.toVal (m :: Text))
    equal (f 2 []) (Right (["4/4", "4/4"], []))
    -- Not mislead by a meter change past the end.
    equal (f 2 [(32, 0, Nothing, [meter "3/4"])]) (Right (["4/4", "4/4"], []))

makeLy :: Types.Config -> [UiTest.TrackSpec] -> (Text, [Text])
makeLy config = first (LilypondTest.makeLy config)
    . LilypondTest.partitionLogs . LilypondTest.deriveTracks
