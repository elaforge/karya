module Derive.Call.Lily_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ShowVal as ShowVal
import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.LilypondTest as LilypondTest


test_when_ly = do
    let run_ly = extract . LilypondTest.derive
        run_normal = extract . DeriveTest.derive_tracks
        extract = DeriveTest.extract $
            \e -> (DeriveTest.e_note e, DeriveTest.e_attributes e)
    let tracks =
            [ ("> | when-ly", [(0, 1, "")])
            , ("*", [(0, 0, "4a")])
            , ("> | unless-ly", [(1, 1, "")])
            , ("*", [(1, 0, "4b")])
            ]
    equal (run_ly tracks) ([((0, 1, "4a"), "-")], [])
    equal (run_normal tracks) ([((1, 1, "4b"), "-")], [])

    let tracks = UiTest.note_track [(0, 1, "when-ly +a | -- 4a")]
    equal (run_ly tracks) ([((0, 1, "4a"), "+a")], [])
    equal (run_normal tracks) ([((0, 1, "4a"), "-")], [])
    let tracks = UiTest.note_track [(0, 1, "unless-ly +a | -- 4a")]
    equal (run_ly tracks) ([((0, 1, "4a"), "-")], [])
    equal (run_normal tracks) ([((0, 1, "4a"), "+a")], [])

test_ly_track = do
    let run_normal = DeriveTest.extract ex . DeriveTest.linear_derive_tracks id
            where ex e = (DeriveTest.e_pitch e, DeriveTest.e_attributes e)
        run_ly = LilypondTest.extract extract . LilypondTest.derive_linear
            where
            extract e = (Lilypond.event_pitch e,
                ShowVal.show_val (Lilypond.event_attributes e))

    let tracks =
            [ (">", [(1, 1, "+always")])
            , ("> | ly-track", [(0, 1, "+ly1"), (1, 1, "+ly2")])
            , ("> | not-ly-track", [(1, 1, "+no1"), (2, 1, "+no2")])
            ] ++ UiTest.regular_notes 4
    equal (run_normal tracks)
        ([("3c", "-"), ("3d", "+always+no1"), ("3e", "+no2"), ("3f", "-")], [])
    equal (run_ly tracks)
        ([("c", "+ly1"), ("d", "+always+ly2"), ("e", "-"), ("f", "-")], [])

test_if_ly = do
    let run = LilypondTest.derive_measures [] . UiTest.note_track
    equal (run [(0, 1, "if-ly +accent +mute -- 4a")])
        (Right "a'4-> r4 r2", [])
    equal (run [(0, 1, "if-ly +mute +accent -- 4a")])
        (Right "a'4-+ r4 r2", [])
    -- Passing as strings is also ok.
    equal (run [(0, 1, "if-ly '+accent' '+mute' -- 4a")])
        (Right "a'4-> r4 r2", [])
    -- Null call works.
    equal (run [(0, 1, "if-ly '' '+mute' -- 4a")])
        (Right "a'4 r4 r2", [])

test_8va = do
    let run = LilypondTest.derive_measures ["ottava"]
    equal (run $ UiTest.note_track
            [(0, 1, "8va 1 | -- 4a"), (1, 1, "8va 0 | -- 4b")])
        (Right "\\ottava #1 a'4 \\ottava #0 b'4 r2", [])
    equal (run $
            (">", [(0, 0, "8va 1"), (1, 0, "8va 0")])
            : UiTest.regular_notes 2)
        (Right "\\ottava #1 c4 \\ottava #0 d4 r2", [])

test_xstaff = do
    let run = LilypondTest.derive_measures ["change"]
    equal (run $ UiTest.note_track [(0, 1, "xstaff bad | -- 4a")])
        (Right "a'4 r4 r2", ["Error: expected 'up' or 'down', got bad"])
    equal (run $ UiTest.note_track [(0, 1, "xstaff up | -- 4a")])
        (Right "\\change Staff = \"up\" a'4 r4 r2", [])
    equal (run $ (">", [(1, 0, "xstaff up")]) : UiTest.regular_notes 2)
        (Right "c4 \\change Staff = \"up\" d4 r2", [])

test_reminder_accidental = do
    let run = LilypondTest.derive_measures [] . UiTest.note_track
    equal (run [(0, 1, "ly-! -- 4a"), (1, 1, "ly-? -- 4b")])
        (Right "a'!4 b'?4 r2", [])
    equal (run [(0, 8, "ly-! -- 4a")])
        (Right "a'!1~ | a'1", [])

test_tie_direction = do
    let run = LilypondTest.derive_measures [] . concatMap UiTest.note_track
    equal (run [[(0, 8, "ly-^~ -- 4a"), (8, 8, "ly-_~ -- 4b")]])
        (Right "a'1^~ | a'1 | b'1_~ | b'1", [])
    equal (run [[(0, 8, "ly-^~ -- 4a")], [(0, 8, "ly-_~ -- 4b")]])
        (Right "<a'^~ b'_~>1 | <a' b'>1", [])

test_crescendo_diminuendo = do
    let run = LilypondTest.derive_measures ["<", ">", "!"]
    equal (run $ (">", [(1, 1, "ly-<"), (3, 0, "ly->")])
            : UiTest.regular_notes 4)
        (Right "c4 d4 \\< e4 \\! f4 \\>", [])

test_ly_text = do
    let run = LilypondTest.derive_measures []
    equal (run $ UiTest.note_track [(0, 1, "ly^ hi | -- 4a")])
        (Right "a'4^\"hi\" r4 r2", [])
    equal (measures_linear [] $
            (">", [(0, 0, "ly_ hi")]) : UiTest.regular_notes 1)
        (Right "c4_\"hi\" r4 r2", [])

test_ly_slur = do
    let run = LilypondTest.derive_measures []
    equal (run $ UiTest.note_track
            [(0, 2, "ly-( | -- 3a"), (2, 6, "ly-) | -- 3b")])
        (Right "a2( b2~ | b1)", [])

measures_linear :: [String] -> [UiTest.TrackSpec]
    -> (Either String String, [String])
measures_linear wanted =
    LilypondTest.measures wanted . LilypondTest.derive_linear
