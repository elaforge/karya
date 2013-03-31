module Derive.Call.Lily_test where
import Util.Test
import qualified Ui.Skeleton as Skeleton
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.LilypondTest as LilypondTest


test_when_ly = do
    let run_ly = extract . uncurry derive_ly
        run_normal = extract . uncurry derive_normal
        extract = DeriveTest.extract DeriveTest.e_note
        mktracks ly normal = ([(1, 3), (2, 3), (3, 4)],
            [ ("> | when-ly", ly)
            , ("> | unless-ly", normal)
            ] ++ UiTest.regular_notes 4)

    prettyp (run_ly $ mktracks [(0, 2, "(")] [(2, 2, "(")])
    prettyp (run_normal $ mktracks [(0, 2, "(")] [(2, 2, "(")])

derive_normal skel =
    DeriveTest.derive_tracks_with_ui id (DeriveTest.set_skel skel)

derive_ly :: [Skeleton.Edge] -> [UiTest.TrackSpec] -> Derive.Result
derive_ly skel =
    DeriveTest.derive_tracks_with_ui id (DeriveTest.set_skel skel)

test_is_ly = do
    let not_ly = DeriveTest.extract ex . DeriveTest.linear_derive_tracks id
            where ex e = (DeriveTest.e_pitch e, DeriveTest.e_attributes e)
        is_ly = LilypondTest.extract extract . LilypondTest.derive_linear
            where
            extract e = (Lilypond.event_pitch e,
                ShowVal.show_val (Lilypond.event_attributes e))

    let tracks =
            [ (">", [(1, 1, "+always")])
            , ("> | is-ly", [(0, 1, "+ly1"), (1, 1, "+ly2")])
            , ("> | not-ly", [(1, 1, "+no1"), (2, 1, "+no2")])
            ] ++ UiTest.regular_notes 4
    equal (not_ly tracks)
        ([("4a", "-"), ("4b", "+always+no1"), ("4c", "+no2"), ("4d", "-")], [])
    equal (is_ly tracks)
        ([("a'", "+ly1"), ("b'", "+always+ly2"), ("c'", "-"), ("d'", "-")], [])

test_if_ly = do
    let run = LilypondTest.derive_measures [] . UiTest.note_track
    equal (run [(0, 1, "if-ly +accent +mute -- 4a")])
        (Right "a'4-> r4 r2", [])
    equal (run [(0, 1, "if-ly +mute +accent -- 4a")])
        (Right "a'4-+ r4 r2", [])
    -- Passing as strings is also ok.
    equal (run [(0, 1, "if-ly '+accent' '+mute' -- 4a")])
        (Right "a'4-> r4 r2", [])

test_8va = do
    let run = LilypondTest.derive_measures ["ottava"]
    equal (run $ UiTest.note_track
            [(0, 1, "8va 1 | -- 4a"), (1, 1, "8va 0 | -- 4b")])
        (Right "\\ottava #1 a'4 \\ottava #0 b'4 r2", [])
    equal (run $
            (">", [(0, 0, "8va 1"), (1, 0, "8va 0")])
            : UiTest.regular_notes 2)
        (Right "\\ottava #1 a'4 \\ottava #0 b'4 r2", [])

test_xstaff = do
    let run = LilypondTest.derive_measures ["change"]
    equal (run $ UiTest.note_track [(0, 1, "xstaff bad | -- 4a")])
        (Right "a'4 r4 r2", ["Error: expected 'up' or 'down', got bad"])
    equal (run $ UiTest.note_track [(0, 1, "xstaff up | -- 4a")])
        (Right "\\change Staff = \"up\" a'4 r4 r2", [])
    equal (run $ (">", [(1, 0, "xstaff up")]) : UiTest.regular_notes 2)
        (Right "a'4 \\change Staff = \"up\" b'4 r2", [])

test_reminder_accidental = do
    let run = LilypondTest.derive_measures [] . UiTest.note_track
    equal (run [(0, 1, "ly-! -- 4a"), (1, 1, "ly-? -- 4b")])
        (Right "a'!4 b'?4 r2", [])
    equal (run [(0, 8, "ly-! -- 4a")])
        (Right "a'!1~ | a'1", [])

test_crescendo_diminuendo = do
    let run = LilypondTest.derive_measures ["<", ">", "!"]
    equal (run $ (">", [(1, 1, "ly-<"), (3, 0, "ly->")])
            : UiTest.regular_notes 4)
        (Right "a'4 b'4 \\< c'4 \\! d'4 \\>", [])
