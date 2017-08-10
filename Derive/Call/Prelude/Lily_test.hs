-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.Lily_test where
import qualified Data.Text as Text

import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Flags as Flags
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Lilypond as Lilypond
import qualified Perform.Lilypond.Constants as Constants
import qualified Perform.Lilypond.LilypondTest as LilypondTest

import Global


test_when_ly = do
    let run_ly = extract . LilypondTest.derive_tracks
        run_normal = extract . DeriveTest.derive_tracks "import ly"
        extract = DeriveTest.extract $
            \e -> (DeriveTest.e_note e, DeriveTest.e_attributes e)
    let tracks =
            [ ("> | when-ly", [(0, 1, "")])
            , ("*", [(0, 0, "4a")])
            , ("> | unless-ly", [(1, 1, "")])
            , ("*", [(1, 0, "4b")])
            ]
    equal (run_ly tracks) ([((0, 1, "4a"), "+")], [])
    equal (run_normal tracks) ([((1, 1, "4b"), "+")], [])

    let tracks = UiTest.note_track [(0, 1, "when-ly +a | -- 4a")]
    equal (run_ly tracks) ([((0, 1, "4a"), "+a")], [])
    equal (run_normal tracks) ([((0, 1, "4a"), "+")], [])
    let tracks = UiTest.note_track [(0, 1, "unless-ly +a | -- 4a")]
    equal (run_ly tracks) ([((0, 1, "4a"), "+")], [])
    equal (run_normal tracks) ([((0, 1, "4a"), "+a")], [])

test_ly_track = do
    let run_normal =
            DeriveTest.extract ex . DeriveTest.derive_tracks_linear "import ly"
            where ex e = (DeriveTest.e_pitch e, DeriveTest.e_attributes e)
        run_ly =
            LilypondTest.extract extract . LilypondTest.derive_tracks_linear
            where
            extract e = (Lilypond.event_pitch e,
                ShowVal.show_val (Lilypond.event_attributes e))

    -- tr       0       1       2       3
    -- >                +always
    -- ly-track +ly1--- +ly2---
    -- not-ly           +no1--- +no2---
    --          3c      3d      3e      3f

    -- Slice is with event range, e.g. (1, 2), but the subs have been
    -- shifted so (0, 1, "+no1")
    let tracks =
            [ (">", [(1, 1, "+always")])
            , ("> | ly-track", [(0, 1, "+ly1"), (1, 1, "+ly2")])
            , ("> | not-ly-track", [(1, 1, "+no1"), (2, 1, "+no2")])
            ] ++ UiTest.regular_notes 4
    equal (run_normal tracks)
        ([("3c", "+"), ("3d", "+always+no1"), ("3e", "+no2"), ("3f", "+")], [])
    equal (run_ly tracks)
        ([("c", "+ly1"), ("d", "+always+ly2"), ("e", "+"), ("f", "+")], [])

test_if_ly = do
    let run = LilypondTest.derive_measures [] . UiTest.note_track
    equal (run [(0, 1, "if-ly +accent +mute -- 4a")])
        (Right "a'4-> r4 r2", [])
    equal (run [(0, 1, "if-ly \"(+mute) \"(+accent) -- 4a")])
        (Right "a'4-+ r4 r2", [])
    -- Passing as strings is also ok.
    equal (run [(0, 1, "if-ly '+accent' '+mute' -- 4a")])
        (Right "a'4-> r4 r2", [])
    -- Null call works.
    equal (run [(0, 1, "if-ly '' '+mute' -- 4a")])
        (Right "a'4 r4 r2", [])

test_8va = do
    let run = LilypondTest.derive_measures ["ottava"]
    equal (run $ UiTest.note_track [(0, 1, "8va 1 | -- 4a"), (1, 1, "4b")])
        (Right "\\ottava #1 a'4 \\ottava #0 b'4 r2", [])
    equal (run $ (">", [(0, 1, "8va 1")]) : UiTest.regular_notes 2)
        (Right "\\ottava #1 c4 \\ottava #0 d4 r2", [])

test_xstaff_around = do
    let run = LilypondTest.derive_measures ["change"]
    equal (run $ UiTest.note_track [(0, 1, "xstaff-a u | -- 4a")])
        (Right "\\change Staff = \"up\" a'4 \\change Staff = \"down\" r4 r2",
            [])
    equal (run $ (">", [(1, 0, "xstaff-a u")]) : UiTest.regular_notes 2)
        (Right "c4 \\change Staff = \"up\" d4 r2", [])

test_clef_dyn = do
    let run = LilypondTest.derive_measures ["clef", "f"]
        tracks = (">", [(0, 0, "clef treble | dyn f")]) : UiTest.regular_notes 2
    equal (run tracks) (Right "\\clef treble c4 \\f d4 r2", [])

    let extract e = (Score.event_start e, DeriveTest.e_pitch e,
            LilypondTest.e_ly_env e)
        pre = Constants.v_ly_prepend
        app = Constants.v_ly_append_all
    equal (DeriveTest.extract extract $ LilypondTest.derive_tracks tracks)
        ( [ (0, "?", [(pre, "'\\clef treble'")])
          , (0, "?", [(app, "'\\f'")])
          , (0, "3c", []), (1, "3d", [])
          ]
        , []
        )

test_articulation = do
    let run = measures_linear []
    equal (run $ (">", [(0, 3, "ly- '>'")]) : UiTest.regular_notes 3)
        (Right "c4-> d4-> e4-> r4", [])

test_ly_notes_post = do
    let run = measures_linear []
    equal (run $ (">", [(0, 3, "ly-notes-post down")]) : UiTest.regular_notes 3)
        (Right "c4\\down d4\\down e4\\down r4", [])

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
    -- It only goes on the first one.
    equal (measures_linear [] $
            (">", [(0, 2, "ly_ hi")]) : UiTest.regular_notes 2)
        (Right "c4_\"hi\" d4 r2", [])

test_ly_slur_beam = do
    let run = LilypondTest.derive_measures []
    -- Works both as a generator and transformer.
    equal (run $ UiTest.note_track
            [(0, 2, "ly-( -- 3a"), (2, 6, "ly-) | -- 3b")])
        (Right "a2( b2~ | b1)", [])
    equal (run $ UiTest.note_track
            [(0, 2, "ly-[ | -- 3a"), (2, 6, "ly-] | -- 3b")])
        (Right "a2[ b2~] | b1", [])

test_subdivision = do
    let run = LilypondTest.staves [] . LilypondTest.derive_tracks
            . concatMap UiTest.inst_note_track
    equal (run [("i1", [(0, 4, "4c")]), ("i2", [(0, 4, "4d")]) ])
        (Right [("i1", ["c'1"]), ("i2", ["d'1"])], [])

    let meter68 = ("ly-global", [(0, 0, "meter '6/8' --")])
    let subdivision t s = [(t, 0, "subdivision '" <> s <> "' --")]
    let one_measure subdiv =
            [ meter68
            , ("i1", [(0, 1.5, "3c"), (1.5, 1.5, "3d")])
            , ("i2", maybe [] (subdivision 0) subdiv ++  [(1, 1, "4c")])
            ]
    equal (run (one_measure Nothing))
        (Right [("i1", ["c4. d4."]), ("i2", ["r4 c'8~ c'8 r4"])], [])
    left_like (fst $ run (one_measure (Just "4/4"))) "incompatible with meter"
    let two_measures subdiv =
            [ meter68
            , ("i1",
                [ (0, 1.5, "3c"), (1.5, 1.5, "3d")
                , (3, 1.5, "3e"), (4.5, 1.5, "3f")
                ])
            , ("i2",
                subdivision 0 "3/4" ++ [(1, 1, "4c")]
                ++ maybe [] (subdivision 3) subdiv ++ [(4, 1, "4d")])
            ]
    -- different spelling persists
    equal (run (two_measures Nothing))
        ( Right
            [("i1", ["c4. d4. | e4. f4."]),
             ("i2", ["r4 c'4 r4 | r4 d'4 r4"])]
        , []
        )
    -- cancel the subdivision
    equal (run (two_measures (Just "")))
        ( Right
            [("i1", ["c4. d4. | e4. f4."]),
             ("i2", ["r4 c'4 r4 | r4 d'8~ d'8 r4"])]
        , []
        )

    -- rests also follow subdivision
    equal (run
            [ meter68
            , ("i1", [(0, 0, "subdivision '3/4' --")])
            , ("i1", [(0, 1, "3c"), (2, 1, "3d")])
            ])
        (Right [("i1", ["c4 r4 d4"])], [])
    -- if it has a duration, cancel at the end
    let run skel = LilypondTest.staves []
            . LilypondTest.derive_tracks_setup (DeriveTest.with_skel skel)
            . concatMap UiTest.inst_note_track
    equal (run [(3, 4)]
            [ meter68
            , ("i1", [(0, 3, "subdivision '3/4' --")])
            , ("i1", [(1, 1, "3c"), (3, 1.5, "3d")])
            ])
        (Right [("i1", ["r4 c4 r4 | d4. r4."])], [])

test_movement = do
    let run = LilypondTest.convert_score . LilypondTest.derive_blocks
    -- Movements split up properly.
    let (ly, logs) = run [(UiTest.default_block_name,
            ("> | ly-global", [(0, 0, "movement I"), (4, 0, "movement II")])
            : UiTest.regular_notes 8)]
    equal logs []
    match ly "piece = \"I\" *c4 d4 e4 f4 * piece = \"II\" * g4 a4 b4 c'4"

    -- Not affected by local tempo.
    let (ly, logs) = run
            [ ("score",
                [ ("> | ly-global",
                    [(0, 0, "movement I"), (4, 0, "movement II")])
                , (">", [(0, 4, "mvt1"), (4, 4, "mvt2")])
                ])
            , ("mvt1=ruler", UiTest.regular_notes 4)
            , ("mvt2=ruler", ("tempo", [(0, 0, "2")]) : UiTest.regular_notes 4)
            ]
    equal logs []
    match ly "piece = \"I\" *c4 d4 e4 f4 * piece = \"II\" * c4 d4 e4 f4"

test_attach_and_emit = do
    -- Ensure that attach calls doesn't try to attach code to emit's code
    -- events.
    let run = DeriveTest.extract extract
            . LilypondTest.derive_tracks_linear
        extract e =
            ( DeriveTest.e_note e
            , DeriveTest.e_environ_like ("ly-" `Text.isPrefixOf`) e
            , Score.event_flags e
            )
    let run_ly = LilypondTest.measures ["a", "b"]
            . LilypondTest.derive_tracks_linear

    let tracks = (">", [(0, 2, "(")])
            : UiTest.note_track1 ["ly-post a | -- 3c", "ly-post b | -- 3d"]
    equal (run tracks)
        ( [ ((0, 0, "?"), [(Constants.v_ly_append_all, "'\\a'")], Flags.ly_code)
          , ((0, 1, "3c"), [(Constants.v_ly_append_first, "'('")], mempty)
          , ((1, 0, "?"), [(Constants.v_ly_append_all, "'\\b'")], Flags.ly_code)
          , ((1, 1, "3d"), [(Constants.v_ly_append_last, "')'")], mempty)
          ]
        , []
        )

    equal (run_ly tracks) (Right "c4( \\a d4) \\b r2", [])
    equal (run_ly $ (">", [(0, 2, "(")])
            : UiTest.note_track1 ["ly-pre a | -- 3c", "ly-pre b | -- 3d"])
        (Right "\\a c4( \\b d4) r2", [])


-- * util

measures_linear :: [Text] -> [UiTest.TrackSpec] -> (Either Text Text, [Text])
measures_linear wanted =
    LilypondTest.measures wanted . LilypondTest.derive_tracks_linear
