-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Postproc_test where
import qualified Util.Lists as Lists
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.C.Post.Postproc as Postproc
import qualified Derive.C.Prelude.Note as Note
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import Global


-- * cancel

test_cancel :: Test
test_cancel = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks "cancel" . UiTest.note_track
        notes n1 n2 = [(0, 2, n1 <> "d 2 | -- 4c"), (2, 3, n2 <> "-- 4d")]
    -- No flags, no cancel.
    equal (run (notes "" ""))
        ([(2, 2, "4c"), (2, 3, "4d")], [])
    equal (run (notes "" "add-flag weak | "))
        ([(2, 2, "4c")], [])
    equal (run (notes "add-flag weak | " ""))
        ([(2, 3, "4d")], [])
    equal (run (notes "add-flag strong | " ""))
        ([(2, 2, "4c")], [])

    -- Multiple weaks and strongs together are ok.
    equal (run (notes "add-flag weak | " "add-flag weak | "))
        ([(2, 2, "4c"), (2, 3, "4d")], [])
    equal (run (notes "add-flag strong | " "add-flag strong | "))
        ([(2, 2, "4c"), (2, 3, "4d")], [])

test_infer_duration_block :: Test
test_infer_duration_block = do
    -- The note deriver automatically adds flags so that a note at the end of
    -- a block can cancel the next event and get an inferred duration.
    let run = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_blocks
        top = ("top -- cancel 2",
            [(">", [(0, 2, "sub")]), (">", [(2, 2, "sub")])])
        sub notes = ("sub=ruler", UiTest.note_track notes)
    equal (run [top, sub [(1, 1, "4c"), (2, 0, "4d")]])
        ([(1, 1, "4c"), (2, 1, "4d"), (3, 1, "4c"), (4, 2, "4d")], [])

    -- First note is cancelled out.
    equal (run [top, sub [(0, 1, "4c"), (1, 1, "4d"), (2, 0, "4e")]])
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (3, 1, "4d"), (4, 2, "4e")],
            [])
    -- The inferred note takes the duration of the replaced one.
    equal (run [top, sub [(0, 1.5, "4c"), (2, 0, "4d")]])
        ([(0, 1.5, "4c"), (2, 1.5, "4d"), (4, 2, "4d")], [])

    -- A zero duration block doesn't have its duration changed, since otherwise
    -- I can't write single note calls for e.g. percussion.
    equal (run [top, sub [(0, 0, "4c")]]) ([(0, 0, "4c"), (2, 0, "4c")], [])

test_infer_duration :: Test
test_infer_duration = do
    let run extract = DeriveTest.extract extract
            . DeriveTest.derive_tracks "cancel" . UiTest.note_track
    -- Infer duration to fill the gap.
    equal (run DeriveTest.e_note
            [(0, 1, "add-flag infer-duration | -- 4c"), (4, 1, "4d")])
        ([(0, 4, "4c"), (4, 1, "4d")], [])
    -- Also the flag is removed to avoid inferring twice.
    equal (run Score.event_flags
            [(0, 0, "add-flag infer-duration | -- 4c"), (4, 1, "4d")])
        ([mempty, mempty], [])

test_suppress_until :: Test
test_suppress_until = do
    let run = DeriveTest.extract Score.event_start
            . DeriveTest.derive_tracks "cancel 1"
            . map (second (map (\(d, t) -> (d, 1, t))))
        suppress t = "suppress-until = " <> t <> " | --"
    equal (run [(">", [(0, ""), (1, "")])]) ([0, 1], [])
    equal (run [(">", [(0, ""), (1, suppress "2s"), (2, ""), (3, "")])])
        ([0, 1, 3], [])
    -- Suppression works even with a coincident note coming later.
    equal (run
            [ (">", [(0, ""), (1, ""), (2, ""), (3, "")])
            , (">", [(1, suppress "2s")])
            ])
        ([0, 1, 3], [])

test_randomize_start :: Test
test_randomize_start = do
    let run title = DeriveTest.extract Score.event_start
            . DeriveTest.derive_tracks title
    let notes = [(">", [(0, 1, ""), (1, 1, "")])]
    let (starts, logs) = run "randomize-start 5" notes
    equal logs []
    not_equal starts [0, 1]

-- * other

test_apply_start_offset :: Test
test_apply_start_offset = do
    let run = DeriveTest.extract DeriveTest.e_note . DeriveTest.derive_blocks
        top = "top -- apply-start-offset .25"
        min_dur = 0.25
    equal (run [(top, UiTest.note_track [(1, 1, "start-s=1 | -- 4c")])])
        ([(2, min_dur, "4c")], [])
    equal (run
            [ (top, UiTest.note_track [(2, 2, "start-s = -1 | sub -- 4c")])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ])
        ([(1, 3, "4c")], [])
    equal (run [(top, ("tempo", [(0, 0, "2")])
            : UiTest.note_track [(2, 2, "start-t=1 | -- 4c")])])
        ([(1.5, 0.5, "4c")], [])

    -- 0   1   2   3
    -- 4c--4d--4e--|
    let start offset = "start-s = " <> offset
    let neighbors offset = UiTest.note_track
            [(0, 1, "4c"), (1, 1, start offset <> " | -- 4d") , (2, 1, "4e")]

    equal (run [(top, neighbors "-.5")])
        ([(0, 0.5, "4c"), (0.5, 1.5, "4d"), (2, 1, "4e")], [])

    -- It's already overlapping, so don't change the duration.
    equal (run [(top, UiTest.note_track
            [ (0, 1, "sus=1.5 | -- 4c"), (1, 1, start "-.5" <> " | -- 4d")
            , (2, 1, "4e")
            ])])
        ([(0, 1.5, "4c"), (0.5, 1.5, "4d"), (2, 1, "4e")], [])
    -- Not overlapping, but will shorten.
    equal (run [(top, UiTest.note_track
            [ (0, 1, "sus=.75 | -- 4c"), (1, 1, start "-.5" <> " | -- 4d")
            , (2, 1, "4e")
            ])])
        ([(0, 0.5, "4c"), (0.5, 1.5, "4d"), (2, 1, "4e")], [])

    -- Bounded by previous or next notes.
    equal (run [(top, neighbors "-2")])
        ([(0, min_dur, "4c"), (min_dur, 2 - min_dur, "4d"), (2, 1, "4e")], [])

test_apply_start_offset_sorted :: Test
test_apply_start_offset_sorted = do
    -- The output is still sorted after applying start offset.
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks "apply-start-offset"
            . UiTest.note_track
    equal (run [(0, 1, "start-s = 2 | -- 4c"), (1, 1, "4d")])
        ([(1, 1, "4d"), (2, Note.min_duration, "4c")], [])

test_adjust_offset :: Test
test_adjust_offset = do
    let f (s1, o1) (s2, o2) =
            ( s1 + Postproc.adjust_offset d Nothing (Just (o2, s2)) o1 s1
            , s2 + Postproc.adjust_offset d (Just (o1, s1)) Nothing o2 s2
            )
        d = 0.25
        range s e = Lists.range s e (if e >= s then 1 else -1)
    -- There are two variations: they can move in the same direction, or in
    -- opposite directions.  And then, the directions can be positive or
    -- negative.

    -- 0   1   2   3   4   5
    -- |---> - - - - - >
    --         |---> - - - >
    equal [[f (0, o1) (2, o2) | o1 <- range 0 4] | o2 <- range 0 3]
        [ [(0, 2), (1, 2), (2-d, 2), (2-d, 2), (2-d, 2)]
        , [(0, 3), (1, 3), (2, 3), (3-d, 3), (3-d, 3)]
        , [(0, 4), (1, 4), (2, 4), (3, 4), (4-d, 4)]
        , [(0, 5), (1, 5), (2, 5), (3, 5), (4, 5)]
        ]

    -- 0   1   2   3   4   5
    --     |---> - - - - - >
    -- < - - - - - <---|
    equal [[f (1, o1) (4, o2) | o1 <- range 0 4] | o2 <- range 0 (-3)]
        [ [(1, 4), (2, 4), (3, 4), (4-d, 4), (4-d, 4)]
        , [(1, 3), (2, 3), (3-d, 3), (3.5-d, 3.5), (3.5-d, 3.5)]
        , [(1, 2), (2-d, 2), (2.5-d, 2.5), (3-d, 3), (3-d, 3)]
        , [(1, 1+d), (1.5-d, 1.5), (2-d, 2), (2.5-d, 2.5), (2.5-d, 2.5)]
        ]

    -- 0   1   2   3   4   5
    -- < - - - <---|
    --     < - - - - - <---|
    equal [[f (3, o1) (5, o2) | o1 <- range 0 (-3)] | o2 <- range 0 (-4)]
        [ [(3, 5), (2, 5), (1, 5), (0, 5)]
        , [(3, 4), (2, 4), (1, 4), (0, 4)]
        , [(3, 3+d), (2, 3), (1, 3), (0, 3)]
        , [(3, 3+d), (2, 2+d), (1, 2), (0, 2)]
        , [(3, 3+d), (2, 2+d), (1, 1+d), (0, 1)]
        ]
