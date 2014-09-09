module Derive.Call.Post.ArrivalNote_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_infer_duration = do
    let run = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_blocks
        top = ("top -- infer-duration 2",
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

test_infer_duration_controls = do
    -- A zero duration note at the end of a block gets controls from right
    -- before.
    let run extract = DeriveTest.extract extract . DeriveTest.derive_blocks
        top = "top -- infer-duration"

    -- sub---| sub---|
    -- 4c 4d 4e 4d 4e
    -- "  "  "  4e 4f
    equal (run DeriveTest.e_pitch
            [ (top,
                [ ("t-dia", [(0, 0, "0"), (2, 0, "1")])
                , (">", [(0, 2, "sub"), (2, 2, "sub")])
                ])
            , ("sub=ruler", UiTest.note_track
                [(0, 1, "4c"), (1, 1, "4d"), (2, 0, "4e")])
            ])
        (["4c", "4d", "4e", "4e", "4f"], [])

    -- The inferred note takes the controls of the replaced note, except the
    -- first sample, which comes from the previous block.
    equal (run (DeriveTest.e_control "c")
            [ (top, [(">", [(0, 2, "sub"), (2, 2, "sub")])])
            , ("sub=ruler",
                [ ("c", [(0, 0, "1"), (1, 0, "2"), (2, 0, "3")])
                , (">", [(0, 2, ""), (2, 0, "")])
                ])
            ])
        ([[(0, 1), (1, 2)], [(2, 3), (3, 2)], [(4, 3)]], [])

    -- But if there is no replaced event, I don't know how to get the controls.
    equal (run (DeriveTest.e_control "c")
            [ (top,
                [ ("c", [(3, 0, "1")])
                , (">", [(0, 2, "sub1"), (2, 2, "sub2")])
                ])
            , ("sub1=ruler",
                [ (">", [(0, 2, ""), (2, 0, "")])
                ])
            , ("sub2=ruler",
                [ (">", [(2, 0, "")])
                ])
            ])
        -- TODO The note whose duration was inferred should get continuous
        -- signal changes from the caller, as if it were always that duration:
        -- ([[], [(3, 1)], [(3, 1)]], [])
        -- But this is what I actually get:
        ([[], [], [(3, 1)]], [])
