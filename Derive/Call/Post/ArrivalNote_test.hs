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
