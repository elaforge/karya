module Derive.Call.Post.Idiom_test where
import Util.Control
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_pizz_arp = do
    let f = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks_with_ui id (global "pizz-arp .5")
            . concatMap UiTest.note_track

    equal (f [[(0, 1, "+pizz -- 4c")], [(0, 1, "+pizz -- 4d")]])
        ([(0, 1, "4c"), (0.5, 1, "4d")], [])
    equal (f [[(0, 1, "4c")], [(0, 1, "+pizz -- 4d"), (1, 1, "+pizz -- 4e")]])
        ([(0, 1, "4c"), (0, 1, "4d"), (1, 1, "4e")], [])

test_avoid_overlap = do
    let f = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks_with_ui id (global "avoid-overlap .5")
            . UiTest.note_track
    equal (f [(0, 1, "4c"), (1, 1, "4d")])
        ([(0, 1, "4c"), (1, 1, "4d")], [])
    equal (f [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4d"), (3, 1, "4c")])
        ([(0, 1, "4c"), (1, 0.5, "4d"), (2, 1, "4d"), (3, 1, "4c")], [])

global val = State.config#State.global_transform #= val
