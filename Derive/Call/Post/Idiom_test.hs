module Derive.Call.Post.Idiom_test where
import Util.Control
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_arp_pizz = do
    let f = DeriveTest.extract DeriveTest.e_note
            . DeriveTest.derive_tracks_with_ui id (global "arp-pizz .5")
            . concatMap UiTest.note_track

    equal (f [[(0, 1, "+pizz -- 4c")], [(0, 1, "+pizz -- 4d")]])
        ([(0, 1, "4c"), (0.5, 1, "4d")], [])
    equal (f [[(0, 1, "4c")], [(0, 1, "+pizz -- 4d"), (1, 1, "+pizz -- 4e")]])
        ([(0, 1, "4c"), (0, 1, "4d"), (1, 1, "4e")], [])

global val = State.config#State.global_transform #= val
