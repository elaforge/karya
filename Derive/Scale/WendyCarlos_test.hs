module Derive.Scale.WendyCarlos_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import Global


test_pitches = do
    let run title = DeriveTest.extract DeriveTest.e_nns
            . DeriveTest.derive_tracks ("scale=alpha" <> title)
            . UiTest.note_track
    equal (run "" [(0, 1, "1a"), (1, 1, "1b")])
        ([[(0, 12)], [(1, 12.78)]], [])
    equal (run " | %a0-nn=1" [(0, 1, "1a"), (1, 1, "1b")])
        ([[(0, 13)], [(1, 13.78)]], [])
