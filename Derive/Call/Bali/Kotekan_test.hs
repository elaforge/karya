module Derive.Call.Bali.Kotekan_test where
import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_nyogcag = do
    let run = derive extract (inst_title <> " | nyog")
        extract e = (Score.event_start e, DeriveTest.e_inst e)
    let notes = [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")]
    equal (run notes) ([(0, "i1"), (1, "i2"), (2, "i1")], [])

test_noltol = do
    let run arg = derive extract (" | noltol " <> arg)
        extract e = (Score.event_start e, DeriveTest.e_inst e,
            DeriveTest.e_attributes e)
    let notes = [(0, 1, "n >i1 -- 4c"), (1, 1, "n >i2 -- 4d"),
            (2, 1, "n >i1 -- 4e")]
    equal (run "2.5" notes)
        ([(0, "i1", "+"), (1, "i2", "+"), (2, "i1", "+")], [])
    equal (run "2" notes)
        ([ (0, "i1", "+"), (1, "i1", "+loose+mute"), (1, "i2", "+")
         , (2, "i1", "+")
         ], [])

    let run2 = derive extract (inst_title <> " | noltol 1 | nyog")
    equal (run2 [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e"), (3, 1, "4f")])
        ([ (0, "i1", "+"), (1, "i1", "+loose+mute")
         , (1, "i2", "+"), (2, "i2", "+loose+mute")
         , (2, "i1", "+"), (3, "i2", "+")
         ], [])

derive :: (Score.Event -> a) -> String -> [UiTest.EventSpec] -> ([a], [String])
derive extract title notes =
    DeriveTest.extract extract $ DeriveTest.derive_tracks $
    UiTest.note_spec (title, notes, [])

inst_title :: String
inst_title = "i3 | inst-polos = >i1 | inst-sangsih = >i2"
