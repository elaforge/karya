module Derive.Call.Post.Map_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_mapc = do
    let run extract = DeriveTest.extract extract . DeriveTest.derive_tracks ""
    strings_like
        (snd $ run (DeriveTest.e_control "c")
            [("> | mapc in_valid \"(smooth 2)", [(0, 8, "")])])
        ["expected ControlName"]
    equal (run (DeriveTest.e_control "c")
        [ ("> | mapc c \"(smooth 2)", [(0, 8, "")])
        , ("c", [(0, 0, "0"), (4, 0, "1")])
        ])
        ([[(0, 0), (5, 0.5), (6, 1)]], [])
