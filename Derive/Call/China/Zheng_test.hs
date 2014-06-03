module Derive.Call.China.Zheng_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


title :: String
title = "import china.zheng | open-strings = (list (4c) (4d) (4e) (4g) (4a))"

test_gliss = do
    let run gliss dest = DeriveTest.extract extract $
            DeriveTest.derive_tracks title
            [ (">", [(4, 1, gliss)]), ("*", [(4, 0, dest)])]
        extract e = (DeriveTest.e_note e, Score.initial_dynamic e)
    equal (run "gliss-a 2 1 .5" "4f")
        ([((3, 0.5, "4a"), 0.5), ((3.5, 0.5, "4g"), 0.75), ((4, 1, "4f"), 1)],
            [])
    equal (run "gliss-a -2 1" "4f")
        ([((3, 0.5, "4d"), 1), ((3.5, 0.5, "4e"), 1), ((4, 1, "4f"), 1)], [])
    equal (run "gliss-a 2 1" "4d")
        ([((3, 0.5, "4g"), 1), ((3.5, 0.5, "4e"), 1), ((4, 1, "4d"), 1)], [])

    let run2 gliss dest = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks title
            [ ("tempo", [(0, 0, "1"), (2, 0, "2")]), (">", [(4, 1, gliss)])
            , ("*", [(4, 0, dest)])
            ]
    -- 0   1   2   3   4
    -- 0   1   2   2.5 3
    equal (run2 "gliss-a 2 1t" "4f") ([2.5, 2.75, 3], [])
    equal (run2 "gliss-a 2 1s" "4f") ([2, 2.5, 3], [])
