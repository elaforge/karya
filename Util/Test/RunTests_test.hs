module Util.Test.RunTests_test where
import Util.Test
import qualified Util.Test.RunTests as RunTests


test_extractStats = do
    let f = RunTests.extractStats
    equal (f
        "===> run-test\n\
        \ok output\n\
        \++-> ok\n\
        \fail1 output\n\
        \__-> fail1\n\
        \fail2 output\n\
        \__-> fail2\n\
        \junk\n")
        ( [ "fail1 output\n__-> fail1\n"
          , "fail2 output\n__-> fail2\n"
          ]
        , 2, 3, 1
        )
