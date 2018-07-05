-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Test.RunTests_test where
import qualified Data.Text.Lazy as Text.Lazy

import Util.Test
import qualified Util.Test.RunTests as RunTests


test_extractStats = do
    let f = RunTests.extractStats
    let output =
            "===> run-test\n\
            \ok output\n\
            \++-> ok\n\
            \fail1 output\n\
            \__-> fail1\n\
            \fail2 output\n\
            \__-> fail2\n\
            \junk\n"
    equal (RunTests.extractFailures $ drop 1 $ Text.Lazy.lines output)
        [ Nothing
        , Just "fail1 output\n__-> fail1\nfail2 output\n"
        , Just "fail2 output\n__-> fail2\njunk\n"
        ]
    equal (f output)
        ( [ "fail1 output\n__-> fail1\nfail2 output\n"
          , "fail2 output\n__-> fail2\njunk\n"
          ]
        , 2, 3, 1
        )
