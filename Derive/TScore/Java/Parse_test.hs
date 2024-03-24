-- Copyright 2024 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.TScore.Java.Parse_test where
import qualified Data.Text.IO as Text.IO

import qualified Derive.TScore.Java.Parse as Parse

import           Global
import           Util.Test.Global


everything_score :: Text
everything_score =
    "%irama = tanggung\n\
    \%instrument = gender-barung -- comment\n\
    \3231^ dualolo cilik [\n\
    \    > '5653 | .6.56.1.\n\
    \    >  .12_ | 6.2.321.\n\
    \]\n\
    \'1235 ranbatan -- comment\n\
    \1235) tumurun gede -- comment\n"

test_roundtrip :: Test
test_roundtrip = do
    let trip = fmap Parse.unparse . Parse.parse_score
    right_equal (trip "5321 -- seleh 1\n3216 -- seleh 6\n") "5321\n3216\n"
    right_equal (trip "%piece = b") "%piece = b\n"
    right_equal (trip "1235 [ > 5 6/. _ ]") "1235 [ > 5 6/. _ ]\n"
    right_equal (trip "1235 gantung-2 seleh-5") "1235 gantung-2 seleh-5\n"
    let normalized = trip everything_score
    right_equal (const () <$> normalized) ()
    equal normalized (trip =<< normalized)
    Text.IO.putStrLn $ either id id normalized

test_unwrap :: Test
test_unwrap = do
    let trip = fmap Parse.unparse . Parse.parse_score
    let score =
            "1235 [\n\
            \    > 1235\n\
            \    > 2356\n\
            \    > 1235\n\
            \    > 2356\n\
            \]\n"
    right_equal (trip score) "1235 [ > 1235 | 1235 > 2356 | 2356 ]\n"
    right_equal (trip "1235 [ > 1235 > 2356 > 3567 ]")
        "1235 [ > 1235 > 2356 > 3567 ]\n"
