-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ModifyEvents_test where
import qualified Data.Text as Text

import qualified Cmd.ModifyEvents as ModifyEvents
import           Cmd.ModifyEvents (w, ws, ws1, Replacement(..))

import           Util.Test


test_pipeline :: Test
test_pipeline = do
    let f = ModifyEvents.pipeline
    equal (f id "i b1") "i b1"
    equal (f id "z | i b1") "z | i b1"
    equal (f (["q"]:) "i b1") "q | i b1"
    equal (f (drop 1) "i b1") ""
    equal (f (drop 1) "q | i b1") "i b1"

test_substitute :: Test
test_substitute = do
    let f p repl = ModifyEvents.substitute p repl
    equal (f ("sd" <> w <> w) [F 0, "| d", F 1] "sd a b")
        (Right "a | d b")
    equal (f ("sd" <> ws) [F 0] "sd a b") (Right "a b")
    equal (f ("sd" <> ws <> w) [F 0] "sd a b") (Right "a")
    equal (f ("sd" <> ws) ["df", F 0] "sd") (Right "df")
    -- No match returns the input.
    equal (f ("sd" <> w <> w) [F 0, "| d", F 1] "sd a")
        (Right "sd a")
    equal (f ("sd" <> w) [F 0, "| d", F 1] "sd a")
        (Left "no match for field 1")

test_parse :: Test
test_parse = do
    let f ps t = ModifyEvents.parse (mconcat ps) (Text.words t)
        -- parse (ModifyEvents.Parser p) = p
        -- complete = map fst . filter (null . snd)

    -- IsString instance wraps in 'literal'.
    equal (f ["a", "b"] "a b") [[]]
    equal (f [w, "b"] "a b") [[["a"]]]
    equal (f [w, ws] "a b") [[["a"], ["b"]]]
    -- ws is greedy, so the first one eats everything
    equal (f [ws, ws] "a b")
        [[["a", "b"], []], [["a"], ["b"]], [[], ["a", "b"]]]
    equal (f [ws, "b"] "a b") [[["a"]]]
    equal (f [ws, "c"] "a b") []
    equal (f [ws1, "b"] "a b") [[["a"]]]

    -- ws1 must match at least 1.
    equal (f [ws1, "b"] "b") []
