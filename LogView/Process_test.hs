-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module LogView.Process_test where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import Util.Test

import qualified LogView.Process as Process


test_render_status = do
    let status = Map.fromList [("a", "one {click} 1"), ("b", "two")]
    let f = Process.render_status
    pprint (f status)

test_process_msg = do
    let state = (Process.initial_state "")
            { Process.state_catch_patterns = [Process.global_status_pattern]
            , Process.state_status = Map.fromList [("key", "val1")]
            }
        f state msg = fmap (UTF8.toString . Process.style_text) styled
            where styled = fst $ Process.process_msg state msg

    -- test general formatting
    msg <- Log.initialized_msg Log.Debug "hi"
    equal (f state msg) (Just "*\thi\n")

    -- test catch patterns
    let set_status key val = Log.initialized_msg Log.Debug $
            "global status: " <> key <> " -- " <> val
    msg <- set_status "key" "val2"
    equal (Process.state_status $ snd $ Process.process_msg state msg)
        (Map.fromList [("key", "val2")])
    -- Key is deleted.
    msg <- set_status "key" ""
    equal (Process.state_status $ snd $ Process.process_msg state msg)
        Map.empty

test_flatten_ranges = do
    let f = Process.flatten_ranges 'z'
    equal (f [((2, 4), 'a')]) [(2, 'z'), (2, 'a')]
    -- Ranges are half-open, so (4, 4) is empty.
    -- 01234567
    -- ..aaaa
    -- ...bb
    equal (f [((2, 6), 'a'), ((3, 5), 'b'), ((4, 4), 'c')])
        [(2, 'z'), (1, 'a'), (1, 'b'), (1, 'b'), (1, 'a')]

test_regex_style = do
    let f = UTF8.toString . Process.style_style . Process.run_formatter
            . Process.regex_style Process.style_plain Process.msg_text_regexes
    equal (f "{a}") "CCC"
    equal (f "x {a} x") "AACCCAA"
    equal (f "x (bid \"a\") x")
             "AADDDDDDDDDAA"
    equal (f "x { a (bid \"name\") b } x")
             "AACCCCDDDDDDDDDDDDCCCCAA"
    let len = ByteString.length (UTF8.fromString "字")
    equal (f "x { 字 }")
        ("AACC" ++ replicate len 'C' ++ "CC")
