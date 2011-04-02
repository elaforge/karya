module LogView.Process_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Log as Log
import qualified Util.Regex as Regex

import qualified LogView.Process as Process


test_render_status = do
    let status = Map.fromList [("a", "one {click} 1"), ("b", "two")]
    let f = Process.render_status
    pprint (f status)

test_process_msg = do
    let state = (Process.initial_state "")
            { Process.state_catch_patterns =
                    [("catch", Regex.make "^catch me: (.*), (.*)")]
            }
        f state msg = (fmap (fst . Process.extract_style) styled)
            where styled = fst $ Process.process_msg state msg

    -- test general formatting
    msg <- Log.initialized_msg Log.Debug "hi"
    equal (f state msg) (Just "*\thi\n")

    -- test catch patterns
    msg <- Log.initialized_msg Log.Debug "catch me: title, stuff"
    equal (Process.state_status $ snd $ Process.process_msg state msg)
        (Map.fromList [("title", "stuff")])

test_regex_style = do
    let f = Process.run_formatter
            . Process.regex_style Process.style_plain Process.msg_text_regexes
    -- TODO actually test
    pprint $ f "bah (bid \"name\") bah (vid \"2\") q"
