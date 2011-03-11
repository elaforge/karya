module LogView.Process_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Log as Log
import qualified Data.Time as Time

import qualified LogView.Process as Process


test_render_status = do
    let status = Map.fromList [("a", "one {click} 1"), ("b", "two")]
    let f = Process.render_status
    pprint (f status)

test_process_msg = do
    let state = Process.initial_state ""
        f state msg =
            (Process.state_last_timing new_state,
                fmap (fst . Process.extract_style) styled)
            where (styled, new_state) = Process.process_msg state msg

    msg <- Log.initialized_msg Log.Debug "hi"
    equal (f state msg)
        (Nothing, Just "*\thi\n")

    let day = Time.UTCTime (Time.ModifiedJulianDay 42)
        timing t = fmap (\m -> m { Log.msg_date = t })
            (Log.initialized_msg Log.Timer "hello")
    msg0 <- timing (day 0)
    msg1 <- timing (day 0.01)
    msg2 <- timing (day 1)

    -- first timer is suppressed
    equal (f state msg0)
        (Just msg0, Nothing)
    -- below threshold
    equal (f (state { Process.state_last_timing = Just msg0 }) msg1)
        (Just msg1, Nothing)
    -- above threshold, and timing prepended
    equal (f (state { Process.state_last_timing = Just msg0 }) msg2)
        (Just msg2, Just "***\t1s hello\n")

test_regex_style = do
    let f = Process.run_formatter
            . Process.regex_style Process.style_plain Process.msg_text_regexes
    -- TODO actually test
    pprint $ f "bah (bid \"name\") bah (vid \"2\") q"
