module LogView.Process_test where

import Util.Test
import qualified Util.Log as Log
import qualified Data.Time as Time

import qualified LogView.Process as Process


test_process_msg = do
    let state = Process.initial_state ""
        f state msg =
            ( Process.render_status (Process.state_status new_state)
            , Process.state_last_timing new_state
            , fmap (fst . Process.extract_style) styled)
            where
            (new_state, styled) = Process.process_msg state msg

    equal (f state (Log.msg Log.Debug "hi"))
        ("", Nothing, Just "*\thi\n")

    let day = Time.UTCTime (Time.ModifiedJulianDay 42)
        timing t = (Log.msg Log.Debug "timer: hello") { Log.msg_date = t }
        msg0 = timing (day 0)
        msg1 = timing (day 0.01)
        msg2 = timing (day 1)

    -- first timer is suppressed
    equal (f state msg0)
        ("", Just msg0, Nothing)
    -- below threshold
    equal (f (state { Process.state_last_timing = Just msg0 }) msg1)
        ("", Just msg1, Nothing)
    -- above threshold, and timing prepended
    equal (f (state { Process.state_last_timing = Just msg0 }) msg2)
        ("", Just msg2, Just "*\t1s timer: hello\n")
