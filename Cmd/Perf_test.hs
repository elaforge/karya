-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Perf_test where
import qualified Util.Log as Log
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Perf as Perf
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import Global


test_derive_expr = do
    let run extract tracks cmd = CmdTest.extract (extract_events extract) <$>
            CmdTest.run_perf_tracks tracks cmd
        f tracknum pos expr = Perf.derive_expr UiTest.default_block_id
            (UiTest.mk_tid tracknum) pos
            (expect_right (Parse.parse_expr expr))
        note_call e = (Score.event_start e, DeriveTest.e_attributes e)
    io_equal (run note_call [(">i1", [])] (f 1 0 "+b | n +a"))
        (Right (Just (Right [(0, "+a+b")], []), []))

    let control_call :: Signal.Control -> [(Signal.X, Signal.Y)]
        control_call = Signal.unsignal :: Signal.Control
            -> [(Signal.X, Signal.Y)]
    io_equal (run control_call [("c", []), (">", [])] (f 1 0 ".5"))
        (Right (Just (Right [[(0, 0.5)]], []), []))

extract_events :: (d -> e) -> (Either Text [d], [Log.Msg])
    -> (Either Text [e], [String])
extract_events f (val, logs) = (map f <$> val, map DeriveTest.show_log logs)
