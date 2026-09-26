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


test_derive_expr :: Test
test_derive_expr = do
    let run extract tracks cmd = CmdTest.extract (extractEvents extract) <$>
            CmdTest.runTracksWithPerformance tracks cmd
        f tracknum pos expr = Perf.deriveExpr UiTest.defaultBlockId
            (UiTest.mkTid tracknum) pos
            (expectRight (Parse.parseExpr expr))
        noteCall e = (Score.eventStart e, DeriveTest.eAttributes e)
    ioEqual (run noteCall [(">i1", [])] (f 1 0 "+b | +a"))
        (Right (Just (Right [(0, "+a+b")], []), []))

    let controlCall :: Signal.Control -> [(Signal.X, Signal.Y)]
        controlCall = Signal.toPairs :: Signal.Control
            -> [(Signal.X, Signal.Y)]
    ioEqual (run controlCall [("c", []), (">", [])] (f 1 0 ".5"))
        (Right (Just (Right [[(0, 0.5)]], []), []))

extractEvents :: (d -> e) -> (Either Text [d], [Log.Msg])
    -> (Either Text [e], [Text])
extractEvents f (val, logs) = (map f <$> val, map DeriveTest.showLog logs)
