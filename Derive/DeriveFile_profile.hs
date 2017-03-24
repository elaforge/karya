-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.DeriveFile_profile where
import qualified Data.Vector as Vector

import Util.Test
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.Save as Save

import qualified Derive.Cache as Cache
import qualified Derive.DeriveSaved as DeriveSaved
import qualified D
import Global


-- Run by hand to manually derive a score to see how long it takes.
profile_file = do
    cmd_config <- DeriveSaved.load_cmd_config
    results <- perform cmd_config "save/wayang/speed-test"
    let maybe_perf = CmdTest.e_performance (D.bid "wayang/top") $
            ResponderTest.result_cmd (last results)
    putStrLn "perf:"
    whenJust maybe_perf $ \perf -> do
        putStrLn $ "events: " <> show (Vector.length (Cmd.perf_events perf))
        putStrLn "logs:"
        mapM_ prettyp $ filter (not . Cache.is_cache_log) (Cmd.perf_logs perf)
    return ()

perform :: Cmd.Config -> FilePath -> IO [ResponderTest.Result]
perform cmd_config fname = do
    let states = (Ui.empty, Cmd.initial_state cmd_config)
    let continue = ResponderTest.continue_all 8
    result <- ResponderTest.respond_cmd states $ Save.load fname
    results1 <- continue result
    -- The first one is cancelled because of loading the defs file.
    results2 <- continue (last results1)
    return $ results1 ++ results2
