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
profileFile = do
    cmdConfig <- DeriveSaved.loadCmdConfig
    results <- perform cmdConfig "save/wayang/speed-test"
    let maybePerf = CmdTest.ePerformance (D.bid "wayang/top") $
            ResponderTest.resultCmd (last results)
    putStrLn "perf:"
    whenJust maybePerf $ \perf -> do
        putStrLn $ "events: " <> show (Vector.length (Cmd.perfEvents perf))
        putStrLn "logs:"
        mapM_ prettyp $ filter (not . Cache.isCacheLog) (Cmd.perfLogs perf)
    pure ()

perform :: Cmd.Config -> FilePath -> IO [ResponderTest.Result]
perform cmdConfig fname = do
    let states = (Ui.empty, Cmd.initialState cmdConfig)
    let continue = ResponderTest.continueAll [] 8
    result <- ResponderTest.respondCmd states $ Save.load fname
    results1 <- continue result
    -- The first one is cancelled because of loading the defs file.
    results2 <- continue (last results1)
    pure $ results1 ++ results2
