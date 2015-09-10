-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Perform some scores and compare them against a saved version of what they
-- used to output.
module Derive.Regression_test where
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector
import qualified System.FilePath as FilePath

import Util.Test
import qualified Util.Thread as Thread
import Midi.Instances ()
import qualified Cmd.DiffPerformance as DiffPerformance
import qualified Derive.DeriveSaved as DeriveSaved
import Global


-- | Perform the input score and save the midi msgs to the output file.
-- This creates the -perf files.
save_performance :: FilePath -> FilePath -> IO ()
save_performance output input = do
    cmd_config <- DeriveSaved.load_cmd_config
    msgs <- DeriveSaved.perform_file cmd_config input
    DiffPerformance.save_midi output (Vector.fromList msgs)

large_test_bloom = check =<< compare_performance
    "data/bloom-perf" "data/bloom"
large_test_pnovla = check =<< compare_performance
    "data/pnovla-perf" "data/pnovla"
large_test_viola_sonata = check =<< compare_performance
    "data/viola-sonata-perf" "data/viola-sonata"

-- * implementation

compare_performance :: FilePath -> FilePath -> IO Bool
compare_performance saved score = timeout score $ do
    cmd_config <- DeriveSaved.load_cmd_config
    expected <- either (errorIO . untxt) (return . Vector.toList)
        =<< DiffPerformance.load_midi saved
    got <- DeriveSaved.perform_file cmd_config score
    let name = FilePath.takeFileName score
    dir <- tmp_dir "regression"
    (maybe_diff, wrote_files) <- DiffPerformance.diff_lines name dir
        (DiffPerformance.show_midi expected) (DiffPerformance.show_midi got)
    case maybe_diff of
        Nothing -> return True
        Just diff -> do
            Text.IO.putStrLn diff
            putStrLn $ "wrote: " <> unwords wrote_files
            return False

timeout :: String -> IO a -> IO a
timeout fname = maybe (errorIO msg) return <=< Thread.timeout 60
    where
    msg = fname
        ++ ": timed out, this can happen when too many msgs are different"
