-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities shared between drum patches.
module Synth.Sampler.Patch.Lib.Drum where
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.Seq as Seq
import qualified Synth.Sampler.Patch.Util as Util
import qualified Synth.Shared.Config as Config
import Global


-- | Generate 'articulationSamples'.  This could have been TH but it seems not
-- worth it.
--
-- This expects a subdirectory for each articulation, whose name is the same
-- as the Articulation constructor, and sorts by the 4th field e.g.
-- {Thom,Nam,...}/x-x-x-$vel-...
makeFileList :: FilePath -> [FilePath] -> String -> IO ()
makeFileList dir articulations variableName = do
    putStrLn $ variableName <> " :: Articulation -> [FilePath]"
    putStrLn $ variableName <> " = \\case"
    forM_ articulations $ \art -> do
        fns <- Seq.sort_on filenameVelocity <$>
            Directory.listDirectory (Config.unsafeSamplerRoot </> dir </> art)
        putStrLn $ "    " <> show art <> " ->"
        let indent = replicate 8 ' '
        putStrLn $ indent <> "[ " <> show (head fns)
        mapM_ (\fn -> putStrLn $ indent <> ", " <> show fn) (tail fns)
        putStrLn $ indent <> "]"

filenameVelocity :: FilePath -> Int
filenameVelocity fname = case Seq.split "-" fname of
    _ : _ : _ : lowVel : _ -> read lowVel
    _ -> error fname
