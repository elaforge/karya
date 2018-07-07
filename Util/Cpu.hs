-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Get info about CPUs.
module Util.Cpu where
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.Environment as Environment
import qualified System.Info
import qualified System.Process as Process

import qualified Text.Read as Read


-- | Get number of physical cores.  This can be overidden with a CPUS
-- environment variable.  This is useful if you are running on a VM in CI
-- and don't agree with how many cores it claims to have.
physicalCores :: IO Int
physicalCores = maybe getPhysicalCores return =<< envCores

getPhysicalCores :: IO Int
getPhysicalCores = case System.Info.os of
    "darwin" -> read <$>
        Process.readProcess "/usr/sbin/sysctl" ["-n", "hw.physicalcpu"] ""
    "linux" -> linuxPhysicalCores <$> Text.IO.readFile "/proc/cpuinfo"
    _ -> error $ "unknown platform: " ++ System.Info.os

envCores :: IO (Maybe Int)
envCores = (Read.readMaybe =<<) <$> Environment.lookupEnv "CPUS"

-- | Parse /proc/cpuinfo for physical cpu count.
linuxPhysicalCores :: Text -> Int
linuxPhysicalCores = length . unique . map cpu . Text.splitOn "\n\n"
    where
    -- unique pairs of (physical id, core id)
    cpu = filter (\s -> any (`Text.isPrefixOf` s) ["physical id", "core id"])
        . Text.lines

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList
