-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.ImGcMain where
import qualified Streaming.Prelude as S
import qualified System.Environment as Environment

import qualified Synth.ImGc as ImGc

import           Global


-- | I can't have a module be both importable but also be a main.  I have to
-- compile with -main-is, which makes main unqualified.  So I'd have to compile
-- it twice but Shake.Shakefile isn't set up for that.
main :: IO ()
main = do
    dirs <- Environment.getArgs
    S.mapM_ (mapM_ putStrLn) $ mconcatMap ImGc.find dirs
