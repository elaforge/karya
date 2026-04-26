-- Copyright 2025 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Merge Solkattu.Db and Solkattu.Play.
-- It could go into Db, but it would change imported modules from 109 to 467!
-- Play drags in all of Derive and Synth.Sampler.
module Solkattu.PlayDb where
import qualified Solkattu.Db as Db
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Play as Play

import           Solkattu.Db
import           Types


play :: RealTime -> Int -> Maybe Int -> IO Bool
play akshara i mbIndex = playScore akshara korvai
    where korvai = maybe id index mbIndex $ snd $ Db.scores !! i

playScore :: RealTime -> Korvai.Score -> IO Bool
playScore akshara score = do
    printScore score
    Play.play_m akshara $ case score of
        Korvai.Single k -> k
        Korvai.Tani _ _parts -> error "tani not supported yet"
