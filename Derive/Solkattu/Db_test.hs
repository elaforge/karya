-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Db_test where
import qualified Data.Either as Either
import qualified Data.Text as Text

import Util.Test
import qualified Derive.Solkattu.All as All
import qualified Derive.Solkattu.Instrument.Mridangam as Mridangam
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Metadata as Metadata
import qualified Derive.Solkattu.Realize as Realize

import Global


test_all = do
    forM_ All.korvais $ \korvai -> case realize korvai of
        Right _ -> return True
        Left errs -> failure $ Metadata.get_location korvai <> ": "
            <> Text.unlines errs

realize :: Korvai.Korvai -> Either [Text] [[Realize.Note Mridangam.Stroke]]
realize korvai
    | not (null errors) = Left errors
    | not (null warnings) = Left warnings
    | otherwise = Right $ map (map snd) notes
    where
    (errors, results) = Either.partitionEithers $
        Korvai.realize Korvai.mridangam True korvai
    (notes, warnings) = second (filter (/="")) $ unzip results
