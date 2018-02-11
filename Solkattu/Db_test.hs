-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Db_test where
import qualified Control.Exception as Exception
import qualified Data.Either as Either
import qualified Data.Text as Text

import Util.Test
import qualified Util.Testing as Testing
import qualified Solkattu.All as All
import qualified Solkattu.Instrument.Mridangam as Mridangam
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu

import Global


test_all = do
    forM_ All.korvais $ \korvai ->
        realizeCatch korvai >>= \case
            Right _ -> return True
            Left errs -> failure $ location korvai <> ": " <> Text.unlines errs

test_metadata = do
    forM_ All.korvais $ \korvai ->
        forM_ (Metadata.get Metadata.tSimilarTo korvai) $ \tag -> do
            unless (referentExists tag) $
                void $ failure $
                    location korvai <> ": can't find similar-to " <> showt tag

location :: Korvai.Korvai -> Text
location = Metadata.showLocation . Metadata.getLocation

referentExists :: Text -> Bool
referentExists = (`elem` map Metadata.getModuleVariable All.korvais)

realizeCatch :: Korvai.Korvai
    -> IO (Either [Text] [[Realize.Note Mridangam.Stroke]])
realizeCatch korvai =
    Exception.handle (\(Solkattu.Exception msg) -> return (Left [msg])) $ do
        let result = realize korvai
        Testing.force result
        return result

realize :: Korvai.Korvai -> Either [Text] [[Realize.Note Mridangam.Stroke]]
realize korvai
    | not (null errors) = Left errors
    | not (null warnings) = Left warnings
    | otherwise = Right $ map Sequence.flattenedNotes notes
    where
    (errors, results) = Either.partitionEithers $
        Korvai.realize Korvai.mridangam True korvai
    (notes, warnings) = second (filter (/="")) $ unzip results
