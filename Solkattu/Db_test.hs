-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Db_test where
import qualified Control.Exception as Exception
import qualified Data.Either as Either
import qualified Data.Text as Text

import Util.Test
import qualified Util.Test.Testing as Testing
import qualified Solkattu.All as All
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.Sequence as Sequence
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags

import Global


test_all = do
    forM_ All.korvais testKorvai

testKorvai :: Korvai.Korvai -> IO ()
testKorvai korvai =
    forM_ (Korvai.korvaiInstruments korvai) $
        \(name, Korvai.GInstrument inst) ->
    realizeCatch inst korvai >>= \case
        Right _ -> return True
        Left errs -> failure $ location korvai <> ": " <> name <> ": "
            <> Text.unlines errs

test_metadata = do
    forM_ All.korvais $ \korvai ->
        forM_ (Metadata.korvaiTag Tags.similarTo korvai) $ \tag -> do
            unless (referentExists tag) $
                void $ failure $
                    location korvai <> ": can't find similarTo " <> showt tag

location :: Korvai.Korvai -> Text
location = Metadata.showLocation . Metadata.getLocation

referentExists :: Text -> Bool
referentExists = (`elem` map Metadata.getModuleVariable All.korvais)

realizeCatch :: Solkattu.Notation stroke => Korvai.Instrument stroke
    -> Korvai.Korvai -> IO (Either [Text] [[Realize.Note stroke]])
realizeCatch inst korvai =
    Exception.handle (\(Solkattu.Exception msg) -> return (Left [msg])) $ do
        let result = realize inst korvai
        Testing.force result
        return result

realize :: Solkattu.Notation stroke => Korvai.Instrument stroke -> Korvai.Korvai
    -> Either [Text] [[Realize.Note stroke]]
realize inst korvai
    | not (null errors) = Left errors
    | not (null warnings) = Left warnings
    | otherwise = Right $ map Sequence.flattenedNotes notes
    where
    (errors, results) = Either.partitionEithers $
        Korvai.realize inst True korvai
    (notes, warnings) = second (filter (/="")) $ unzip results
