-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Db_test where
import qualified Control.Exception as Exception
import qualified Data.Either as Either
import qualified Data.Text as Text

import qualified Util.Test.Testing as Testing
import qualified Solkattu.All as All
import qualified Solkattu.Dsl.Solkattu as Dsl.Solkattu
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Realize as Realize
import qualified Solkattu.S as S
import qualified Solkattu.Solkattu as Solkattu
import qualified Solkattu.Tags as Tags

import           Global
import           Util.Test


allKorvais :: [(Int, Korvai.Korvai)]
allKorvais =
    [(i, korvai) | (i, Korvai.Single korvai) <- zip [0..] All.scores]

test_all :: Test
test_all = do
    mapM_ (uncurry testKorvai) allKorvais

testKorvai :: Int -> Korvai.Korvai -> Test
testKorvai i korvai =
    forM_ (Korvai.korvaiInstruments korvai) $
        \(name, Korvai.GInstrument inst) ->
    realizeCatch inst korvai >>= \case
        Right _ -> return ()
        Left errs -> failure $ location korvai i <> ": " <> name <> ": "
            <> Text.unlines errs

test_lints :: Test
test_lints = do
    mapM_ testLint allKorvais

testLint :: (Int, Korvai.Korvai) -> Test
testLint (i, korvai) = when (lints /= "") $
    failure $ location korvai i <> ": " <> lints
    where
    lints = Dsl.Solkattu.allLints korvai

test_metadata :: Test
test_metadata = do
    forM_ allKorvais $ \(i, korvai) ->
        forM_ (Metadata.getTag Tags.similarTo (Korvai.korvaiMetadata korvai)) $
            \tag -> unless (referentExists tag) $ void $ failure $
                location korvai i <> ": can't find similarTo " <> showt tag

location :: Korvai.Korvai -> Int -> Text
location korvai i =
    Metadata.showLocation (Korvai._location (Korvai.korvaiMetadata korvai))
    <> " All.scores!!" <> showt i

referentExists :: Text -> Bool
referentExists = (`elem` map Metadata.moduleVariable All.scores)

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
    | not (null warnings) = Left $ map showt warnings
    | otherwise = Right $ map S.flattenedNotes notes
    where
    (errors, results) = Either.partitionEithers $
        Korvai.realize inst korvai
    (notes, warnings) = second concat $ unzip results
