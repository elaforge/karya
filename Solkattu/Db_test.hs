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

import Global
import Util.Test


test_all = do
    forM_ (zip [0..] All.korvais) testKorvai

testKorvai :: (Int, Korvai.Korvai) -> IO ()
testKorvai (i, korvai) =
    forM_ (Korvai.korvaiInstruments korvai) $
        \(name, Korvai.GInstrument inst) ->
    realizeCatch inst korvai >>= \case
        Right _ -> return True
        Left errs -> failure $ location korvai i <> ": " <> name <> ": "
            <> Text.unlines errs

_test0 = either mconcat (const "") $ realize Korvai.konnakol (All.korvais!!114)

test_lints = do
    forM_ (zip [0..] All.korvais) testLint

testLint :: (Int, Korvai.Korvai) -> IO Bool
testLint (i, korvai)
    | lints == "" = return True
    | otherwise = failure $ location korvai i <> ": " <> lints
    where
    lints = Dsl.Solkattu.allLints korvai

test_metadata = do
    forM_ (zip [0..] All.korvais) $ \(i, korvai) ->
        forM_ (Metadata.korvaiTag Tags.similarTo korvai) $ \tag -> do
            unless (referentExists tag) $
                void $ failure $
                    location korvai i <> ": can't find similarTo " <> showt tag

location :: Korvai.Korvai -> Int -> Text
location korvai i = Metadata.showLocation (Metadata.getLocation korvai)
    <> " All.korvais!!" <> showt i

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
    | not (null alignErrors) = Left $ map showt alignErrors
    | otherwise = Right $ map S.flattenedNotes notes
    where
    (errors, results) = Either.partitionEithers $
        Korvai.realize inst korvai
    (notes, alignErrors) = second (mapMaybe id) $ unzip results
