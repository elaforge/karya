-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Solkattu.Db_test where
import qualified Control.Exception as Exception
import qualified Data.Either as Either
import qualified Data.Text as Text

import Util.Test
import qualified Util.Testing as Testing
import qualified Derive.Solkattu.All as All
import qualified Derive.Solkattu.Instrument.Mridangam as Mridangam
import qualified Derive.Solkattu.Korvai as Korvai
import qualified Derive.Solkattu.Metadata as Metadata
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu

import Global


test_all = do
    forM_ All.korvais $ \korvai ->
        realize_catch korvai >>= \x -> case x of
            Right _ -> return True
            Left errs -> failure $ location korvai <> ": " <> Text.unlines errs

test_metadata = do
    forM_ All.korvais $ \korvai ->
        forM_ (Metadata.get Metadata.t_similar_to korvai) $ \tag -> do
            unless (referent_exists tag) $
                void $ failure $
                    location korvai <> ": can't find similar-to " <> showt tag

location :: Korvai.Korvai -> Text
location = Metadata.show_location . Metadata.get_location

referent_exists :: Text -> Bool
referent_exists = (`elem` map Metadata.get_module_variable All.korvais)

realize_catch :: Korvai.Korvai
    -> IO (Either [Text] [[Realize.Note Mridangam.Stroke]])
realize_catch korvai =
    Exception.handle (\(Solkattu.Exception msg) -> return (Left [msg])) $ do
        let result = realize korvai
        Testing.force result
        return result

realize :: Korvai.Korvai -> Either [Text] [[Realize.Note Mridangam.Stroke]]
realize korvai
    | not (null errors) = Left errors
    | not (null warnings) = Left warnings
    | otherwise = Right $ map Sequence.flattened_notes notes
    where
    (errors, results) = Either.partitionEithers $
        Korvai.realize Korvai.mridangam True korvai
    (notes, warnings) = second (filter (/="")) $ unzip results
