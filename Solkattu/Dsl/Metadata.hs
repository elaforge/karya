-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | DSL functions to add metadata to Korvais.
module Solkattu.Dsl.Metadata (
    comment, date, source, similarTo
    , recording
    , korvaiT, koraippu, mohra, mohraKorvai, sarvalaghu, tirmanam, sollu
    , sequenceT, faran, exercise, trikalam
    , withType
) where
import           GHC.Stack (HasCallStack)
import qualified Text.Read as Read

import qualified Util.Lists as Lists
import qualified Solkattu.Korvai as Korvai
import           Solkattu.Korvai (Korvai)
import qualified Solkattu.Metadata as Metadata
import qualified Solkattu.Tags as Tags

import           Global


-- | Attach a generic comment.
comment :: Text -> Korvai -> Korvai
comment = withTag Tags.comment

date :: HasCallStack => Int -> Int -> Int -> Korvai -> Korvai
date y m d = Korvai.withKorvaiMetadata $ mempty { Korvai._date = Just date }
    where !date = Metadata.makeDate y m d

-- | Where or from who I learned it.
source :: Text -> Korvai -> Korvai
source = withTag Tags.source

-- | This could be considered a variant of the other.  Takes "Module"
-- "variableName", since the location is added later in "Solkattu.All".
-- The link is verified in Db_test.
similarTo :: Text -> Text -> Korvai -> Korvai
similarTo module_ variableName =
    withTag Tags.similarTo (module_ <> "." <> variableName)

-- | A recording where the clip is played.
recording :: HasCallStack => Text -- ^ URL to the recording or video
    -> String -- ^ start and end time of the clip within the recording
    -> Korvai -> Korvai
recording url range = withTag Tags.recording $
    Metadata.showRecording url $ parseRange range
    -- showRecording will turn it right back into a string, but by parsing here
    -- it gets validated.

parseRange :: String -> Maybe (Metadata.Time, Maybe Metadata.Time)
parseRange "" = Nothing
parseRange str = case range of
    Nothing -> error $ "no parse for time range: " <> show str
    Just (start, Just end) | end <= start ->
        error $ "end before start: " <> show (start, end)
    Just (start, end) -> Just (start, end)
    where
    range = (,) <$> parseTime start
        <*> (if end == "" then pure Nothing else Just <$> parseTime end)
    (start, end) = second (dropWhile (=='-')) $ break (=='-') str

parseTime :: String -> Maybe (Int, Int, Int)
parseTime str = case mapM Read.readMaybe $ Lists.split (==':') str of
    Just [h, m, s] -> Just (h, m, s)
    Just [m, s] -> Just (0, m, s)
    Just [s] -> Just (0, 0, s)
    _ -> Nothing

-- * types

korvaiT :: Korvai -> Korvai
korvaiT = withType "korvai"

koraippu :: Korvai -> Korvai
koraippu = withType "koraippu"

mohra :: Korvai -> Korvai
mohra = withType "mohra"

mohraKorvai :: Korvai -> Korvai
mohraKorvai = withType "mohra-korvai"

sarvalaghu :: Korvai -> Korvai
sarvalaghu = withType "sarvalaghu"

-- | A short cadence, suitable to end a phrase or section.
tirmanam :: Korvai -> Korvai
tirmanam = withType "tirmanam"

-- | Shorter than a tirmanam and usually not having structure, just a fill.
sollu :: Korvai -> Korvai
sollu = withType "sollu"

-- | A development sequence, possibly leading to a korvai.
sequenceT :: Korvai -> Korvai
sequenceT = withType "sequence"

faran :: Korvai -> Korvai
faran = withType "faran"

exercise :: Korvai -> Korvai
exercise =
    replaceSectionTags Tags.type_ Tags.exercise . withType Tags.exercise
    -- Replace the inferred development and ending types, exercises generally
    -- don't have those things.

trikalam :: Korvai -> Korvai
trikalam = withType "trikalam"

withType :: Text -> Korvai -> Korvai
withType = withTag Tags.type_

-- * util

withTag :: Text -> Text -> Korvai -> Korvai
withTag k v = Korvai.withKorvaiMetadata $
    mempty { Korvai._tags = Tags.tag k v }

replaceSectionTags :: Text -> Text -> Korvai -> Korvai
replaceSectionTags k v = Korvai.modifySections $ Tags.replace k v
