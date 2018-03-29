-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | DSL functions to add metadata to Korvais.
module Solkattu.MetadataGlobal (
    comment, date, source, similarTo
    , recording
    , korvaiT, koraippu, mohra, sarvalaghu, tirmanam
    , sequenceT, faran, exercise, trikalam
) where
import qualified Util.CallStack as CallStack
import qualified Solkattu.Korvai as Korvai
import qualified Solkattu.Metadata as Metadata
import Solkattu.Korvai (Korvai)
import qualified Solkattu.Tags as Tags
import Global


-- | Attach a generic comment.
comment :: Text -> Korvai -> Korvai
comment = withTag Tags.comment

date :: CallStack.Stack => Int -> Int -> Int -> Korvai -> Korvai
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
recording :: CallStack.Stack => Text -- ^ URL to the recording or video
    -> Maybe (Metadata.Time, Metadata.Time)
    -- ^ start and end time of the clip within the recording
    -> Korvai -> Korvai
recording url maybeRange =
    withTag Tags.recording (Metadata.showRecording url maybeRange)

-- * types

korvaiT :: Korvai -> Korvai
korvaiT = withType "korvai"

koraippu :: Korvai -> Korvai
koraippu = withType "koraippu"

mohra :: Korvai -> Korvai
mohra = withType "mohra"

sarvalaghu :: Korvai -> Korvai
sarvalaghu = withType "sarvalaghu"

tirmanam :: Korvai -> Korvai
tirmanam = withType "tirmanam"

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
