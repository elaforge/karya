-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tags type and conventional tag values.
module Solkattu.Tags where
import qualified Data.Map as Map
import qualified Util.Maps as Maps
import Global


newtype Tags = Tags (Map Text [Text])
    deriving (Eq, Show, Pretty)

instance Semigroup Tags where
    Tags t1 <> Tags t2 = Tags (Maps.mappend t1 t2)
instance Monoid Tags where
    mempty = Tags mempty
    mappend = (<>)

tag :: Text -> Text -> Tags
tag k v = Tags (Map.singleton k [v])

untags :: Tags -> Map Text [Text]
untags (Tags tags) = tags

replace :: Text -> Text -> Tags -> Tags
replace k v (Tags tags) = Tags $ Map.insert k [v] tags

-- * tags

comment :: Text
comment = "comment"

source :: Text
source = "source"

type_ :: Text
type_ = "type"

gharana :: Text
gharana = "gharana"

withType :: Text -> Tags
withType = tag type_

exercise :: Text
exercise = "exercise"

-- ** per-korvai

similarTo :: Text
similarTo = "similar_to"

recording :: Text
recording = "recording"

-- ** per-section

development, variation, ending :: Text
development = "development"
variation = "variation"
ending = "ending"

eddupu :: Text
eddupu = "eddupu"

date :: Text
date = "date"

times :: Text
times = "times"
