-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with 'Korvai.Metadata'.  The type itself has to be
-- defined in "Derive.Solkattu.Korvai" to avoid a circular import.
module Derive.Solkattu.Metadata (
    -- * query
    get, get_location, location_tags
    -- * add
    , date, source, korvai_t, koraippu, mohra, sarvalaghu, tirmanam
    , sequence_t, faran, exercise
    , variable_name, module_, line_number
) where
import qualified Data.Map as Map

import qualified Util.CallStack as CallStack
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.Korvai (Korvai)
import Global


-- * query

get :: Text -> Korvai -> [Text]
get tag = Map.findWithDefault [] tag . untags . Korvai._tags
    . Korvai.korvai_metadata
    where
    untags (Korvai.Tags tags) = tags

get_location :: Korvai -> Text
get_location korvai = case (g "module", g "line_number", g "variable_name") of
    (module_:_, line:_, name:_) -> name <> " (" <> module_ <> ":" <> line <> ")"
    _ -> "<unknown>"
    where g = flip get korvai

location_tags :: [Text]
location_tags = ["module", "line_number", "variable_name"]

-- * add

date :: CallStack.Stack => Int -> Int -> Int -> Korvai -> Korvai
date y m d = Korvai.with_metadata $ mempty { Korvai._date = Just date }
    where !date = Korvai.date y m d

-- | Where or from who I learned it.
source :: Text -> Korvai -> Korvai
source = with_tag "source"

korvai_t :: Korvai -> Korvai
korvai_t = with_type "korvai"

koraippu :: Korvai -> Korvai
koraippu = with_type "koraippu"

mohra :: Korvai -> Korvai
mohra = with_type "mohra"

sarvalaghu :: Korvai -> Korvai
sarvalaghu = with_type "sarvalaghu"

tirmanam :: Korvai -> Korvai
tirmanam = with_type "tirmanam"

-- | A development sequence, possibly leading to a korvai.
sequence_t :: Korvai -> Korvai
sequence_t = with_type "sequence"

faran :: Korvai -> Korvai
faran = with_type "faran"

exercise :: Korvai -> Korvai
exercise = with_type "exercise"

with_type :: Text -> Korvai -> Korvai
with_type = with_tag "type"

with_tag :: Text -> Text -> Korvai -> Korvai
with_tag k v = Korvai.with_metadata $
    mempty { Korvai._tags = Korvai.Tags (Map.singleton k [v]) }

-- ** added automatically

-- | Variable name the korvai is bound to.  Probably not much meaning except
-- to find the source.
variable_name :: Text -> Korvai -> Korvai
variable_name = with_tag "variable_name"

-- | Defining module.
module_ :: Text -> Korvai -> Korvai
module_ = with_tag "module"

-- | Line number in defining module.
line_number :: Int -> Korvai -> Korvai
line_number = with_tag "line_number" . showt
