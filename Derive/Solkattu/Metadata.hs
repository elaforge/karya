-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with 'Korvai.Metadata'.  The type itself has to be
-- defined in "Derive.Solkattu.Korvai" to avoid a circular import.
module Derive.Solkattu.Metadata (
    -- * query
    get, get_location, set_location, show_location, get_module_variable
    -- * add
    , comment, date, source, similar_to, t_similar_to
    , korvai_t, koraippu, mohra, sarvalaghu, tirmanam
    , sequence_t, faran, exercise, trikalam
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.Korvai (Korvai)
import Global


-- * query

get :: Text -> Korvai -> [Text]
get tag = Map.findWithDefault [] tag . untags . Korvai._tags
    . Korvai.korvai_metadata
    where untags (Korvai.Tags tags) = tags

get_location :: Korvai -> Korvai.Location
get_location = Korvai._location . Korvai.korvai_metadata

show_location :: Korvai.Location -> Text
show_location (module_, line, name) =
    name <> " (" <> module_ <> ":" <> showt line <> ")"

set_location :: Korvai.Location -> Korvai -> Korvai
set_location loc korvai = korvai
    { Korvai.korvai_metadata = (Korvai.korvai_metadata korvai)
        { Korvai._location = loc
        }
    }

get_module_variable :: Korvai -> Text
get_module_variable korvai = last (Text.splitOn "." module_) <> "." <> name
    where (module_, _, name) = get_location korvai

-- * add

-- | Attach a generic comment.
comment :: Text -> Korvai -> Korvai
comment = with_tag "comment"

date :: CallStack.Stack => Int -> Int -> Int -> Korvai -> Korvai
date y m d = Korvai.with_metadata $ mempty { Korvai._date = Just date }
    where !date = Korvai.date y m d

-- | Where or from who I learned it.
source :: Text -> Korvai -> Korvai
source = with_tag "source"

-- | This could be considered a variant of the other.  Takes "Module"
-- "variable_name", since the location is added later in "Derive.Solkattu.All".
-- The link is verified in Db_test.
similar_to :: Text -> Text -> Korvai -> Korvai
similar_to module_ variable_name =
    with_tag t_similar_to (module_ <> "." <> variable_name)

t_similar_to :: Text
t_similar_to = "similar_to"

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

trikalam :: Korvai -> Korvai
trikalam = with_type "trikalam"

with_type :: Text -> Korvai -> Korvai
with_type = with_tag "type"

with_tag :: Text -> Text -> Korvai -> Korvai
with_tag k v = Korvai.with_metadata $
    mempty { Korvai._tags = Korvai.Tags (Map.singleton k [v]) }
