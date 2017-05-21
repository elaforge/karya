-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with 'Korvai.Metadata'.  The type itself has to be
-- defined in "Derive.Solkattu.Korvai" to avoid a circular import.
module Derive.Solkattu.Metadata (
    date, source, korvai_t, koraippu, mohra, sarvalaghu, tirmanam
    , sequence_t, faran, exercise
) where
import qualified Data.Map as Map

import qualified Util.CallStack as CallStack
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.Korvai (Korvai)
import Global


date :: CallStack.Stack => Int -> Int -> Int -> Korvai -> Korvai
date y m d = Korvai.with_metadata $ mempty { Korvai._date = Just date }
    where !date = Korvai.make_date y m d

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
