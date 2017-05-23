-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Collect korvais into a searchable form.
module Derive.Solkattu.Db (
    module Derive.Solkattu.Db
    , module Derive.Solkattu.Dsl
    , date
) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Solkattu.All as All -- generated
import Derive.Solkattu.Dsl
       (index, realize, realizep, realize_m, realize_k1, realize_r)
import qualified Derive.Solkattu.Korvai as Korvai
import Derive.Solkattu.Korvai (date)
import qualified Derive.Solkattu.Metadata as Metadata

import Global


korvais :: [Korvai.Korvai]
korvais = All.korvais

-- * predicates

around_date :: Calendar.Day -> Integer -> Korvai.Korvai -> Bool
around_date date days =
    maybe False in_range . Korvai._date . Korvai.korvai_metadata
    where
    in_range = Num.inRange (Calendar.addDays (-days) date)
        (Calendar.addDays days date)

of_type :: Text -> Korvai.Korvai -> Bool
of_type type_ = (type_ `elem`) . Metadata.get "type"

-- * search

search :: (Korvai.Korvai -> Bool) -> IO ()
search predicate = mapM_ printk $ filter (predicate . snd) $ zip [0..] korvais

printk :: (Int, Korvai.Korvai) -> IO ()
printk (i, korvai) = Text.IO.putStr $
    showt i <> ": " <> Metadata.get_location korvai <> "\n" <> tags_text
    where
    tags_text = Text.unlines $ map ("    "<>) $ map (Text.intercalate "; ") $
        Seq.chunked 3 $ map (\(k, v) -> k <> ": " <> Text.unwords v) $
        filter ((`notElem` Metadata.location_tags) . fst) $
        Map.toAscList tags
    Korvai.Tags tags = Korvai._tags $ Korvai.korvai_metadata korvai
