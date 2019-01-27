-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Solkattu.Part where
import qualified Util.Styled as Styled
import qualified Solkattu.Korvai as Korvai

import Global


data Part = K !Korvai.Korvai !Index | Comment !Text
    deriving (Eq, Show)
-- | The indices start at 1, since the section display also does.
data Index = All | Index !Int | Range !Int !Int
    deriving (Eq, Show)

realizeParts :: (Korvai.Korvai -> IO ()) -> [Part] -> IO ()
realizeParts realize = mapM_ part
    where
    -- TODO it would be nice to print the name, but it's only available through
    -- Db.
    part (K korvai i) = realize $ index i korvai
    part (Comment comment) = Styled.printLn $ Styled.bold comment

index :: Index -> Korvai.Korvai -> Korvai.Korvai
index idx korvai = case Korvai.korvaiSections korvai of
    Korvai.Mridangam sections ->
        korvai { Korvai.korvaiSections = Korvai.Mridangam (get sections) }
    Korvai.Sollu sections ->
        korvai { Korvai.korvaiSections = Korvai.Sollu (get sections) }
    where
    get xs = case idx of
        All -> xs
        Index i -> [xs !! (i-1)]
        Range i j -> take (max 0 (j-1)) $ drop i xs
