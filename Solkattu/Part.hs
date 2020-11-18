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

instance Num Index where
    fromInteger = Index . fromInteger
    (+) = error "Index has no +"
    -- So -1 works.
    Index a - Index b = Index (a-b)
    a - b = error $ "no (-) for " <> show (a, b)
    (*) = error "Index has no *"
    abs = error "Index has no abs"
    signum = error "Index has no signum"

realizeParts :: (Korvai.Korvai -> IO ()) -> [Part] -> IO ()
realizeParts realize = mapM_ part
    where
    -- TODO it would be nice to print the name, but it's only available through
    -- Db.
    part (K korvai i) = realize $ index i korvai
    part (Comment comment) = Styled.printLn $ Styled.bold comment

index :: Index -> Korvai.Korvai -> Korvai.Korvai
index idx korvai = case Korvai.korvaiSections korvai of
    Korvai.TMridangam sections ->
        korvai { Korvai.korvaiSections = Korvai.TMridangam (get sections) }
    Korvai.TSollu sections ->
        korvai { Korvai.korvaiSections = Korvai.TSollu (get sections) }
    where
    get xs = case idx of
        All -> xs
        Index i
            | i < 0 && inRange (length xs + i) -> [xs !! (length xs + i)]
            | inRange i -> [xs !! (i-1)]
            | otherwise -> error $ "index " <> show i
                <> " not in 1-based range [1, " <> show (length xs) <> "]"
            where
            inRange x = 1 <= x && x <= length xs
        Range i j -> take (max 0 (j-1)) $ drop i xs
