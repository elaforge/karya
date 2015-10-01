-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Functions to realize pakhawaj bols.
module Derive.Call.India.Pakhawaj where
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Global


data Stroke =
    Tet | Te | Tette -- ^ either tet or te, whichever is more convenient
    | Ne -- ^ tet with two fingers
    | Na | Ta | Di
    | Di1 -- ^ di with one finger
    | Di3 -- ^ di with three fingers
    -- bayan
    | Ka | Ge
    deriving (Show, Eq)

instance Pretty.Pretty Stroke where pretty = Text.toLower . showt

data Bol = One Stroke | Together Stroke Stroke | Flam Stroke Stroke
    deriving (Show, Eq)

-- | This is polymorphic just so I can get Traversable.
data Sequence note = Rest | Note note | Speed Speed [Sequence note]
    deriving (Show, Eq, Functor, Foldable, Traversable)

data Speed = S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8
    deriving (Show, Eq)

speed :: Speed -> [Bol] -> Sequence Bol
speed s = Speed s . map Note

map_stroke :: (Stroke -> Stroke) -> Bol -> Bol
map_stroke f bol = case bol of
    One b -> One (f b)
    Together b1 b2 -> Together (f b1) (f b2)
    Flam b1 b2 -> Flam (f b1) (f b2)

strokes_of :: Bol -> [Stroke]
strokes_of bol = case bol of
    One b -> [b]
    Together b1 b2 -> [b1, b2]
    Flam b1 b2 -> [b1, b2]

instance Pretty.Pretty Bol where
    pretty bol = case bol of
        One b -> pretty b
        Together b1 b2 -> pretty b1 <> "+" <> pretty b2
        Flam b1 b2 -> pretty b1 <> "/" <> pretty b2

instance Pretty.Pretty note => Pretty.Pretty (Sequence note) where
    pretty Rest = "-"
    pretty (Note bol) = pretty bol
    pretty (Speed speed sequence) =
        "(" <> showt speed <> " " <> pretty sequence <> ")"

-- | Textual representation of a bol.
type Syllable = Text

all_bols :: [([Syllable], Sequence Bol)]
all_bols =
    [([name], speed S1 [bol]) | (names, bol) <- single_bols, name <- names]
    ++ sequences

-- Single strokes.
single_bols :: [([Syllable], Bol)]
single_bols =
    [ (["tet"], One Tet)
    , (["te"], One Te)
    , (["ne", "re"], One Ne)
    , (["na"], One Na)
    , (["ta"], One Ta)
    , (["di", "din"], One Di)
    -- bayan
    , (["ka", "kat", "ki"], One Ka)
    , (["ge", "gen", "ga"], One Ge)
    -- both
    , (["dha"], Together Ge Ta)
    , (["dhin"], Together Ge Di)
    , (["dhet"], Together Ge Tette)
    ]

sequences :: [([Syllable], Sequence Bol)]
sequences =
    [ (["kre"], speed S1 [Flam Ka Tet])
    , (["gre"], speed S1 [Flam Ge Tet])
    , (["te", "re", "ki", "ta"], speed1 [Tet, Te, Ka, Tet])
    , (["tr", "kt"], speed2 [Tet, Te, Ka, Tet])
    , (["te", "re", "ki", "ta", "ta", "ka"], speed1 [Tet, Te, Ka, Tet, Te, Ka])
    , (["tr", "kt", "tk"], speed2 [Tet, Te, Ka, Tet, Te, Ka])
    , (["ki", "ta", "ta", "ka"], speed1 [Tet, Te, Ka, Tet])
    , (["kt", "tk"], speed2 [Tet, Te, Ka, Tet])
    , (["ta", "ki"], speed1 [Tet, Ka])
    , (["te", "ran"], speed1 [Di3, Di1])
    , (["dhu", "ma"], speed S1 [Together Ge Di, One Te])
    -- Abbreviations.
    , (["tetekata"], speed1 [Tet, Te, Ka, Ta, Ge, Di, Ge, Ne])
    ]
    where
    speed1 = speed S1 . map One
    speed2 = speed S2 . map One

-- | Parse scores from "Derive.Call.India.PakhawajScore".
parse :: Text -> Either Text (Sequence Bol)
parse = fmap infer_tette . match_syllables . Text.words
    . Text.replace "|" " " . Text.toLower

match_syllables :: [Syllable] -> Either Text (Sequence Bol)
match_syllables = fmap (simplify . Speed S1) . go
    where
    go [] = Right []
    go syllables@(w:ws)
        | w == "-" = (Rest:) <$> go ws
        | Just (_, (rest, bols)) <- best_match syllables = (bols:) <$> go rest
        | otherwise = Left $ "unknown bol: " <> showt w
    best_match syllables =
        Seq.maximum_on fst $ mapMaybe (match_bols syllables) all_bols
    match_bols syllables (bol_syllables, bols)
        | pre == bol_syllables = Just (length bol_syllables, (post, bols))
        | otherwise = Nothing
        where (pre, post) = splitAt (length bol_syllables) syllables

-- | Combine sequences with the same speed.  This is because 'all_bols'
-- includes a speed for every sequence.
simplify :: Sequence bol -> Sequence bol
simplify = Speed S1 . go S1
    where
    go current_speed seq = case seq of
        Speed speed seqs
            | speed == current_speed -> concatMap (go current_speed) seqs
            | otherwise -> [Speed speed (concatMap (go speed) seqs)]
        _ -> [seq]

-- | Replace 'Tette' with either Tet or Te, based on its neighbors.
infer_tette :: Sequence Bol -> Sequence Bol
infer_tette = map_neighbors infer
    where
    infer prev bol next
        | Tette `elem` strokes_of bol = map_stroke (replace replacement) bol
        | otherwise = bol
        where
        replacement = case (prev, next) of
            (Just p, _) | Tet `elem` strokes_of p -> Te
            (_, Just n) | Tet  `elem` strokes_of n -> Te
            _ -> Tet
    replace stroke Tette = stroke
    replace _ stroke = stroke

-- | This is different from @map f . Seq.zip_prev@ in that you can see whatever
-- change @f@ made to the previous value.
-- map_prev :: (Maybe b -> a -> b) -> [a] -> [b]

map_neighbors :: Traversable t => (Maybe b -> a -> Maybe a -> b) -> t a -> t b
map_neighbors f xs =
    snd $ List.mapAccumL go (Nothing, drop 1 $ Foldable.toList xs) xs
    where
    go (prev, nexts) x = ((Just y, drop 1 nexts), y)
        where y = f prev x (Seq.head nexts)

map_prev :: (Maybe b -> a -> b) -> [a] -> [b]
map_prev f = snd . List.mapAccumL go Nothing
    where go prev x = let y = f prev x in (Just y, y)
