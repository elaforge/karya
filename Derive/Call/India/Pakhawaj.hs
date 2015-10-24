-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Functions to realize pakhawaj bols.
module Derive.Call.India.Pakhawaj where
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Derive.Score as Score
import Global
import Types


-- * calls

realize :: ScoreTime -> [(ScoreTime, Bol)] -> [(ScoreTime, Score.Attributes)]
realize flam notes =
    concat $ zipWith (bol_to_attribute flam) ts (infer_tette bols)
    where (ts, bols) = unzip notes

realize_bols :: ScoreTime -- ^ Normally 'Notes' are divided evenly in the
    -- time given, but that doesn't work for the final note, which gets this
    -- duration.
    -> [(ScoreTime, Text)] -> Either Text [(ScoreTime, Bol)]
realize_bols default_dur = fmap realize_notes . match_syllables
    where
    realize_notes = concatMap realize_note . Seq.zip_next
    realize_note ((t, note), next) = case note of
        Rest -> []
        Note bol -> [(t, bol)]
        Notes notes -> realize_notes $ zip ts notes
            where ts = Seq.range_ t (dur / fromIntegral (length notes))
        where dur = maybe default_dur (subtract t . fst) next

bol_to_attribute :: ScoreTime -> ScoreTime -> Bol
    -> [(ScoreTime, Score.Attributes)]
bol_to_attribute flam t bol = case bol of
    One s -> [(t, stroke_to_attribute s)]
    Together s1 s2 -> [(t, stroke_to_attribute s1), (t, stroke_to_attribute s2)]
    Flam s1 s2 ->
        [(t, stroke_to_attribute s1), (t + flam, stroke_to_attribute s2)]

stroke_to_attribute :: Stroke -> Score.Attributes
stroke_to_attribute s = case s of
    -- This should have already been eliminated by 'infer_tette'.
    Tette -> Score.attr "tet"
    _ -> Score.attr (Text.toLower (showt s))

-- * implementation

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

data Note a = Rest | Note a | Notes [Note a]
    deriving (Show, Eq, Functor, Foldable.Foldable, Traversable.Traversable)

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

instance Pretty.Pretty a => Pretty.Pretty (Note a) where
    pretty Rest = "-"
    pretty (Note bol) = pretty bol
    pretty (Notes note) = "(" <> pretty note <> ")"

-- | Textual representation of a bol.
type Syllable = Text

all_bols :: [([Syllable], [Note Bol])]
all_bols =
    [([name], [Note bol]) | (names, bol) <- single_bols, name <- names]
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

sequences :: [([Syllable], [Note Bol])]
sequences =
    [ (["kre"], note $ Flam Ka Tet)
    , (["gre"], note $ Flam Ge Tet)
    , (["te", "re", "ki", "ta"], notes [Tet, Te, Ka, Tet])
    , (["tr", "kt"], notes2 [[Tet, Te], [Ka, Tet]])
    , (["te", "re", "ki", "ta", "ta", "ka"], notes [Tet, Te, Ka, Tet, Te, Ka])
    , (["tr", "kt", "tk"], notes2 [[Tet, Te], [Ka, Tet], [Te, Ka]])
    , (["ki", "ta", "ta", "ka"], notes [Tet, Te, Ka, Tet])
    , (["kt", "tk"], notes2 [[Tet, Te], [Ka, Tet]])
    , (["ta", "ki"], notes [Tet, Ka])
    , (["te", "ran"], notes [Di3, Di1])
    , (["dhu", "ma"], map Note [Together Ge Di, One Te])
    -- Abbreviations.
    , (["tetekata"], notes [Tet, Te, Ka, Ta, Ge, Di, Ge, Ne])
    ]
    where
    note = (:[]) . Note
    notes = map (Note . One)
    notes2 = map (Notes . notes)

-- | Parse scores from "Derive.Call.India.PakhawajScore".
parse :: ScoreTime -> Text -> Either Text [(ScoreTime, Bol)]
parse dur = fmap infer . realize_bols dur . zip (Seq.range_ 0 dur)
    . Text.words . Text.replace "|" " " . Text.toLower
    where
    infer notes = zip ts (infer_tette bols)
        where (ts, bols) = unzip notes

match_syllables :: [(a, Syllable)] -> Either Text [(a, Note Bol)]
match_syllables = go
    where
    go [] = Right []
    go syllables@((annot, w) : ws)
        | w == "-" = ((annot, Rest) :) <$> go ws
        | Just (rest, bols) <- best_match syllables = (bols++) <$> go rest
        | otherwise = Left $ "unknown bol: " <> showt w
    best_match syllables = fmap snd $
        Seq.maximum_on fst $ mapMaybe (match_bols syllables) all_bols
    match_bols :: [(a, Syllable)] -> ([Syllable], [Note Bol])
        -> Maybe (Int, ([(a, Syllable)], [(a, Note Bol)]))
    match_bols syllables (bol_syllables, bols)
        | pre == bol_syllables =
            Just (length bol_syllables, (post, zip annots bols))
        | otherwise = Nothing
        where
        ((annots, pre), post) = first unzip $
            splitAt (length bol_syllables) syllables

-- | Replace 'Tette' with either Tet or Te, based on its neighbors.
infer_tette :: [Bol] -> [Bol]
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

-- | This is different from @map f . Seq.zip_neighbors@ in that you can see
-- whatever change @f@ made to the previous value.
map_neighbors :: Traversable.Traversable t => (Maybe b -> a -> Maybe a -> b)
    -> t a -> t b
map_neighbors f xs =
    snd $ Traversable.mapAccumL go (Nothing, drop 1 (Foldable.toList xs)) xs
    where
    go (prev, nexts) x = ((Just y, drop 1 nexts), y)
        where y = f prev x (Seq.head nexts)
