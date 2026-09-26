-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Functions to realize pakhawaj bols.
module Derive.C.India.Pakhawaj where
import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Traversable as Traversable

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.SubT as SubT
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import           Global
import           Types


library :: Library.Library
library = Library.generators [("bols", cBols)]

-- * calls

cBols :: Derive.Generator Derive.Note
cBols = Derive.generator ("india" <> "pakhawaj") "bols"
    (Tags.inst <> Tags.subs)
    ("Interpret pakhawaj bols in sub events.  Bols: "
        <> Texts.join " / " (map (Doc.Doc . Text.unwords . fst) allBols))
    $ Sig.call (
        Sig.defaulted "flam" (0.15 :: Double)
            "Time between flam type bols like `kre`."
    ) $ \flamDur  args -> do
        args <- Derive.requireRight id $
            Sub.modifyNotes (realizeEvents (Args.end args) flamDur) args
        events <- Sub.subEvents args
        mconcatMap Sub.derive events

realizeEvents :: ScoreTime -> ScoreTime -> [SubT.EventT Text]
    -> Either Text [SubT.EventT Text]
realizeEvents end flamDur =
    fmap (map to . bolsToAttribute flamDur) . realizeBols end . map from
    where
    from e = (SubT._start e, SubT._note e)
    -- TODO this is a bit sketchy since I'm relying on the +attr lookup call.
    -- But to directly add the call I would have to be able to return
    -- a NoteDeriver, not just text.  Of course, if I wind up using a score
    -- integrate, then of course I'd need text again.
    to (t, attrs) = SubT.EventT t 0 (ShowVal.showVal attrs)

bolsToAttribute :: ScoreTime -> [(ScoreTime, Bol)]
    -> [(ScoreTime, Attrs.Attributes)]
bolsToAttribute flam notes =
    concat $ zipWith (bolToAttribute flam) ts (inferTette bols)
    where (ts, bols) = unzip notes

realizeBols :: ScoreTime -- ^ End time, which is used for the duration of the
    -- final note.  This is needed if the last bol is a sequence.
    -> [(ScoreTime, Text)] -> Either Text [(ScoreTime, Bol)]
realizeBols end = fmap realizeNotes . matchSyllables
    where
    realizeNotes = concatMap realizeNote . Lists.zipNext
    realizeNote ((t, note), next) = case note of
        Rest -> []
        Note bol -> [(t, bol)]
        Notes notes -> realizeNotes $ zip ts notes
            where ts = Lists.range_ t (dur / fromIntegral (length notes))
        where dur = maybe end fst next - t

bolToAttribute :: ScoreTime -> ScoreTime -> Bol
    -> [(ScoreTime, Attrs.Attributes)]
bolToAttribute flam t bol = case bol of
    One s -> [(t, strokeToAttribute s)]
    Together s1 s2 -> [(t, strokeToAttribute s1), (t, strokeToAttribute s2)]
    Flam s1 s2 ->
        [(t, strokeToAttribute s1), (t + flam, strokeToAttribute s2)]

strokeToAttribute :: Stroke -> Attrs.Attributes
strokeToAttribute s = case s of
    -- This should have already been eliminated by 'infer_tette'.
    Tette -> Attrs.attr "tet"
    _ -> Attrs.attr (Text.toLower (showt s))

-- * implementation

data Stroke
    = Tet | Te | Tette -- ^ either tet or te, whichever is more convenient
    | Ne -- ^ tet with two fingers
    | Na | Ta | Di
    | Di1 -- ^ di with one finger
    | Di3 -- ^ di with three fingers
    -- bayan
    | Ka | Ge
    deriving (Show, Eq)

instance Pretty Stroke where pretty = Text.toLower . showt

data Bol = One Stroke | Together Stroke Stroke | Flam Stroke Stroke
    deriving (Show, Eq)

data Note a = Rest | Note a | Notes [Note a]
    deriving (Show, Eq, Functor, Foldable, Traversable)

mapStroke :: (Stroke -> Stroke) -> Bol -> Bol
mapStroke f bol = case bol of
    One b -> One (f b)
    Together b1 b2 -> Together (f b1) (f b2)
    Flam b1 b2 -> Flam (f b1) (f b2)

strokesOf :: Bol -> [Stroke]
strokesOf bol = case bol of
    One b -> [b]
    Together b1 b2 -> [b1, b2]
    Flam b1 b2 -> [b1, b2]

instance Pretty Bol where
    pretty bol = case bol of
        One b -> pretty b
        Together b1 b2 -> pretty b1 <> "+" <> pretty b2
        Flam b1 b2 -> pretty b1 <> "/" <> pretty b2

instance Pretty a => Pretty (Note a) where
    pretty Rest = "-"
    pretty (Note bol) = pretty bol
    pretty (Notes note) = "(" <> pretty note <> ")"

-- | Textual representation of a bol.
type Syllable = Text

allBols :: [([Syllable], [Note Bol])]
allBols =
    [([name], [Note bol]) | (names, bol) <- singleBols, name <- names]
    ++ sequences

-- Single strokes.
singleBols :: [([Syllable], Bol)]
singleBols =
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

-- TODO the length of the syllables should be the same as the length of the
-- bols or the extras will be silently dropped.
sequences :: [([Syllable], [Note Bol])]
sequences =
    [ (["kre"], note $ Flam Ka Tet)
    , (["gre"], note $ Flam Ge Tet)
    , (["te", "re", "ki", "ta"], notes [Tet, Te, Ka, Tet])
    , (["ki", "ta", "ta", "ka"], notes [Tet, Te, Ka, Tet])
    , (["tr", "kt"], notes2 [[Tet, Te], [Ka, Tet]])
    , (["te", "re", "ki", "ta", "ta", "ka"], notes [Tet, Te, Ka, Tet, Te, Ka])
    , (["tr", "kt", "tk"], notes2 [[Tet, Te], [Ka, Tet], [Te, Ka]])
    , (["kt", "tk"], notes2 [[Tet, Te], [Ka, Tet]])
    , (["ta", "ki"], notes [Tet, Ka])
    , (["te", "ran"], notes [Di3, Di1])
    , (["dhu", "ma"], map Note [Together Ge Di, One Te])
    -- Abbreviations.
    , (["tetekata"], [Notes $ notes [Tet, Te, Ka, Ta, Ge, Di, Ge, Ne]])
    ]
    where
    note = (:[]) . Note
    notes = map (Note . One)
    notes2 = map (Notes . notes)

-- | Parse scores from "Derive.Call.India.PakhawajScore".
parse :: ScoreTime -> Text -> Either Text [(ScoreTime, Bol)]
parse dur = fmap infer . realizeBols dur . zip (Lists.range_ 0 dur)
    . Text.words . Text.replace "|" " " . Text.toLower
    where
    infer notes = zip ts (inferTette bols)
        where (ts, bols) = unzip notes

matchSyllables :: [(a, Syllable)] -> Either Text [(a, Note Bol)]
matchSyllables = go
    where
    go [] = Right []
    go syllables@((annot, w) : ws)
        | w == "-" = ((annot, Rest) :) <$> go ws
        | Just (rest, bols) <- bestMatch syllables = (bols++) <$> go rest
        | otherwise = Left $ "unknown bol: " <> showt w
    bestMatch syllables = fmap snd $
        Lists.maximumOn fst $ mapMaybe (matchBols syllables) allBols
    matchBols :: [(a, Syllable)] -> ([Syllable], [Note Bol])
        -> Maybe (Int, ([(a, Syllable)], [(a, Note Bol)]))
    matchBols syllables (bolSyllables, bols)
        | pre == bolSyllables =
            Just (length bolSyllables, (post, zip annots bols))
        | otherwise = Nothing
        where
        ((annots, pre), post) = first unzip $
            splitAt (length bolSyllables) syllables

-- | Replace 'Tette' with either Tet or Te, based on its neighbors.
inferTette :: [Bol] -> [Bol]
inferTette = mapNeighbors infer
    where
    infer prev bol next
        | Tette `elem` strokesOf bol = mapStroke (replace replacement) bol
        | otherwise = bol
        where
        replacement = case (prev, next) of
            (Just p, _) | Tet `elem` strokesOf p -> Te
            (_, Just n) | Tet  `elem` strokesOf n -> Te
            _ -> Tet
    replace stroke Tette = stroke
    replace _ stroke = stroke

-- | This is different from @map f . Lists.zipNeighbors@ in that you can see
-- whatever change @f@ made to the previous value.
mapNeighbors :: Traversable t => (Maybe b -> a -> Maybe a -> b)
    -> t a -> t b
mapNeighbors f xs =
    snd $ Traversable.mapAccumL go (Nothing, drop 1 (Foldable.toList xs)) xs
    where
    go (prev, nexts) x = ((Just y, drop 1 nexts), y)
        where y = f prev x (Lists.head nexts)
