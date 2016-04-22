-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams #-}
-- | Notate Carnatic solkattu and realize to mridangam fingering.
module Derive.Call.India.Solkattu where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Global


type Sequence = [Note]

data Note = Sollu Sollu Karvai | Rest
    -- | Set pattern with the given duration.
    | Pattern Matras Karvai
    | Alignment Alignment
    deriving (Eq, Show)

data Alignment = Sam | Arudi
    deriving (Eq, Show)

instance Pretty.Pretty Note where
    pretty n = case n of
        Sollu s karvai -> pretty s <> k karvai
        Rest -> "__"
        Pattern d karvai -> "p" <> showt d <> k karvai
        Alignment Sam -> "at0"
        Alignment Arudi -> "atX"
        where
        k Karvai = "_"
        k NoKarvai = ""

data Karvai = Karvai | NoKarvai
    deriving (Eq, Show)

data RealizedNote = RRest !Matras | RSollu !Sollu | RPattern !Matras
    deriving (Show)

instance Pretty.Pretty RealizedNote where
    pretty n = case n of
        RRest dur -> "__" <> if dur == 1 then "" else showt dur
        RSollu s -> pretty s
        RPattern dur -> "p" <> showt dur

data Sollu = Ta | Di | Ki | Thom -- ta di ki ta thom
    | Na | Ka | Ti | Ku | Ri -- nakatikutari
    | Din | Gin -- ta din gin na tom
    | Dit | Dheem
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Sollu where
    pretty = Text.toLower . showt

type Aksharas = Int
type Matras = Int

duration :: Sequence -> Matras
duration = sum . map note_duration

note_duration :: Note -> Matras
note_duration n = case n of
    Sollu {} -> 1
    Rest -> 1
    Pattern dur _ -> dur
    Alignment {} -> 0

data Tala = Tala {
    tala_aksharas :: !Aksharas
    , tala_arudi :: !Aksharas
    , tala_nadai :: !Matras
    } deriving (Show)

instance Pretty.Pretty Tala where
    format (Tala aksharas arudi nadai) = Pretty.record "Tala"
        [ ("aksharas", Pretty.format aksharas)
        , ("arudi", Pretty.format arudi)
        , ("nadai", Pretty.format nadai)
        ]

adi_tala :: Matras -> Tala
adi_tala = Tala 8 4

-- | Realize karvai and verify alignment.
realize_tala :: Tala -> Sequence -> Either [Text] [RealizedNote]
realize_tala tala =
    fmap realized_notes . check_errors . map apply_karvai
        . verify_durations tala . split_just is_alignment Sam
    where
    is_alignment (Alignment align) = Just align
    is_alignment _ = Nothing
    apply_karvai = either Left (uncurry realize_karvai)
    -- apply_patterns = either ((:[]) . Left) (realize_patterns patterns)
    check_errors groups
        | any Either.isLeft groups = Left $ map (either id pretty) groups
        | otherwise = Right $ concatMap (either (const []) id) groups

realized_notes :: [Note] -> [RealizedNote]
realized_notes ns =
    (if not (null rests) then (RRest (length rests) :) else id) $
        case non_rests of
            [] -> []
            Sollu s _ : ns -> RSollu s : realized_notes ns
            Pattern d _ : ns -> RPattern d : realized_notes ns
            _ : ns -> realized_notes ns
    where (rests, non_rests) = span (==Rest) ns

-- | Divide available time among notes with karvai.  Error if there are no
-- karvai, or if the time doesn't divide evenly.  Otherwise, replace karvai
-- with Rests.
realize_karvai :: Matras -> [Note] -> Either Text [Note]
realize_karvai extra notes
    | extra == 0 = Right notes
    | karvais <= 0 =
        Left $ "no karvai but there's unfilled space: " <> showt extra
            <> ": " <> pretty notes
    | remainder /= 0 =
        Left $ "uneven division: " <> showt karvais <> " karvais into "
            <> showt extra <> " matras"
    | otherwise = Right $ concatMap replace notes
    where
    karvais = Seq.count has_karvai notes
    (per_karvai, remainder) = extra `divMod` karvais
    padding = replicate per_karvai Rest
    replace (Sollu x Karvai) = Sollu x NoKarvai : padding
    replace (Pattern x Karvai) = Pattern x NoKarvai : padding
    replace x = [x]

has_karvai :: Note -> Bool
has_karvai (Sollu _ Karvai) = True
has_karvai (Pattern _ Karvai) = True
has_karvai _ = False

-- | If the notes have a duration that's longer than the the time from the
-- previous alignment, then error.  Also error if it's shorter and there are no
-- karvai.
--
-- Arudi must be followed by Sam.  If Sam is followed by Sam, it can insert
-- enough integral avartanams to make the duration long enough.
verify_durations :: Tala -> [(Alignment, [Note])]
    -- ^ Notes divided into alignment groups.
    -> [Either Text (Matras, [Note])] -- ^ (extra_matras, notes)
verify_durations tala notes =
    map (verify . second (maybe Sam fst)) $ Seq.zip_next $ drop_initial notes
    where
    -- If the sequenced started with Sam, I'll get an extra here.
    drop_initial ((Sam, []) : ns@((Sam, _) : _)) = ns
    drop_initial ns = ns
    verify ((align, notes), next) = case (align, next) of
        (Arudi, Arudi) -> Left "arudi must be followed by sam"
        (Arudi, Sam) -> transition "Arudi->Sam"
            (tala_nadai tala * (tala_aksharas tala - tala_arudi tala))
        (Sam, Arudi) -> transition "Sam->Arudi"
            (tala_nadai tala * tala_arudi tala)
        (Sam, Sam) -> Right (until - dur, notes)
            where until = round_up dur (tala_aksharas tala)
        where
        dur = duration notes
        transition name until
            | dur > until =
                Left $ name <> " transition should have <= "
                    <> showt until <> " matras, but has " <> showt dur
            | otherwise = Right (until - dur, notes)


-- * realize

data Korvai = Korvai {
    korvai_sequence :: Sequence
    , korvai_mridangam :: MridangamMap
    , korvai_tala :: Tala
    } deriving (Show)

-- | [Sollu] and Strokes should be the same length.
-- TODO enforce it in constructor
type MridangamMap = Map.Map [Sollu] [Stroke]

-- | length Strokes should equal Matras.
-- TODO enforce it in constructor
type Patterns = Map.Map Matras [Maybe Stroke]

data Stroke = Thoppi !Thoppi | Valantalai Valantalai | Both !Thoppi !Valantalai
    deriving (Eq, Show)
data Thoppi = MTha | MThom
    deriving (Eq, Show)
data Valantalai = MKi | MTa | MNam | MDin | MChapu | MDheem
    deriving (Eq, Show)

instance Pretty.Pretty Stroke where
    pretty (Thoppi t) = pretty t
    pretty (Valantalai v) = pretty v
    pretty (Both t v) = pretty t <> pretty v

instance Pretty.Pretty Thoppi where
    pretty n = case n of
        MThom -> "o"
        MTha -> "p"
instance Pretty.Pretty Valantalai where
    pretty n = case n of
        MKi -> "k"
        MTa -> "t"
        MNam -> "n"
        MDin -> "d"
        MChapu -> "u"
        MDheem -> "i"

instance Pretty.Pretty Korvai where
    format (Korvai sequence mridangam tala) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("mridangam", Pretty.format mridangam)
        , ("tala", Pretty.format tala)
        ]

-- | Check for errors and construct a 'Korvai'.
korvai :: Tala -> [(Sequence, [Maybe Stroke])] -> Sequence -> Either Text Korvai
korvai tala mridangam sequence = do
    let (keys, vals) = unzip mridangam
    keys <- mapM check_sollus keys
    vals <- return $ map Maybe.catMaybes vals -- TODO verify no Nothings
    let (mridangam_map, dups) = Util.Map.unique2 (zip keys vals)
    unless (null dups) $
        Left $ "duplicate mridangam keys: " <> pretty dups
    return $ Korvai
        { korvai_sequence = sequence
        , korvai_mridangam = mridangam_map <> standard_mridangam_map
        , korvai_tala = tala
        }
    where
    check_sollus = mapM $ \n -> case n of
        Sollu s _ -> Right s
        _ -> Left $
            "korvai: mridangam map should only have sollus: " <> pretty n

standard_mridangam_map :: MridangamMap
standard_mridangam_map = Map.fromList
    [ ([Thom], [Thoppi MThom])
    ]

realize_korvai :: Patterns -> Korvai -> Either [Text] [Maybe Stroke]
realize_korvai patterns korvai = do
    rnotes <- realize_tala (korvai_tala korvai) (korvai_sequence korvai)
    first (:[]) $ realize_mridangam patterns (korvai_mridangam korvai) rnotes

realize_mridangam :: Patterns -> MridangamMap -> [RealizedNote]
    -> Either Text [Maybe Stroke]
realize_mridangam patterns mmap = go
    where
    go [] = Right []
    go (n : ns) = case n of
        RPattern dur -> case Map.lookup dur patterns of
            Nothing -> Left $ "no pattern with duration " <> showt dur
            Just mseq -> (mseq<>) <$> go ns
        RRest dur -> (replicate dur Nothing <>) <$> go ns
        RSollu sollu -> do
            (strokes, rest) <- add_context (n:ns) $
                find_mridangam_sequence mmap sollu ns
            (strokes<>) <$> go rest
    add_context notes = first (("at " <> pretty (take 16 notes) <> ": ") <>)

-- | Find the longest matching sequence until the sollus are consumed or
-- a sequence isn't found.
find_mridangam_sequence :: MridangamMap -> Sollu -> [RealizedNote]
    -> Either Text ([Maybe Stroke], [RealizedNote])
find_mridangam_sequence mmap sollu notes =
    case longest_match (sollu : sollus) of
        Nothing -> Left $ "sequence not found: " <> pretty (sollu : sollus)
        Just strokes -> Right $ insert_rests strokes (RSollu sollu : notes)
    where
    -- Collect only sollus and rests, and strip the rests.
    sollus = fst $ first Maybe.catMaybes $ Seq.span_while is_sollu notes
    is_sollu (RSollu s) = Just (Just s)
    is_sollu (RRest {}) = Just Nothing
    is_sollu _ = Nothing
    longest_match = Seq.head . mapMaybe (flip Map.lookup mmap) . reverse
        . drop 1 . List.inits

-- | Match each stroke to its RealizedNote, and insert rests where the
-- RealizedNotes has them.
insert_rests :: [Stroke] -> [RealizedNote] -> ([Maybe Stroke], [RealizedNote])
insert_rests [] ns = ([], ns)
insert_rests (stroke : strokes) (n : ns) = case n of
    RRest dur ->
        first (replicate dur Nothing ++) $ insert_rests (stroke : strokes) ns
    -- This shouldn't happen because the strokes are from the result of
    -- span_sollus.
    RPattern {} -> insert_rests (stroke : strokes) ns
    RSollu {} -> first (Just stroke :) $ insert_rests strokes ns
insert_rests (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the MridangamMap should be
    -- the same length as the RealizedNotes used to find them.

show_strokes :: [Maybe Stroke] -> Text
show_strokes = Text.unwords . map stroke
    where
    stroke Nothing = "-"
    stroke (Just s) = pretty s

-- * transform

-- | Drop a number of matras from the Sequence.  Patterns will be shortened.
dropM :: Matras -> Sequence -> Sequence
dropM matras ns = case ns of
    [] -> []
    (n:ns)
        | matras <= 0 -> (n:ns)
        | otherwise -> case n of
            Sollu {} -> dropM (matras-1) ns
            Rest {} -> dropM (matras-1) ns
            Pattern dur karvai
                | dur > matras -> Pattern (dur - matras) karvai : ns
                | otherwise -> dropM (matras - dur) ns
            Alignment {} -> dropM matras ns


-- * misc

check :: Log.Stack => Either Text a -> a
check = either error_stack id

error_stack :: Log.Stack => Text -> a
error_stack msg = error $ untxt $ Log.show_stack ?stack <> ": " <> msg

-- * util

splits :: [a] -> [([a], [a])]
splits xs = drop 1 $ zip (List.inits xs) (List.tails xs)

-- | Round the first argument up to the next multiple of the second.
round_up :: Integral a => a -> a -> a
round_up a b = b * ceiling (fromIntegral a / fromIntegral b)

-- | Split when the function returns Just, and pair that value with the
-- subsequent elements.
split_just :: (a -> Maybe b) -> b -> [a] -> [(b, [a])]
split_just f initial xs = go [] initial (zip (map f xs) xs)
    where
    go accum key ((mb, a) : rest) = case mb of
        Nothing -> go (a : accum) key rest
        Just b -> (key, reverse accum) : go [a] b rest
    go accum key [] = [(key, reverse accum)]

group_rights :: [Either a b] -> [Either a [b]]
group_rights xs = case rest of
    [] -> cons []
    Left x : xs -> cons $ Left x : group_rights xs
    Right x : xs -> Right [x] : group_rights xs
    where
    (rights, rest) = Seq.span_while (either (const Nothing) Just) xs
    cons = if null rights then id else (Right rights :)
