-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances #-}
-- | Tie together generic Solkattu and specific instruments into a single
-- 'Korvai'.
module Derive.Solkattu.Korvai where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time.Calendar as Calendar

import qualified Util.CallStack as CallStack
import qualified Util.Map
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.Solkattu.Instrument.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.Instrument.Konnakol as Konnakol
import qualified Derive.Solkattu.Instrument.Mridangam as Mridangam
import qualified Derive.Solkattu.Instrument.Reyong as Reyong
import qualified Derive.Solkattu.Instrument.Sargam as Sargam
import qualified Derive.Solkattu.Instrument.ToScore as ToScore
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type Sequence = [Sequence.Note (Solkattu.Note Stroke)]

-- * korvai

data Korvai = Korvai {
    korvai_sequences :: ![Sequence]
    , korvai_instruments :: !Instruments
    , korvai_tala :: !Tala.Tala
    , korvai_metadata :: !Metadata
    } deriving (Eq, Show)

instance Pretty Korvai where
    format (Korvai sequence instruments tala metadata) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("instruments", Pretty.format instruments)
        , ("tala", Pretty.format tala)
        , ("metadata", Pretty.format metadata)
        ]

korvai :: Tala.Tala -> Instruments -> [Sequence] -> Korvai
korvai tala instruments sequences = infer_metadata $ Korvai
    { korvai_sequences = sequences
    , korvai_instruments = instruments
    , korvai_tala = tala
    , korvai_metadata = mempty
    }

-- | TODO the name is awkward.  This is really just ties together all the
-- instrument-specific code.
data GetInstrument stroke = GetInstrument {
    get_realization :: Instruments -> Realize.Instrument stroke
    , get_stroke :: Stroke -> Maybe (Realize.Stroke stroke)
    , get_to_score :: ToScore.ToScore stroke
    }

mridangam :: GetInstrument Mridangam.Stroke
mridangam = GetInstrument
    { get_realization = inst_mridangam
    , get_stroke = s_mridangam
    , get_to_score = ToScore.to_score
    }

kendang_tunggal :: GetInstrument KendangTunggal.Stroke
kendang_tunggal = GetInstrument
    { get_realization = inst_kendang_tunggal
    , get_stroke = s_kendang_tunggal
    , get_to_score = ToScore.to_score
    }

reyong :: GetInstrument Reyong.Stroke
reyong = GetInstrument
    { get_realization = inst_reyong
    , get_stroke = s_reyong
    , get_to_score = ToScore.to_score
    }

sargam :: GetInstrument Sargam.Stroke
sargam = GetInstrument
    { get_realization = inst_sargam
    , get_stroke = s_sargam
    , get_to_score = Sargam.to_score
    }

-- | Realize a Korvai on a particular instrument.
realize :: Pretty stroke => GetInstrument stroke -> Bool -> Korvai
    -> [Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)]
realize get realize_patterns korvai =
    map (realize1 get realize_patterns (korvai_instruments korvai)
            (korvai_tala korvai))
        (korvai_sequences korvai)

-- | Realize a Korvai on a particular instrument.
realize1 :: Pretty stroke => GetInstrument stroke -> Bool
    -> Instruments -> Tala.Tala -> Sequence
    -> Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)
realize1 get realize_patterns instruments tala sequence = do
    -- Continue to realize even if there are align errors.  Misaligned notes
    -- are easier to read if I realize them down to strokes.
    let (notes, align_error) = verify_alignment tala sequence
    realized <- realize_instrument get instruments realize_patterns notes
    return (realized, fromMaybe "" align_error)

verify_alignment :: Tala.Tala -> Sequence
    -> ([(Sequence.Tempo, Solkattu.Note Stroke)], Maybe Text)
verify_alignment tala sequence = Solkattu.verify_alignment tala $
    Solkattu.cancel_karvai $ Sequence.flatten sequence

realize_instrument :: Pretty stroke => GetInstrument stroke
    -> Instruments -> Bool -> [(Sequence.Tempo, Solkattu.Note Stroke)]
    -> Either Text [(Sequence.Tempo, Realize.Note stroke)]
realize_instrument get instruments realize_patterns notes = do
    let inst = get_realization get instruments
    notes <- return $
        map (fmap (Solkattu.modify_stroke (get_stroke get =<<))) notes
    notes <- if realize_patterns
        then Realize.realize_patterns (Realize.inst_patterns inst) notes
        else return notes
    Realize.realize (Realize.inst_stroke_map inst) notes

vary :: (Sequence -> [Sequence]) -> Korvai -> Korvai
vary modify korvai = korvai
    { korvai_sequences = concatMap modify (korvai_sequences korvai) }

-- ** konnakol

realize_konnakol :: Bool -> Korvai
    -> [Either Text ([(Sequence.Tempo, Realize.Note Solkattu.Sollu)], Text)]
realize_konnakol realize_patterns korvai =
    map realize1 (korvai_sequences korvai)
    where
    realize1 sequence = do
        let (notes, align_error) =
                verify_alignment (korvai_tala korvai) sequence
        notes <- return $
            map (fmap (Solkattu.modify_stroke (const Nothing))) notes
        notes <- if realize_patterns
            then Realize.realize_patterns Konnakol.default_patterns notes
            else return notes
        return (to_konnakol notes, fromMaybe "" align_error)

to_konnakol :: [(tempo, Solkattu.Note (Realize.Stroke Solkattu.Sollu))]
    -> [(tempo, Realize.Note Solkattu.Sollu)]
to_konnakol = mapMaybe convert
    where
    convert (tempo, note) = (tempo,) <$> case note of
        Solkattu.Note note -> Just $ Realize.Note $
            fromMaybe (Realize.stroke (Solkattu._sollu note))
                (Solkattu._stroke note)
        Solkattu.Space space -> Just (Realize.Space space)
        Solkattu.Pattern p -> Just $ Realize.Pattern p
        Solkattu.Alignment {} -> Nothing

-- * Metadata

-- | Attach some metadata to a Korvai.  Someday I'll put them in some kind of
-- searchable database and then this should be useful.
data Metadata = Metadata {
    _date :: !(Maybe Calendar.Day)
    , _tags :: !Tags
    } deriving (Eq, Show)

instance Monoid Metadata where
    mempty = Metadata Nothing mempty
    mappend (Metadata date1 tags1) (Metadata date2 tags2) =
        Metadata (date1 <|> date2) (tags1 <> tags2)

instance Pretty Metadata where
    format (Metadata date tags) = Pretty.record "Metadata"
        [ ("date", Pretty.format date)
        , ("tags", Pretty.format tags)
        ]

newtype Tags = Tags (Map Text [Text])
    deriving (Eq, Show, Pretty)

instance Monoid Tags where
    mempty = Tags mempty
    mappend (Tags t1) (Tags t2) = Tags (Util.Map.mappend t1 t2)

date :: CallStack.Stack => Int -> Int -> Int -> Calendar.Day
date y m d
    | Num.inRange 2012 2020 y && Num.inRange 1 13 m && Num.inRange 1 32 d =
        Calendar.fromGregorian (fromIntegral y) m d
    | otherwise = errorStack $ "invalid date: " <> showt (y, m, d)

-- ** infer

infer_metadata :: Korvai -> Korvai
infer_metadata korvai =
    with_metadata (mempty { _tags = infer_tags korvai }) korvai

infer_tags :: Korvai -> Tags
infer_tags korvai = Tags $ Util.Map.multimap $ concat
    [ [("tala", Tala._name tala)]
    , map (("avartanams",) . pretty . (/aksharas) . Solkattu.duration_of) seqs
    , map ("nadai",) (map showt nadais)
    , map ("speed",) (map showt speeds)
    , map ("instrument",) [name | (name, True) <- instruments]
    ]
    where
    tala = korvai_tala korvai
    aksharas = fromIntegral (sum (Tala.tala_aksharas tala))

    seqs = korvai_sequences korvai
    notes = map (Solkattu.cancel_karvai . Sequence.flatten) seqs
    nadais = Seq.unique_sort $ concatMap (map (Sequence.nadai . fst)) notes
    speeds = Seq.unique_sort $ concatMap (map (Sequence.speed . fst)) notes

    instruments =
        [ ("mridangam", has_instrument inst_mridangam)
        , ("kendang_tunggal", has_instrument inst_kendang_tunggal)
        , ("reyong", has_instrument inst_reyong)
        , ("sargam", has_instrument inst_sargam)
        ]
    has_instrument get = get (korvai_instruments korvai) /= mempty

with_metadata :: Metadata -> Korvai -> Korvai
with_metadata meta korvai =
    korvai { korvai_metadata = meta <> korvai_metadata korvai }

-- * types

data Instruments = Instruments {
    inst_mridangam :: Realize.Instrument Mridangam.Stroke
    , inst_kendang_tunggal :: Realize.Instrument KendangTunggal.Stroke
    , inst_reyong :: Realize.Instrument Reyong.Stroke
    , inst_sargam :: Realize.Instrument Sargam.Stroke
    } deriving (Eq, Show)

instance Monoid Instruments where
    mempty = Instruments mempty mempty mempty mempty
    mappend (Instruments a1 a2 a3 a4) (Instruments b1 b2 b3 b4) =
        Instruments (a1<>b1) (a2<>b2) (a3<>b3) (a4<>b4)

instance Pretty Instruments where
    format (Instruments mridangam kendang_tunggal reyong sargam) =
        Pretty.record "Instruments"
            [ ("mridangam", Pretty.format mridangam)
            , ("kendang_tunggal", Pretty.format kendang_tunggal)
            , ("reyong", Pretty.format reyong)
            , ("sargam", Pretty.format sargam)
            ]

data Stroke = Stroke {
    s_mridangam :: !(RStroke Mridangam.Stroke)
    , s_kendang_tunggal :: !(RStroke KendangTunggal.Stroke)
    , s_reyong :: !(RStroke Reyong.Stroke)
    , s_sargam :: !(RStroke Sargam.Stroke)
    } deriving (Eq, Ord, Show)

type RStroke a = Maybe (Realize.Stroke a)

instance Monoid Stroke where
    mempty = Stroke Nothing Nothing Nothing Nothing
    mappend (Stroke a1 a2 a3 a4) (Stroke b1 b2 b3 b4) =
        Stroke (a1<|>b1) (a2<|>b2) (a3<|>b3) (a4<|>b4)

instance Pretty Stroke where
    pretty (Stroke m k r s) = pretty (m, k, r, s)

class ToStroke stroke where
    to_stroke :: CallStack.Stack => stroke -> Stroke
instance ToStroke Stroke where
    to_stroke = id
instance ToStroke (Realize.Stroke Mridangam.Stroke) where
    to_stroke s = mempty { s_mridangam = Just s }
instance ToStroke (Realize.Stroke KendangTunggal.Stroke) where
    to_stroke s = mempty { s_kendang_tunggal = Just s }

instance (Pretty stroke, ToStroke stroke) =>
        ToStroke (Sequence.Note stroke) where
    to_stroke (Sequence.Note s) = to_stroke s
    to_stroke n = errorStack $ "requires a note: " <> pretty n

instance ToStroke (Realize.Note Mridangam.Stroke) where
    to_stroke (Realize.Note s) = to_stroke s
    to_stroke n = errorStack $ "requires a note: " <> pretty n

instance ToStroke (Realize.Note KendangTunggal.Stroke) where
    to_stroke (Realize.Note s) = to_stroke s
    to_stroke n = errorStack $ "requires a note: " <> pretty n

-- This generalizes the Realize.Note instances, but would require
-- UndecidableInstances.
-- instance (Pretty stroke, ToStroke (Realize.Stroke stroke)) =>
--         ToStroke (Realize.Note stroke) where
--     to_stroke (Realize.Note s) = to_stroke s
--     to_stroke n = errorStack $ "requires a note: " <> pretty n


-- * print score

print_instrument :: Pretty stroke => GetInstrument stroke -> Bool -> Korvai
    -> IO ()
print_instrument instrument realize_patterns korvai =
    print_results Nothing korvai $ realize instrument realize_patterns korvai

print_konnakol :: Bool -> Korvai -> IO ()
print_konnakol realize_patterns korvai =
    print_results (Just 4) korvai $ realize_konnakol realize_patterns korvai

print_konnakol_html :: Bool -> Korvai -> IO ()
print_konnakol_html realize_patterns korvai =
    case sequence (realize_konnakol realize_patterns korvai) of
        Left err -> Text.IO.putStrLn $ "ERROR:\n" <> err
        Right results
            | any (not . Text.null) warnings -> mapM_ Text.IO.putStrLn warnings
            | otherwise -> Realize.write_html "konnakol.html"
                (korvai_tala korvai) notes
            where (notes, warnings) = unzip results

print_results :: Pretty stroke => Maybe Int -> Korvai
    -> [Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)] -> IO ()
print_results override_stroke_width korvai = print_list . map show1
    where
    show1 (Left err) = "ERROR:\n" <> err
    show1 (Right (notes, warning)) = TextUtil.joinWith "\n"
        (Realize.format override_stroke_width width tala notes)
        warning
    tala = korvai_tala korvai

width :: Int
width = 78

print_list :: [Text] -> IO ()
print_list [] = return ()
print_list [x] = Text.IO.putStrLn x
print_list xs = mapM_ print1 (zip [1..] xs)
    where
    print1 (i, x) = do
        putStrLn $ "---- " <> show i
        Text.IO.putStrLn x
