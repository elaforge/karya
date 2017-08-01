-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Tie together generic Solkattu and specific instruments into a single
-- 'Korvai'.
module Derive.Solkattu.Korvai where
import qualified Data.Map as Map
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


type Sequence = SequenceT Solkattu.Sollu
type SequenceT sollu = [Sequence.Note (Solkattu.Note sollu)]

map_sollu :: (a -> b) -> SequenceT a -> SequenceT b
map_sollu f = map (fmap (fmap f))

-- * korvai

data Korvai = Korvai {
    korvai_sequences :: !KorvaiType
    , korvai_stroke_maps :: !StrokeMaps
    , korvai_tala :: !Tala.Tala
    , korvai_metadata :: !Metadata
    } deriving (Eq, Show)

data KorvaiType =
    Sollu [SequenceT Solkattu.Sollu]
    | Mridangam [SequenceT (Realize.Stroke Mridangam.Stroke)]
    deriving (Show, Eq)

sollu_sequence (Sollu seq) = Just seq
sollu_sequence _ = Nothing

instance Pretty KorvaiType where
    pretty (Sollu a) = pretty a
    pretty (Mridangam a) = pretty a

instance Pretty Korvai where
    format (Korvai sequence stroke_maps tala metadata) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("stroke_maps", Pretty.format stroke_maps)
        , ("tala", Pretty.format tala)
        , ("metadata", Pretty.format metadata)
        ]

korvai :: Tala.Tala -> StrokeMaps -> [Sequence] -> Korvai
korvai tala stroke_maps sequences = infer_metadata $ Korvai
    { korvai_sequences = Sollu sequences
    , korvai_stroke_maps = stroke_maps
    , korvai_tala = tala
    , korvai_metadata = mempty
    }

mridangam_korvai :: Tala.Tala -> Realize.Patterns Mridangam.Stroke
    -> [SequenceT (Realize.Stroke Mridangam.Stroke)]
    -> Korvai
mridangam_korvai tala pmap sequences = infer_metadata $ Korvai
    { korvai_sequences = Mridangam sequences
    , korvai_stroke_maps = mempty
        { inst_mridangam = Realize.Instrument
            { inst_stroke_map = mempty
            , inst_patterns = pmap
            }
        }
    , korvai_tala = tala
    , korvai_metadata = mempty
    }

-- | TODO the name is awkward.  This is really just ties together all the
-- instrument-specific code.
data GetInstrument sollu stroke = GetInstrument {
    -- TODO should be get_instrument, also StrokeMaps is actually just
    -- Map Inst Realize.Instrument
    get_realization :: StrokeMaps -> Realize.Instrument stroke
    , get_sequence :: KorvaiType -> Maybe [SequenceT sollu]
    , get_realize_note :: Realize.StrokeMap stroke
        -> Realize.RealizeNote Sequence.Tempo sollu stroke
    , get_to_score :: ToScore.ToScore stroke
    }

mridangam :: GetInstrument Solkattu.Sollu Mridangam.Stroke
mridangam = GetInstrument
    { get_realization = inst_mridangam
    , get_sequence = sollu_sequence
    , get_realize_note = Realize.realize_sollu
    , get_to_score = ToScore.to_score
    }

mridangam_stroke :: GetInstrument (Realize.Stroke Mridangam.Stroke)
    Mridangam.Stroke
mridangam_stroke = GetInstrument
    { get_realization = inst_mridangam
    , get_sequence = \t -> case t of
        Mridangam seq -> Just seq
        _ -> Nothing
    , get_realize_note = const Realize.realize_stroke
    , get_to_score = ToScore.to_score
    }

konnakol :: GetInstrument Solkattu.Sollu Solkattu.Sollu
konnakol = GetInstrument
    { get_realization = const $ Realize.Instrument
        { inst_stroke_map = mempty
        -- TODO to control the patterns, I could modify
        -- konnakol.get_realization
        , inst_patterns = Konnakol.default_patterns
        }
    , get_sequence = sollu_sequence
    , get_realize_note = const Realize.realize_simple_stroke
    , get_to_score = ToScore.to_score
    }

kendang_tunggal :: GetInstrument Solkattu.Sollu KendangTunggal.Stroke
kendang_tunggal = GetInstrument
    { get_realization = inst_kendang_tunggal
    , get_sequence = sollu_sequence
    , get_realize_note = Realize.realize_sollu
    , get_to_score = ToScore.to_score
    }

reyong :: GetInstrument Solkattu.Sollu Reyong.Stroke
reyong = GetInstrument
    { get_realization = inst_reyong
    , get_sequence = sollu_sequence
    , get_realize_note = Realize.realize_sollu
    , get_to_score = ToScore.to_score
    }

sargam :: GetInstrument Solkattu.Sollu Sargam.Stroke
sargam = GetInstrument
    { get_realization = inst_sargam
    , get_sequence = sollu_sequence
    , get_realize_note = Realize.realize_sollu
    , get_to_score = Sargam.to_score
    }

-- | An existential type to capture the Pretty instance.
data GInstrument = forall sollu stroke.
    Pretty stroke => GInstrument (GetInstrument sollu stroke)

instruments :: Map Text GInstrument
instruments = Map.fromList
    [ ("mridangam", GInstrument mridangam)
    -- TODO select mridangam_stroke if it's one of those
    , ("konnakol", GInstrument konnakol)
    , ("kendang_tunggal", GInstrument kendang_tunggal)
    , ("reyong", GInstrument reyong)
    , ("sargam", GInstrument sargam)
    ]

-- | Realize a Korvai on a particular instrument.
realize :: Pretty stroke => GetInstrument sollu stroke -> Bool -> Korvai
    -> [Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)]
realize get realize_patterns korvai =
    case get_sequence get (korvai_sequences korvai) of
        Nothing -> [Left "no sequence, wrong instrument type"]
        Just seqs -> map (realize1 get realize_patterns korvai) seqs

-- | Version of 'realize' for mridangam sollu or mridangam strokes.
--
-- TODO I should be able to do this with 'realize', but I can't figure out how.
-- I need say if the sequence is Mridangam, and stroke ~ Mridangam.Stroke,
-- then I can apply mridangam_stroke, or more specifically,
-- Realize.realize_stroke.  In fact, if sollu ~ Stroke stroke, then I can do
-- that.
realize_mridangam :: Bool -> Korvai
    -> [Either Text ([(Sequence.Tempo, Realize.Note Mridangam.Stroke)], Text)]
realize_mridangam realize_patterns korvai = case korvai_sequences korvai of
    Sollu seqs -> map (realize1 mridangam realize_patterns korvai) seqs
    Mridangam seqs ->
        map (realize1 mridangam_stroke realize_patterns korvai) seqs

realize1 :: Pretty stroke => GetInstrument sollu stroke -> Bool -> Korvai
    -> [Sequence.Note (Solkattu.Note sollu)]
    -> Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)
realize1 get realize_patterns korvai seq =
    realize_instrument realize_patterns
        (get_realize_note get (Realize.inst_stroke_map inst)) inst
        (korvai_tala korvai) seq
    where
    inst = get_realization get (korvai_stroke_maps korvai)

realize_instrument :: Pretty stroke =>
    Bool -> Realize.RealizeNote Sequence.Tempo sollu stroke
    -> Realize.Instrument stroke -> Tala.Tala
    -> [Sequence.Note (Solkattu.Note sollu)]
    -> Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)
realize_instrument realize_patterns realize_note inst tala sequence = do
    -- Continue to realize even if there are align errors.  Misaligned notes
    -- are easier to read if I realize them down to strokes.
    let (notes, align_error) = Solkattu.verify_alignment tala (flatten sequence)
    let pattern
            | realize_patterns =
                Realize.realize_pattern (Realize.inst_patterns inst)
            | otherwise = Realize.keep_pattern
    realized <- Realize.realize pattern realize_note notes
    return (realized, fromMaybe "" align_error)

{-
class InstrumentConfig stroke where
    get_instrument :: StrokeMaps -> Realize.Instrument stroke

class RealizeNote sollu stroke where
    realize_note :: Realize.StrokeMap stroke
        -> Realize.RealizeNote Sequence.Tempo sollu stroke

instance RealizeNote (Realize.Stroke stroke) stroke where
    realize_note _ = Realize.realize_stroke

instance RealizeNote Solkattu.Sollu Mridangam.Stroke where
    realize_note smap = Realize.realize_sollu smap

instance RealizeNote Solkattu.Sollu Solkattu.Sollu where
    realize_note _ = Realize.realize_simple_stroke

-- realize2 :: (InstrumentConfig stroke, RealizeNote 
realize2 realize_patterns korvai = case korvai_sequences korvai of
    Sollu seqs -> map (realize3 smap realize_patterns inst tala) seqs
    Mridangam seqs -> map (realize3 smap realize_patterns inst tala) seqs
    where
    -- realize1 seq = realize3 (Realize.inst_stroke_map inst) realize_patterns inst
    --     (korvai_tala korvai) seq
    inst = get_instrument (korvai_stroke_maps korvai)
    smap = Realize.inst_stroke_map inst
    tala = korvai_tala korvai

realize3 :: (RealizeNote sollu stroke, Pretty stroke) =>
    Realize.StrokeMap stroke -> Bool -> Realize.Instrument stroke
    -> Tala.Tala -> SequenceT sollu
    -> Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)
realize3 smap realize_patterns inst tala sequence = do
    let (notes, align_error) = Solkattu.verify_alignment tala (flatten sequence)
    let pattern
            | realize_patterns =
                Realize.realize_pattern (Realize.inst_patterns inst)
            | otherwise = Realize.keep_pattern
    realized <- Realize.realize pattern (realize_note smap) notes
    return (realized, fromMaybe "" align_error)
-}

flatten :: [Sequence.Note (Solkattu.Note sollu)]
    -> [(Sequence.Tempo, Solkattu.Note sollu)]
flatten = Solkattu.cancel_karvai . Sequence.flatten

-- vary :: (Sequence -> [Sequence]) -> Korvai -> Korvai
-- vary modify korvai = korvai
--     { korvai_sequences = concatMap modify (korvai_sequences korvai) }

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

    seqs = case korvai_sequences korvai of
        Sollu seqs -> map (map_sollu (const ())) seqs
        Mridangam seqs -> map (map_sollu (const ())) seqs
    notes = map (Solkattu.cancel_karvai . Sequence.flatten) seqs
    nadais = Seq.unique_sort $ concatMap (map (Sequence.nadai . fst)) notes
    speeds = Seq.unique_sort $ concatMap (map (Sequence.speed . fst)) notes

    instruments =
        [ ("mridangam", has_instrument korvai inst_mridangam)
        , ("kendang_tunggal", has_instrument korvai inst_kendang_tunggal)
        , ("reyong", has_instrument korvai inst_reyong)
        , ("sargam", has_instrument korvai inst_sargam)
        ]
    has_instrument korvai get = get (korvai_stroke_maps korvai) /= mempty

with_metadata :: Metadata -> Korvai -> Korvai
with_metadata meta korvai =
    korvai { korvai_metadata = meta <> korvai_metadata korvai }

-- * types

data StrokeMaps = StrokeMaps {
    inst_mridangam :: Realize.Instrument Mridangam.Stroke
    , inst_kendang_tunggal :: Realize.Instrument KendangTunggal.Stroke
    , inst_reyong :: Realize.Instrument Reyong.Stroke
    , inst_sargam :: Realize.Instrument Sargam.Stroke
    } deriving (Eq, Show)

instance Monoid StrokeMaps where
    mempty = StrokeMaps mempty mempty mempty mempty
    mappend (StrokeMaps a1 a2 a3 a4) (StrokeMaps b1 b2 b3 b4) =
        StrokeMaps (a1<>b1) (a2<>b2) (a3<>b3) (a4<>b4)

instance Pretty StrokeMaps where
    format (StrokeMaps mridangam kendang_tunggal reyong sargam) =
        Pretty.record "StrokeMaps"
            [ ("mridangam", Pretty.format mridangam)
            , ("kendang_tunggal", Pretty.format kendang_tunggal)
            , ("reyong", Pretty.format reyong)
            , ("sargam", Pretty.format sargam)
            ]


-- * print score

print_instrument :: Pretty stroke => GetInstrument sollu stroke
    -> Bool -> Korvai -> IO ()
print_instrument instrument realize_patterns korvai =
    print_results Nothing korvai $ realize instrument realize_patterns korvai

print_konnakol :: Bool -> Korvai -> IO ()
print_konnakol realize_patterns korvai =
    print_results (Just 4) korvai $ realize konnakol realize_patterns korvai

write_konnakol_html :: Bool -> Korvai -> IO ()
write_konnakol_html realize_patterns korvai =
    case sequence (realize konnakol realize_patterns korvai) of
        Left err -> Text.IO.putStrLn $ "ERROR:\n" <> err
        Right results
            | any (not . Text.null) warnings -> mapM_ Text.IO.putStrLn warnings
            | otherwise -> do
                putStrLn "write konnakol.html"
                Realize.write_html "konnakol.html" (korvai_tala korvai) notes
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
