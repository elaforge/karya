-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleInstances #-}
-- | Tie together generic Solkattu and specific instruments into a single
-- 'Korvai'.
module Derive.Solkattu.Korvai where
import qualified Data.Either as Either
import qualified Data.Map as Map

import qualified Util.CallStack as CallStack
import qualified Util.Map
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Solkattu.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.Konnakol as Konnakol
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Reyong as Reyong
import qualified Derive.Solkattu.Sequence as Sequence
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type Sequence = [Sequence.Note (Solkattu.Note Stroke)]

-- * korvai

data Korvai = Korvai {
    korvai_sequence :: !Sequence
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

korvai :: Tala.Tala -> Instruments -> Sequence -> Korvai
korvai tala instruments sequence = infer_metadata $ korvai_t $ Korvai
    { korvai_sequence = sequence
    , korvai_instruments = instruments
    , korvai_tala = tala
    , korvai_metadata = mempty
    }

data GetInstrument stroke = GetInstrument {
    get_realization :: Instruments -> Realize.Instrument stroke
    , get_stroke :: Stroke -> Maybe (Realize.Stroke stroke)
    }

mridangam :: GetInstrument Mridangam.Stroke
mridangam = GetInstrument
    { get_realization = inst_mridangam
    , get_stroke = s_mridangam
    }

kendang_tunggal :: GetInstrument KendangTunggal.Stroke
kendang_tunggal = GetInstrument
    { get_realization = inst_kendang_tunggal
    , get_stroke = s_kendang_tunggal
    }

reyong :: GetInstrument Reyong.Stroke
reyong = GetInstrument
    { get_realization = inst_reyong
    , get_stroke = s_reyong
    }

-- | Realize a Korvai on a particular instrument.
realize :: Pretty stroke => GetInstrument stroke -> Bool -> Korvai
    -> Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)
realize get realize_patterns korvai = do
    -- Continue to realize even if there are align errors.  Misaligned notes
    -- are easier to read if I realize them down to strokes.
    let (notes, align_error) = verify_alignment korvai
    realized <- realize_instrument get (korvai_instruments korvai)
        realize_patterns notes
    return (realized, fromMaybe "" align_error)

verify_alignment :: Korvai
    -> ([(Sequence.Tempo, Solkattu.Note Stroke)], Maybe Text)
verify_alignment korvai = Solkattu.verify_alignment (korvai_tala korvai) $
    Solkattu.cancel_karvai $ Sequence.flatten $ korvai_sequence korvai

realize_instrument :: Pretty stroke => GetInstrument stroke
    -> Instruments -> Bool -> [(Sequence.Tempo, Solkattu.Note Stroke)]
    -> Either Text [(Sequence.Tempo, Realize.Note stroke)]
realize_instrument get instruments realize_patterns notes = do
    let inst = get_realization get instruments
    notes <- return $ map (Solkattu.map_stroke (get_stroke get =<<)) notes
    notes <- if realize_patterns
        then Realize.realize_patterns (Realize.inst_patterns inst) notes
        else return notes
    Realize.realize (Realize.inst_stroke_map inst) notes

vary :: (Sequence -> [Sequence]) -> Korvai -> [Korvai]
vary modify korvai =
    [korvai { korvai_sequence = new } | new <- modify (korvai_sequence korvai)]

-- ** konnakol

realize_konnakol :: Bool -> Korvai
    -> Either Text ([(Sequence.Tempo, Realize.Note Solkattu.Sollu)], Text)
realize_konnakol realize_patterns korvai = do
    let (notes, align_error) = verify_alignment korvai
    notes <- return $ map (Solkattu.map_stroke (const Nothing)) notes
    notes <- if realize_patterns
        then Realize.realize_patterns Konnakol.default_patterns notes
        else return notes
    return (to_konnakol notes, fromMaybe "" align_error)

to_konnakol :: [(tempo, Solkattu.Note (Realize.Stroke Solkattu.Sollu))]
    -> [(tempo, Realize.Note Solkattu.Sollu)]
to_konnakol = mapMaybe convert
    where
    convert (tempo, note) = (tempo,) <$> case note of
        Solkattu.Note sollu _ maybe_stroke ->
            Just $ Realize.Note (fromMaybe (Realize.stroke sollu) maybe_stroke)
        Solkattu.Rest -> Just Realize.Rest
        Solkattu.Pattern p -> Just $ Realize.Pattern p
        Solkattu.Alignment {} -> Nothing

-- ** metadata

-- | Attach some metadata to a Korvai.  Someday I'll put them in some kind of
-- searchable database and then this should be useful.
data Metadata = Metadata {
    _date :: !(Maybe Date)
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

-- | Year, month, day.
data Date = Date !Int !Int !Int
    deriving (Eq, Show)

instance Pretty Date where
    pretty (Date y m d) = showt y <> "-" <> showt m <> "-" <> showt d

make_date :: CallStack.Stack => Int -> Int -> Int -> Date
make_date y m d
    | Num.inRange 2012 2020 y && Num.inRange 1 13 m && Num.inRange 1 32 d =
        Date y m d
    | otherwise = errorStack $ "invalid date: " <> showt (y, m, d)

date :: CallStack.Stack => Int -> Int -> Int -> Korvai -> Korvai
date y m d = with_metadata $ mempty { _date = Just date }
    where !date = make_date y m d

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

-- | A development sequence leading to a korvai.
sequence_t :: Korvai -> Korvai
sequence_t = with_type "sequence"

faran :: Korvai -> Korvai
faran = with_type "faran"

exercise :: Korvai -> Korvai
exercise = with_type "exercise"

with_type :: Text -> Korvai -> Korvai
with_type = with_tag "type"

with_tag :: Text -> Text -> Korvai -> Korvai
with_tag k v = with_metadata $ mempty { _tags = Tags (Map.singleton k [v]) }

with_metadata :: Metadata -> Korvai -> Korvai
with_metadata meta korvai =
    korvai { korvai_metadata = meta <> korvai_metadata korvai }

-- ** infer

infer_metadata :: Korvai -> Korvai
infer_metadata korvai =
    with_metadata (mempty { _tags = infer_tags korvai }) korvai

infer_tags :: Korvai -> Tags
infer_tags korvai = Tags $ Util.Map.multimap $ concat
    [ [ ("tala", Tala._name tala)
      , ("avartanams", pretty $ Solkattu.duration_of seq / aksharas)
      ]
    , map ("nadai",) (map showt nadais)
    , map ("speed",) (map showt speeds)
    , map ("instrument",) [name | (name, True) <- instruments]
    ]
    where
    tala = korvai_tala korvai
    aksharas = fromIntegral (sum (Tala.tala_aksharas tala))

    seq = korvai_sequence korvai
    notes = Solkattu.cancel_karvai $ Sequence.flatten seq
    nadais = Seq.unique_sort $ map (Sequence.nadai . fst) notes
    speeds = Seq.unique_sort $ map (Sequence.speed . fst) notes

    instruments =
        [ ("mridangam", has_realization mridangam)
        , ("kendang_tunggal", has_realization kendang_tunggal)
        , ("reyong", has_realization reyong)
        ]
    has_realization get = Either.isRight $
        realize_instrument get (korvai_instruments korvai) False notes

-- * types

data Instruments = Instruments {
    inst_mridangam :: Realize.Instrument Mridangam.Stroke
    , inst_kendang_tunggal :: Realize.Instrument KendangTunggal.Stroke
    , inst_reyong :: Realize.Instrument Reyong.Stroke
    } deriving (Eq, Show)

instance Monoid Instruments where
    mempty = Instruments mempty mempty mempty
    mappend (Instruments a1 a2 a3) (Instruments b1 b2 b3) =
        Instruments (a1<>b1) (a2<>b2) (a3<>b3)

instance Pretty Instruments where
    format (Instruments mridangam kendang_tunggal reyong) =
        Pretty.record "Instruments"
            [ ("mridangam", Pretty.format mridangam)
            , ("kendang_tunggal", Pretty.format kendang_tunggal)
            , ("reyong", Pretty.format reyong)
            ]

data Stroke = Stroke {
    s_mridangam :: !(Maybe (Realize.Stroke Mridangam.Stroke))
    , s_kendang_tunggal :: !(Maybe (Realize.Stroke KendangTunggal.Stroke))
    , s_reyong :: !(Maybe (Realize.Stroke Reyong.Stroke))
    } deriving (Eq, Ord, Show)

instance Monoid Stroke where
    mempty = Stroke Nothing Nothing Nothing
    mappend (Stroke a1 a2 a3) (Stroke b1 b2 b3) =
        Stroke (a1<|>b1) (a2<|>b2) (a3<|>b3)

instance Pretty Stroke where
    pretty (Stroke m k r) = pretty (m, k, r)

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
