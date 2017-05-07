-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tie together generic Solkattu and specific instruments into a single
-- 'Korvai'.
module Derive.Solkattu.Korvai where
import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.KendangTunggal as KendangTunggal
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
    } deriving (Eq, Show)

instance Pretty.Pretty Korvai where
    format (Korvai sequence instruments tala) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("instruments", Pretty.format instruments)
        , ("tala", Pretty.format tala)
        ]

korvai :: Tala.Tala -> Instruments -> Sequence -> Korvai
korvai tala instruments sequence = Korvai
    { korvai_sequence = sequence
    , korvai_instruments = instruments
    , korvai_tala = tala
    }

data GetInstrument stroke = GetInstrument {
    get_realization :: Instruments -> Realize.Instrument stroke
    , get_stroke :: Stroke -> Maybe stroke
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
realize :: Pretty.Pretty stroke => GetInstrument stroke -> Bool -> Korvai
    -> Either Text ([(Sequence.Tempo, Realize.Note stroke)], Text)
realize get realize_patterns korvai = do
    -- Continue to realize even if there are align errors.  Misaligned notes
    -- are easier to read if I realize them down to strokes.
    let notes = Solkattu.cancel_karvai $ Sequence.flatten $
            korvai_sequence korvai
    (notes, align_error) <- return $
        Solkattu.verify_alignment (korvai_tala korvai) notes
    let inst = get_realization get (korvai_instruments korvai)
    notes <- return $ map (Solkattu.map_stroke (get_stroke get =<<)) notes
    notes <- if realize_patterns
        then Realize.realize_patterns (Realize.inst_patterns inst) notes
        else return notes
    realized <- Realize.realize (Realize.inst_stroke_map inst) notes
    return (realized, fromMaybe "" align_error)

vary :: (Sequence -> [Sequence]) -> Korvai -> [Korvai]
vary modify korvai =
    [korvai { korvai_sequence = new } | new <- modify (korvai_sequence korvai)]

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

instance Pretty.Pretty Instruments where
    format (Instruments mridangam kendang_tunggal reyong) =
        Pretty.record "Instruments"
            [ ("mridangam", Pretty.format mridangam)
            , ("kendang_tunggal", Pretty.format kendang_tunggal)
            , ("reyong", Pretty.format reyong)
            ]

data Stroke = Stroke {
    s_mridangam :: !(Maybe Mridangam.Stroke)
    , s_kendang_tunggal :: !(Maybe KendangTunggal.Stroke)
    , s_reyong :: !(Maybe Reyong.Stroke)
    } deriving (Eq, Ord, Show)

instance Monoid Stroke where
    mempty = Stroke Nothing Nothing Nothing
    mappend (Stroke a1 a2 a3) (Stroke b1 b2 b3) =
        Stroke (a1<|>b1) (a2<|>b2) (a3<|>b3)

instance Pretty.Pretty Stroke where
    pretty (Stroke m k r) = pretty (m, k, r)

class ToStroke stroke where
    to_stroke :: stroke -> Stroke
instance ToStroke Stroke where
    to_stroke = id
instance ToStroke Mridangam.Stroke where
    to_stroke s = mempty { s_mridangam = Just s }
instance ToStroke KendangTunggal.Stroke where
    to_stroke s = mempty { s_kendang_tunggal = Just s }

instance (Pretty.Pretty stroke, ToStroke stroke) =>
        ToStroke (Sequence.Note stroke) where
    to_stroke (Sequence.Note s) = to_stroke s
    to_stroke n = errorStack $ "requires a note: " <> pretty n

instance (Pretty.Pretty stroke, ToStroke stroke) =>
        ToStroke (Realize.Note stroke) where
    to_stroke (Realize.Note s) = to_stroke s
    to_stroke n = errorStack $ "requires a note: " <> pretty n
