-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tie together generic Solkattu and specific instruments into a single
-- 'Korvai'.
module Derive.Solkattu.Korvai where
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.KendangTunggal as KendangTunggal
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as Solkattu

import Global


type Sequence = Solkattu.Sequence Stroke

-- * korvai

data Korvai = Korvai {
    korvai_sequence :: Sequence
    , korvai_instruments :: Instruments
    , korvai_tala :: Solkattu.Tala
    } deriving (Show)

instance Pretty.Pretty Korvai where
    format (Korvai sequence instruments tala) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("instruments", Pretty.format instruments)
        , ("tala", Pretty.format tala)
        ]

korvai :: Solkattu.Tala -> Instruments -> Sequence -> Korvai
korvai tala instruments sequence = Korvai
    { korvai_sequence = sequence
    , korvai_instruments = instruments
    , korvai_tala = tala
    }

data GetInstrument stroke = GetInstrument {
    get_realization :: Instruments -> Realize.Instrument stroke
    , get_stroke :: Stroke -> Maybe stroke
    , get_stroke_to_call :: stroke -> Text
    }

mridangam :: GetInstrument Mridangam.Stroke
mridangam = GetInstrument
    { get_realization = inst_mridangam
    , get_stroke = s_mridangam
    , get_stroke_to_call = Mridangam.stroke_to_call
    }

kendang_tunggal :: GetInstrument KendangTunggal.Stroke
kendang_tunggal = GetInstrument
    { get_realization = inst_kendang_tunggal
    , get_stroke = s_kendang_tunggal
    , get_stroke_to_call = KendangTunggal.stroke_to_call
    }

-- | Realize a Korvai on a particular instrument.
realize :: Pretty.Pretty stroke => GetInstrument stroke -> Bool -> Korvai
    -> Either Text [Realize.Note stroke]
realize get realize_patterns korvai =
    first Text.unlines $ do
        rnotes <- Solkattu.verify_alignment (korvai_tala korvai)
            (korvai_sequence korvai)
        Realize.realize realize_patterns
            (get_realization get (korvai_instruments korvai))
            (map (Solkattu.map_stroke (get_stroke get =<<)) rnotes)

vary :: (Sequence -> [Sequence]) -> Korvai -> [Korvai]
vary modify korvai =
    [korvai { korvai_sequence = new } | new <- modify (korvai_sequence korvai)]

-- * types

data Instruments = Instruments {
    inst_mridangam :: Realize.Instrument Mridangam.Stroke
    , inst_kendang_tunggal :: Realize.Instrument KendangTunggal.Stroke
    } deriving (Show)

instance Monoid Instruments where
    mempty = Instruments mempty mempty
    mappend (Instruments a1 a2) (Instruments b1 b2) =
        Instruments (a1<>b1) (a2<>b2)

instance Pretty.Pretty Instruments where
    format (Instruments mridangam kendang_tunggal) =
        Pretty.record "Instruments"
            [ ("mridangam", Pretty.format mridangam)
            , ("kendang_tunggal", Pretty.format kendang_tunggal)
            ]

data Stroke = Stroke {
    s_mridangam :: !(Maybe Mridangam.Stroke)
    , s_kendang_tunggal :: !(Maybe KendangTunggal.Stroke)
    } deriving (Show)

instance Monoid Stroke where
    mempty = Stroke Nothing Nothing
    mappend (Stroke a1 a2) (Stroke b1 b2) = Stroke (a1<|>b1) (a2<|>b2)

instance Pretty.Pretty Stroke where
    pretty (Stroke m k) = pretty (m, k)

class ToStroke stroke where
    to_stroke :: stroke -> Stroke
instance ToStroke Stroke where
    to_stroke = id
instance ToStroke Mridangam.Stroke where
    to_stroke s = mempty { s_mridangam = Just s }
instance ToStroke KendangTunggal.Stroke where
    to_stroke s = mempty { s_kendang_tunggal = Just s }

instance (Pretty.Pretty stroke, ToStroke stroke) =>
        ToStroke (Realize.Note stroke) where
    to_stroke (Realize.Note s) = to_stroke s
    to_stroke n = errorStack $ "requires a note: " <> pretty n
