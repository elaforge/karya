-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tie together generic Solkattu and specific realizations into a single
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

data Korvai = Korvai {
    korvai_sequence :: Sequence
    , korvai_realizations :: Realizations
    , korvai_tala :: Solkattu.Tala
    } deriving (Show)

instance Pretty.Pretty Korvai where
    format (Korvai sequence realizations tala) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("realizations", Pretty.format realizations)
        , ("tala", Pretty.format tala)
        ]

korvai :: Solkattu.Tala -> Realizations -> Sequence -> Korvai
korvai tala realizations sequence = Korvai
    { korvai_sequence = sequence
    , korvai_realizations = realizations
    , korvai_tala = tala
    }

data Realizations = Realizations {
    mridangam :: Realize.Instrument Mridangam.Stroke
    , kendang_bali_tunggal :: Realize.Instrument KendangTunggal.Stroke
    } deriving (Show)

instance Monoid Realizations where
    mempty = Realizations mempty mempty
    mappend (Realizations a1 a2) (Realizations b1 b2) =
        Realizations (a1<>b1) (a2<>b2)

instance Pretty.Pretty Realizations where
    format (Realizations mridangam kendang_bali_tunggal) =
        Pretty.record "Realizations"
            [ ("mridangam", Pretty.format mridangam)
            , ("kendang_bali_tunggal", Pretty.format kendang_bali_tunggal)
            ]

-- | Realize a Korvai in mridangam strokes.
realize :: Bool -> Korvai -> Either Text [Realize.Note Mridangam.Stroke]
realize realize_patterns korvai = first Text.unlines $ do
    rnotes <- Solkattu.verify_alignment (korvai_tala korvai)
        (korvai_sequence korvai)
    -- TODO extend for non-mridangam realization
    Realize.realize realize_patterns
        (mridangam (korvai_realizations korvai))
        (map (Solkattu.map_stroke (s_mridangam =<<)) rnotes)

vary :: (Sequence -> [Sequence]) -> Korvai -> [Korvai]
vary modify korvai =
    [korvai { korvai_sequence = new } | new <- modify (korvai_sequence korvai)]

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
