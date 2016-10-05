-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tie together generic Solkattu and specific realizations into a single
-- 'Korvai'.
module Derive.Solkattu.Korvai where
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.KendangBaliTunggal as KendangBaliTunggal
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as Solkattu

import Global


type Sequence = Solkattu.Sequence Mridangam.Stroke

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

korvai :: Solkattu.Tala -> Realizations
    -> Solkattu.Sequence Mridangam.Stroke -> Korvai
korvai tala realizations sequence = Korvai
    { korvai_sequence = sequence
    , korvai_realizations = realizations
    , korvai_tala = tala
    }

data Realizations = Realizations {
    mridangam :: Realize.Instrument Mridangam.Stroke
    , kendang_bali_tunggal :: Realize.Instrument KendangBaliTunggal.Stroke
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
    -- TODO
    Realize.realize realize_patterns (mridangam (korvai_realizations korvai))
        rnotes

vary :: (Sequence -> [Sequence]) -> Korvai -> [Korvai]
vary modify korvai =
    [korvai { korvai_sequence = new } | new <- modify (korvai_sequence korvai)]
