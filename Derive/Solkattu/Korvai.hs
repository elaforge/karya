-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Tie together generic Solkattu and specific realizations into a single
-- 'Korvai'.
module Derive.Solkattu.Korvai where
import qualified Data.Text as Text

import qualified Util.Pretty as Pretty
import qualified Derive.Solkattu.Mridangam as Mridangam
import qualified Derive.Solkattu.Realize as Realize
import qualified Derive.Solkattu.Solkattu as Solkattu

import Global


type Sequence = Solkattu.Sequence Mridangam.Stroke

data Korvai = Korvai {
    korvai_sequence :: Sequence
    , korvai_mridangam :: Realize.Instrument Mridangam.Stroke
    , korvai_tala :: Solkattu.Tala
    } deriving (Show)

instance Pretty.Pretty Korvai where
    format (Korvai sequence mridangam tala) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("mridangam", Pretty.format mridangam)
        , ("tala", Pretty.format tala)
        ]

korvai :: Solkattu.Tala -> Realize.Instrument Mridangam.Stroke
    -> Solkattu.Sequence Mridangam.Stroke -> Korvai
korvai tala mridangam sequence = Korvai
    { korvai_sequence = sequence
    , korvai_mridangam = mridangam
    , korvai_tala = tala
    }

-- | Realize a Korvai in mridangam strokes.
realize :: Bool -> Korvai -> Either Text [Realize.Note Mridangam.Stroke]
realize realize_patterns korvai = first Text.unlines $ do
    rnotes <- Solkattu.verify_alignment (korvai_tala korvai)
        (korvai_sequence korvai)
    Realize.realize realize_patterns (korvai_mridangam korvai) rnotes

vary :: (Sequence -> [Sequence]) -> Korvai -> [Korvai]
vary modify korvai =
    [korvai { korvai_sequence = new } | new <- modify (korvai_sequence korvai)]
