-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Javanese scales.

    @

    01 02 03 05 06 11 12 13 15 16

    21 22 23 25 26 31 32 33 35 36 41 42 43 45 46 51 52 53 55 56 61 62 63 65 66
                gender barung---------------------------->
                6..1. 2. 3. 5. 6. 1  2  3  5  6  1^ 2^ 3^
                               gender panerus--------------------------->
                               6..1. 2. 3. 5. 6. 1  2  3  5  6  1^ 2^ 3^

    saron
    gambang
    slenthem

    @
-}
module Derive.Scale.Java (scales) where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch

import           Global


{-
    key=pathet, puts layout such that rare notes are black notes
    This reflects which keys are removed, and so t-dia works as expected.
-}
scales :: [Scale.Definition]
scales = map Scale.Simple
    [ BaliScales.make_scale "pelog" $
        BaliScales.scale_map config pelog_absolute Nothing
    , BaliScales.make_scale "pelog-gender-panerus" $
        BaliScales.scale_map config (cipher_relative 5) Nothing
    , BaliScales.make_scale "pelog-gender-barung" $
        BaliScales.scale_map config (cipher_relative 4) Nothing
    ]
    where
    cipher_relative octave = cipher_relative_dotted octave
        (BaliScales.config_default_key config)
        (BaliScales.config_keys config)

pelog_absolute :: TheoryFormat.Format
pelog_absolute =
    TheoryFormat.make_absolute_format_config config "[0-9][1-7]" cipher7
    where
    config = TheoryFormat.default_config
        { TheoryFormat.config_parse_octave = TheoryFormat.parse_octave1 }

cipher_relative_dotted :: Pitch.Octave -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
cipher_relative_dotted center default_key keys =
    TheoryFormat.make_relative_format "[1-7]|`[1-7][.^]*`" cipher7 fmt
    where
    fmt = BaliScales.with_config (BaliScales.dotted_octaves center) $
        ChromaticScales.relative_fmt default_key keys

cipher7 :: TheoryFormat.Degrees
cipher7 = TheoryFormat.make_degrees (map showt [1..7])

-- * config

config :: BaliScales.Config
config = BaliScales.Config
    { config_layout = layout
    , config_keys = mempty -- pelog_keys
    , config_default_key = default_key
    , config_laras = laras
    , config_default_laras = laras_sequoia_pelog
    }
    where
    layout = Theory.diatonic_layout 7
    -- Just default_key = Map.lookup (Pitch.Key "lima") pelog_keys
    default_key = Theory.key (Pitch.Degree 0 0) "default" (replicate 5 1) layout

pelog_keys :: ChromaticScales.Keys
pelog_keys = Map.fromList $ map make
    --      4   7
    -- 1 2 3 5 6
    [ ("lima", 0, [1, 1, 2, 1, 2]) -- 12356
    , ("nem", 0, [1, 1, 2, 1, 2]) -- same as lima
    -- So it's relative keyboard, but absolute notation?
    --    4     1
    -- 2 3 5 6 7
    , ("barang", 1, [1, 2, 1, 1, 2]) -- 23567
    ]
    where
    make (name, tonic, intervals) =
        ( Pitch.Key name
        , Theory.key (Pitch.Degree tonic 0) name intervals
            (Theory.layout intervals)
        )

laras :: Map Text BaliScales.Laras
laras = Map.fromList $ Lists.keyOn BaliScales.laras_name
    [ laras_sequoia_pelog
    ]

-- laras name base_pitch extend doc nns = Laras
laras_sequoia_pelog :: BaliScales.Laras
laras_sequoia_pelog = BaliScales.laras "sequoia-pelog" (Pitch.pitch 3 5) id
    "Tuning of Sekar Sequoia." $ map (\nn -> (nn, nn))
    [ 58.68 -- 6.. (as3 + 0.5)
    , 60.13 -- 7.. (c4)
    , 62.18 -- 1. approx, no resonator
    , 63.65 -- 2.
    , 65    -- 3.
    , 68    -- 4. guess, no key
    , 69.05 -- 5.
    , 70.5  -- 6.
    , 72.14 -- 7.
    , 74.25 -- 1 approx
    , 75.68 -- 2
    , 77    -- 3
    , 80    -- 4 guess
    , 81.03 -- 5
    , 82.48 -- 6
    , 84.14 -- 7
    , 86.4  -- 1^ approx
    , 87.7  -- 2^
    , 88.98 -- 3^
    ]
