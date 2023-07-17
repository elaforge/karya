-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Javanese scales.

    @
    01 02 03 05 06 11 12 13 15 16 21 22 23 25 26 31 32 33 35 36 41 42 43 45 46
    gong---------->   kempul----------->            kenong----------->
                suwukan->
    11 12 13 15 16 21 22 23 25 26 31 32 33 35 36 41 42 43 45 46 51 52 53 55 56
                               kethuk                        pyang
                   slethem------->saron demung-->saron barung-->peking-------->
                gender barung---------------------------->
                6..1. 2. 3. 5. 6. 1  2  3  5  6  1^ 2^ 3^
                               gender panerus--------------------------->
                               6..1. 2. 3. 5. 6. 1  2  3  5  6  1^ 2^ 3^
                gambang------------------------------------------------->
           (?)  6..1. 2. 3. 5. 6  1  2  3  5  6  1^ 2^ 3^ 5^ 6^ 1^^2^^3^^
    11 12 13 15 16 21 22 23 25 26 31 32 33 35 36 41 42 43 45 46 51 52 53 55 56
                      siter------------------------------>
                                  bonang barung---------------->
                                                 bonang panerus--------------->
    @
-}
module Derive.Scale.Java (
    scales
    -- TESTING
) where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.JavaScales as JavaScales
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
    -- TODO This permutation game is no good, let's go back to key=pathet
    -- and maybe even scale-range=gender-panerus
    [ JavaScales.make_scale "pelog-lima" (lima Nothing) "doc"
    , JavaScales.make_scale "pelog-lima-gender-panerus"
        (lima (Just 4)) "doc"
    , JavaScales.make_scale "pelog-nem" (lima Nothing) "doc"
    , JavaScales.make_scale "pelog-barang" (barang Nothing) "doc"
    , JavaScales.make_scale "pelog-barang-gender-panerus"
        (barang (Just 4)) "doc"
    , JavaScales.make_scale "pelog-barang-gender-barung"
        (barang (Just 3)) "doc"
    ]
    where
    lima center = JavaScales.ScaleMap
        { smap_layout = JavaScales.make_layout 0 [1, 1, 2, 1, 2] -- 12356
        , smap_default_laras = laras_sequoia_pelog
        , smap_laras_map = laras
        , smap_format = format center
        }
    barang center = JavaScales.ScaleMap
        { smap_layout = JavaScales.make_layout 1 [1, 2, 1, 1, 2] -- 23567
        , smap_default_laras = laras_sequoia_pelog
        , smap_laras_map = laras
        , smap_format = format center
        }
    format Nothing = JavaScales.cipher_absolute 7
    format (Just center) = JavaScales.cipher_octave_relative 7 center


scales_old :: [Scale.Definition]
scales_old = map Scale.Simple
    [ BaliScales.make_scale "pelog" $
        BaliScales.scale_map config pelog_relative_keyed Nothing
    , BaliScales.make_scale "pelog-gender-panerus" $
        BaliScales.scale_map config (cipher_relative 5) Nothing
    , BaliScales.make_scale "pelog-gender-barung" $
        BaliScales.scale_map config (cipher_relative 4) Nothing
    ]
    where
    cipher_relative octave = cipher_relative_dotted octave
        (BaliScales.config_default_key config)
        (BaliScales.config_keys config)

-- make_relative_format :: Text -> Degrees -> RelativeFormat key -> Format
pelog_relative_keyed :: TheoryFormat.Format
pelog_relative_keyed =
    TheoryFormat.make_relative_format "[0-9][1-7]" cipher7 fmt
    where
    fmt = BaliScales.modify_config set_octaves $
        ChromaticScales.relative_fmt
            (BaliScales.config_default_key config)
            (BaliScales.config_keys config)
    set_octaves config = config
        { TheoryFormat.config_parse_octave = TheoryFormat.parse_octave1 }
    -- set_octaves = TheoryFormat.set_octave show_octave parse_octave

{-
pelog_relative2 all_keys default_key = TheoryFormat.RelativeFormat
    { rel_config = TheoryFormat.Config
        { config_show_octave = TheoryFormat.show_octave
        , config_parse_octave = TheoryFormat.parse_octave1
        , config_accidental = TheoryFormat.ascii_accidentals -- TODO Nothing
        }
    -- DEFAULT?
    , rel_key_config = TheoryFormat.KeyConfig
        { key_parse = Scales.get_key default_key all_keys
        , key_default = default_key
        }
    , rel_show_degree = TheoryFormat.show_degree_chromatic
    , rel_to_absolute = TheoryFormat.chromatic_to_absolute
    }

type ShowDegree key = key -> ShowOctave -> Degrees -> AccidentalFormat
    -> Either Pitch.Degree Pitch.Pitch -> Pitch.Note

show_degree :: TheoryFormat.ShowDegree Theory.Key
show_degree key show_octave degrees acc_fmt degree_pitch = undefined

show_degree_chromatic :: ShowDegree Theory.Key
show_degree_chromatic key show_octave degrees acc_fmt degree_pitch =
    Pitch.Note $ case degree_pitch of
        Left _ -> pc_text <> acc_text
        Right (Pitch.Pitch oct _) ->
            show_octave (oct + pc_oct) (pc_text <> acc_text)
    where
    Pitch.Degree pc acc = either id Pitch.pitch_degree degree_pitch
    acc_text = show_accidentals acc_fmt $ acc - Theory.accidentals_at_pc key pc
    (pc_oct, pc_text) =
        show_pc degrees (Pitch.degree_pc (Theory.key_tonic key)) pc

pelog_relative2 degrees = TheoryFormat.Format
    { fmt_show
    , fmt_read = p_pitch config degrees
    , fmt_to_absolute
    , fmt_pattern = octave_pattern <> pattern <> acc_pattern
    , fmt_pc_per_octave = Vector.length degrees
    , fmt_relative = True
    }
    where
    RelativeFormat config key_config show_degree to_abs = rel_fmt
    fmt_show key = show_degree
        (either (const (key_default key_config)) id (key_parse key_config key))
        (config_show_octave config) degrees (config_accidental config)
    fmt_to_absolute maybe_key pitch = do
        key <- key_parse key_config maybe_key
        return $ to_abs key degrees pitch
-}

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
    fmt = BaliScales.modify_config (BaliScales.dotted_octaves center) $
        ChromaticScales.relative_fmt default_key keys

cipher7 :: TheoryFormat.Degrees
cipher7 = TheoryFormat.make_degrees (map showt [1..7])

-- * config

config :: BaliScales.Config
config = BaliScales.Config
    { config_layout = layout
    , config_keys = pelog_keys
    , config_default_key = default_key
    , config_laras = laras
    , config_default_laras = laras_sequoia_pelog
    }
    where
    -- layout = Theory.diatonic_layout 7
    -- TODO layout changes based on key
    layout = Theory.layout [1, 1, 2, 1, 2]
    Just default_key = Map.lookup (Pitch.Key "lima") pelog_keys
    -- default_key = Theory.key (Pitch.Degree 0 0) "default" (replicate 5 1) layout

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
laras_sequoia_pelog = BaliScales.laras "sequoia-pelog" (Pitch.pitch 2 5) id
    "Tuning of Sekar Sequoia." $ map (\nn -> (nn, nn))
    [ 58.68 -- 26 6.. (as3 + 0.5)
    , 60.13 -- 27 7.. (c4)
    , 62.18 -- 31 1. approx, no resonator
    , 63.65 -- 32 2.
    , 65    -- 33 3.
    , 68    -- 34 4. guess, no key
    , 69.05 -- 35 5.
    , 70.5  -- 36 6.
    , 72.14 -- 37 7.
    , 74.25 -- 41 1 approx
    , 75.68 -- 42 2
    , 77    -- 43 3
    , 80    -- 44 4 guess
    , 81.03 -- 45 5
    , 82.48 -- 46 6
    , 84.14 -- 47 7
    , 86.4  -- 51 51 1^ approx
    , 87.7  -- 52 2^
    , 88.98 -- 53 3^
    ]
