-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Saih pelegongan.

    Tuning from my gender rambat.  I extend the scale to cover the complete
    gong range.  I start at octave 3 so that the octaves more or less line
    up with 12tet.

    TODO: add pengisep and pengumbang

    @
    3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
    jegog---------
                   calung--------
                                  penyacah------
       ugal-------------------------
          rambat-----------------------------------
    0              7              14             21             28
    3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
                trompong---------------------
                      pemade-----------------------
                                     kantilan---------------------
                         reyong-----------------------------
                         |1-----|---       |3--|---
                                  |2-----|---    |4--------|
    3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
    @
-}
module Derive.Scale.Legong where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Util.Seq as Seq
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales =
    map (Scale.Simple . Scales.add_doc "Saih pelegongan, from my instruments.")
    [ BaliScales.make_scale scale_id complete_scale
    , BaliScales.make_scale "legong-b" complete_scale_balinese
    , Scales.add_doc "Use Javanese-style cipher notation. This can be more\
        \ convenient for saih pitu." $
        -- TODO use 4 and 7 instead of 3# and 6#.
        BaliScales.make_scale "legong-c" cipher_scale
    , inst_doc "pemade" $ BaliScales.make_scale "legong-pemade" pemade
    , inst_doc "pemade" $ BaliScales.make_scale "legong-pemade-b" pemade_b
    , inst_doc "kantilan" $ BaliScales.make_scale "legong-kantilan" kantilan
    , inst_doc "kantilan" $ BaliScales.make_scale "legong-kantilan-b" kantilan_b
    ]
    where
    inst_doc name =
        Scales.add_doc ("This is centered around the " <> name <> " range.")

complete_scale :: BaliScales.ScaleMap
complete_scale = scale_map (BaliScales.ioeua_relative True default_key all_keys)

complete_scale_balinese :: BaliScales.ScaleMap
complete_scale_balinese =
    scale_map (BaliScales.digit_octave_relative BaliScales.balinese True
        default_key all_keys)

cipher_scale :: BaliScales.ScaleMap
cipher_scale = scale_map
    (BaliScales.cipher_relative_dotted 5 default_key all_keys)

scale_map :: TheoryFormat.Format -> BaliScales.ScaleMap
scale_map fmt =
    BaliScales.scale_map layout fmt base_oct all_keys default_key
        saihs default_saih Nothing

jegog, calung, penyacah :: BaliScales.ScaleMap
jegog = inst_scale_map 1 (Pitch.pitch 3 I) (Pitch.pitch 3 As)
calung = inst_scale_map 2 (Pitch.pitch 4 I) (Pitch.pitch 5 As)
penyacah = inst_scale_map 3 (Pitch.pitch 5 0) (Pitch.pitch 5 As)

pemade, pemade_b :: BaliScales.ScaleMap
pemade = inst_scale_map 5 (Pitch.pitch 4 O) (Pitch.pitch 6 I)
pemade_b = inst_scale_map_balinese 5 (Pitch.pitch 4 O) (Pitch.pitch 6 I)

kantilan, kantilan_b :: BaliScales.ScaleMap
kantilan = inst_scale_map 6 (Pitch.pitch 5 O) (Pitch.pitch 7 I)
kantilan_b = inst_scale_map_balinese 6 (Pitch.pitch 5 O) (Pitch.pitch 7 I)

inst_scale_map :: Pitch.Octave -> Pitch.Pitch -> Pitch.Pitch
    -> BaliScales.ScaleMap
inst_scale_map =
    BaliScales.instrument_scale_map
        BaliScales.ioeua BaliScales.arrow_octaves
        layout all_keys default_key saihs default_saih base_oct

inst_scale_map_balinese :: Pitch.Octave -> Pitch.Pitch -> Pitch.Pitch
    -> BaliScales.ScaleMap
inst_scale_map_balinese =
    BaliScales.instrument_scale_map
        BaliScales.balinese BaliScales.balinese_octaves
        layout all_keys default_key saihs default_saih base_oct

scale_id :: Pitch.ScaleId
scale_id = "legong"

layout :: Theory.Layout
layout = Theory.layout [1, 1, 2, 1, 2]

-- | These are from Tenzer's kebyar book, for Semar Pegulingan.  McPhee's
-- book has different names for gambuh, but Tenzer's book is more modern.
all_keys :: ChromaticScales.Keys
all_keys = BaliScales.make_keys layout $ map make_key
    [ ("selisir", [1, 2, 3, 5, 6])
    , ("slendro-gede", [2, 3, 4, 6, 7])
    , ("baro", [1, 3, 4, 5, 7])
    , ("tembung", [1, 2, 4, 5, 6])
    , ("sunaren", [2, 3, 5, 6, 7])
    , ("pengenter-alit", [1, 3, 4, 6, 7])
    , ("pengenter", [1, 2, 4, 5, 7])
    -- TODO these all have a hardcoded layout that assumes some "accidentals".
    -- For lebeng I can just use selisir with all the notes.
    -- , ("lebeng", [1, 2, 3, 4, 5, 6, 7])
    ]

make_key :: (Text, [Pitch.Semi]) -> (Text, Pitch.Semi, [Pitch.Semi])
make_key (_, []) = errorStack "no semis for scale"
make_key (name, n : ns) = (name, n - 1, zipWith (-) (ns ++ [n+7]) (n:ns))

default_key :: Theory.Key
Just default_key = Map.lookup (Pitch.Key "selisir") all_keys

ugal_range, rambat_range, trompong_range, reyong_range :: Scale.Range
ugal_range = Scale.Range (Pitch.pitch 3 O) (Pitch.pitch 5 I)
rambat_range = Scale.Range (Pitch.pitch 3 E) (Pitch.pitch 6 I)
trompong_range = Scale.Range (Pitch.pitch 3 A) (Pitch.pitch 5 U)
reyong_range = Scale.Range (Pitch.pitch 4 E) (Pitch.pitch 6 U)

-- | Lowest note start on this octave.
base_oct :: Pitch.Octave
base_oct = 3

-- * saih

data Pitch = I | O | E | Es | U | A | As deriving (Eq, Enum, Show)

default_saih :: Text
default_saih = "rambat"

saihs :: Map.Map Text BaliScales.Saih
saihs = Map.fromList
    [ (default_saih, saih_rambat)
    , ("teges", saih_teges_pegulingan)
    ]

saih_rambat :: BaliScales.Saih
saih_rambat = BaliScales.saih (extend 3 E)
    "From my gender rambat, made in Blabatuh, Gianyar, tuned in\
    \ Munduk, Buleleng."
    $ map (second (Pitch.add_hz 4)) -- TODO until I measure real values
    [ (51.82,   51.82)  -- 3e, rambat begin
    , (54.00,   54.00)  -- TODO
    , (55.70,   55.70)  -- 3u
    , (56.82,   56.82)  -- 3a, trompong begin
    , (58.00,   58.00)  -- TODO

    , (60.73,   60.73)  -- 4i
    , (62.80,   62.80)  -- 4o, pemade begin
    , (63.35,   63.35)  -- 4e, reyong begin
    , (65.00,   65.00)  -- TODO
    , (67.70,   67.70)  -- 4u
    , (68.20,   68.20)  -- 4a
    , (70.00,   70.00)  -- TODO

    , (72.46,   72.46)  -- 5i
    , (73.90,   73.90)  -- 5o, kantilan begin
    , (75.50,   75.50)  -- 5e
    , (78.00,   78.00)  -- TODO
    , (79.40,   79.40)  -- 5u, trompong end
    , (80.50,   80.50)  -- 5a
    , (83.00,   83.00)  -- TODO

    , (84.46,   84.46)  -- 6i, rambat end, pemade end
    , (86.00,   86.00)  -- 6o
    , (87.67,   87.67)  -- 6e
    , (90.00,   90.00)  -- TODO
    , (91.74,   91.74)  -- 6u, reyong end
    , (92.50,   92.50)  -- 6a
    , (95.00,   95.00)  -- TODO

    , (96.46,   96.46)  -- 7i, kantilan end
    ]

saih_teges_pegulingan :: BaliScales.Saih
saih_teges_pegulingan = BaliScales.saih (extend 4 U)
    "From Teges Semar Pegulingan, via Bob Brown's 1972 recording.\
    \ The original is saih lima, so pemero and penyerog are invented."
    $ map (\nn -> (nn, nn))
    [ 69.55 -- 4u
    , 70.88 -- 4a
    , 73.00

    , 75.25 -- 5i
    , 76.90 -- 5o, kantilan begin
    , 77.94 -- 5e
    , 81.00
    , 81.80 -- 5u... should I agree with the lower octave?
    ]

-- | Extend down to 3i, which is jegog range.
extend :: Pitch.Octave -> Pitch -> [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend oct pc = BaliScales.extend_scale 7
    (Pitch.pitch base_oct I) (Pitch.pitch 7 I) (Pitch.pitch oct pc)

-- | I don't actually have the pemero notes on my instruments, so strip them
-- back out to get the actually recorded scale.
strip_pemero :: [a] -> [a]
strip_pemero = go
    where
    go [] = []
    go nns = strip nns ++ go (drop 7 nns)
    strip nns = mapMaybe (Seq.at nns) [0, 1, 2, 4, 5]

-- * instrument integration

-- | A Scale with the entire theoretical range.  This is for instruments
-- that are normalized to 12tet and then tuned in the patch (e.g. using KSP).
complete_instrument_scale :: BaliScales.Saih -> BaliScales.Tuning -> Patch.Scale
complete_instrument_scale = instrument_scale id

instrument_scale ::
    ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Saih -> BaliScales.Tuning -> Patch.Scale
instrument_scale take_range saih tuning =
    Patch.make_scale ("legong " <> ShowVal.show_val tuning) $
        take_range $ zip midi_keys (Vector.toList nns)
    where
    nns = case tuning of
        BaliScales.Umbang -> BaliScales.saih_umbang saih
        BaliScales.Isep -> BaliScales.saih_isep saih

-- | Emit from i3 on up.
midi_keys :: [Midi.Key]
midi_keys = trim $ concatMap keys [base_oct + 1 ..]
    -- base_oct + 1 because MIDI starts at octave -1
    where
    trim = take (5*7 + 1)
    keys oct = map (Midi.to_key (oct * 12) +) -- i o e e# u a a#
        [Key.c_1, Key.d_1, Key.e_1, Key.f_1, Key.g_1, Key.a_1, Key.b_1]
