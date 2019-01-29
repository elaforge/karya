-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Saih pitu scales.

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

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.Scale as Scale
import qualified Derive.Scale.Bali as Bali
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.McPhee as McPhee
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ShowVal as ShowVal

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch

import Global


scales :: [Scale.Definition]
scales = make_scale_set config scale_id "Saih pelegongan, from my instruments."

scale_id :: Pitch.ScaleId
scale_id = "legong"

make_scale_set :: BaliScales.Config -> Pitch.ScaleId -> Doc.Doc
    -> [Scale.Definition]
make_scale_set config (Pitch.ScaleId prefix) doc =
    map (Scale.Simple . Scales.add_doc doc)
    [ BaliScales.make_scale (id_with "") (scale_map complete_scale)
    , BaliScales.make_scale (id_with "b") (scale_map complete_scale_balinese)
    , Scales.add_doc "Use Javanese-style cipher notation." $
        -- TODO use 4 and 7 instead of 3# and 6#.
        -- Use simple_scale like *wayang and *selesir?
        BaliScales.make_scale (id_with "c") (scale_map cipher_scale)
    , inst_doc "pemade" $ BaliScales.make_scale (id_with "pemade")
        (inst_scale_map pemade)
    , inst_doc "pemade" $ BaliScales.make_scale (id_with "pemade-b")
        (inst_scale_map (balinese_notation pemade))
    , inst_doc "kantilan" $ BaliScales.make_scale (id_with "kantilan")
        (inst_scale_map kantilan)
    , inst_doc "kantilan" $ BaliScales.make_scale (id_with "kantilan-b")
        (inst_scale_map (balinese_notation kantilan))
    ]
    where
    id_with suffix = Pitch.ScaleId $ TextUtil.joinWith "-" prefix suffix
    inst_doc name = Scales.add_doc $
        "This is centered around the " <> name <> " range."
    scale_map fmt = BaliScales.scale_map config fmt Nothing
    inst_scale_map = BaliScales.instrument_scale_map config

    complete_scale = BaliScales.ioeua_relative True
        (BaliScales.config_default_key config) (BaliScales.config_keys config)
    complete_scale_balinese =
        BaliScales.digit_octave_relative BaliScales.balinese True
            (BaliScales.config_default_key config)
            (BaliScales.config_keys config)
    cipher_scale = BaliScales.cipher_relative_dotted 5
        (BaliScales.config_default_key config)
        (BaliScales.config_keys config)

jegog, calung, penyacah :: BaliScales.Instrument
jegog = instrument 1 (Pitch.pitch 3 I) (Pitch.pitch 3 As)
calung = instrument 2 (Pitch.pitch 4 I) (Pitch.pitch 4 As)
penyacah = instrument 3 (Pitch.pitch 5 0) (Pitch.pitch 5 As)

pemade, kantilan :: BaliScales.Instrument
pemade = instrument 5 (Pitch.pitch 4 O) (Pitch.pitch 6 I)
kantilan = instrument 6 (Pitch.pitch 5 O) (Pitch.pitch 7 I)

-- * config

instrument :: Pitch.Octave -> Pitch.Pitch -> Pitch.Pitch
    -> BaliScales.Instrument
instrument = BaliScales.Instrument BaliScales.ioeua BaliScales.arrow_octaves

balinese_notation :: BaliScales.Instrument -> BaliScales.Instrument
balinese_notation inst = inst
    { BaliScales.inst_degrees = BaliScales.balinese
    , BaliScales.inst_relative_octaves = BaliScales.balinese_octaves
    }

-- | These are from Tenzer's "Gamelan Gong Kebyar", page 29.  This is Dewa
-- Beratha's definition.  McPhee's book has different names for gambuh, but
-- Beratha's is probably more modern.
--
-- These are assigned with @key=...@.  McPhee calls them tekepan (suling) or
-- ambah.  Or I could use patutan / pathet.
config :: BaliScales.Config
config = BaliScales.Config
    { config_layout = layout
    , config_keys = all_keys
    , config_default_key = default_key
    , config_laras = laras
    , config_default_laras = laras_rambat
    }
    where
    layout = Theory.layout intervals
    Just default_key = Map.lookup (Pitch.Key "selisir") all_keys

intervals :: [Pitch.Semi]
intervals = [1, 1, 2, 1, 2]

all_keys :: ChromaticScales.Keys
all_keys = Map.fromList $ zipWith make [0..]
    [ "selisir"         -- 123_45_  ioe_ua_
    , "slendro-gede"    -- _234_67  _ioe_ua
    , "baro"            -- 1_345_7  a_ioe_u
    , "tembung"         -- 12_456_  ua_ioe_
    , "sunaren"         -- _23_567  _ua_ioe
    -- hypothetical
    , "pengenter-alit"  -- 1_34_67  e_ua_io
    , "pengenter"       -- 12_45_7  oe_ua_i
    ]
    where
    make tonic name =
        ( Pitch.Key name
        , Theory.key (Pitch.Degree tonic 0) name intervals
            (BaliScales.config_layout config)
        )

ugal_range, rambat_range, trompong_range, reyong_range :: Scale.Range
ugal_range = Scale.Range (Pitch.pitch 3 O) (Pitch.pitch 5 I)
rambat_range = Scale.Range (Pitch.pitch 3 E) (Pitch.pitch 6 I)
trompong_range = Scale.Range (Pitch.pitch 3 A) (Pitch.pitch 5 U)
reyong_range = Scale.Range (Pitch.pitch 4 E) (Pitch.pitch 6 U)

-- * laras

data Pitch = I | O | E | Es | U | A | As
    deriving (Eq, Ord, Enum, Show, Bounded)

laras :: Map Text BaliScales.Laras
laras = Map.fromList $ Seq.key_on BaliScales.laras_name $
    laras_rambat : mcphee

laras_rambat :: BaliScales.Laras
laras_rambat = BaliScales.laras "rambat" low_pitch (extend 3 E)
    "From my gender rambat, made in Blabatuh, Gianyar, tuned in\
    \ Munduk, Buleleng."
    [ (51.03,   52.23)  -- 3e, rambat begin
    , (54.00-4, 54.00)  -- TODO
    , (55.05,   56.05)  -- 3u
    , (56.10,   56.78)  -- 3a, trompong begin
    , (58.00-4, 58.00)  -- TODO

    , (60.73,   60.69)  -- 4i
    , (61.80,   62.41)  -- 4o, pemade begin
    , (62.90,   63.49)  -- 4e, reyong begin
    , (65.00-4, 65.00)  -- TODO
    , (67.15,   67.63)  -- 4u
    , (68.06,   68.49)  -- 4a
    , (70.00-4, 70.00)  -- TODO

    , (71.88,   72.27)  -- 5i
    , (73.60,   73.90)  -- 5o, kantilan begin
    , (75.15,   75.50)  -- 5e
    , (78.00-4, 78.00)  -- TODO
    , (79.12,   79.37)  -- 5u, trompong end
    , (80.27,   80.34)  -- 5a
    , (83.00-4, 83.00)  -- TODO

    , (84.09,   84.30)  -- 6i, rambat end, pemade end
    , (86.08-4, 86.08)  -- 6o
    , (87.82-4, 87.82)  -- 6e
    , (90.00-4, 90.00)  -- TODO
    , (91.82-4, 91.82)  -- 6u, reyong end
    , (92.50-4, 92.50)  -- 6a
    , (95.00-4, 95.00)  -- TODO

    , (96.46-4, 96.46)  -- 7i, kantilan end
    ]
    where
    nn - hz = Pitch.add_hz (-hz) nn

allTunings :: [[Pitch.NoteNumber]]
allTunings =
    -- rambat u, i,  reyong
    [ [51.03, 52.23, 0]         -- 3e
    , [55.05, 56.05, 0]
    , [56.10, 56.78, 56.77]
    , [60.73, 60.69, 60.83]     -- 4i
    , [61.80, 62.41, 62.82]
    , [62.90, 63.49, 63.36]
    , [67.15, 67.63, 67.72]
    , [68.06, 68.49, 68.35]
    , [71.88, 72.27, 72.60]     -- 5i
    , [73.60, 73.90, 74.09]
    , [75.13, 75.50, 75.54]
    , [79.12, 79.37, 79.45]
    , [80.27, 80.34, 80.50]
    , [84.09, 84.30, 84.53]     -- 6i
    , [    0,     0, 86.08]
    , [    0,     0, 87.82]
    , [    0,     0, 91.82]
    ]

-- | Extend down to 3i, which is jegog range.
extend :: Pitch.Octave -> Pitch -> [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend oct pc = Bali.extend_scale 7 low_pitch high_pitch (Pitch.pitch oct pc)

-- | Lowest note starts on this octave.
base_octave :: Pitch.Octave
base_octave = 3

low_pitch, high_pitch :: Pitch.Pitch
low_pitch = Pitch.pitch base_octave I
high_pitch = Pitch.pitch 7 I

mcphee :: [BaliScales.Laras]
mcphee = map (make . McPhee.extract low_pitch high_pitch) McPhee.saih_pitu
    where
    make (name, (nns, doc)) =
        BaliScales.laras name low_pitch id doc
            (map (\nn -> (nn, nn)) nns)

-- * instrument integration

-- | A Scale with the entire theoretical range.  This is for instruments
-- that are normalized to 12tet and then tuned in the patch (e.g. using KSP).
complete_instrument_scale :: BaliScales.Laras -> BaliScales.Tuning
    -> Patch.Scale
complete_instrument_scale = instrument_scale id

instrument_scale ::
    ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Laras -> BaliScales.Tuning -> Patch.Scale
instrument_scale =
    make_instrument_scale "legong" -- i o e e# u a a#
        [Key.c_1, Key.d_1, Key.e_1, Key.f_1, Key.g_1, Key.a_1, Key.b_1]

make_instrument_scale :: Text -> [Midi.Key]
    -> ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Laras -> BaliScales.Tuning -> Patch.Scale
make_instrument_scale name keys take_range laras tuning =
    Patch.make_scale (name <> " " <> ShowVal.show_val tuning) $
        take_range $ zip (midi_keys keys) (Vector.toList nns)
    where
    nns = case tuning of
        BaliScales.Umbang -> BaliScales.laras_umbang laras
        BaliScales.Isep -> BaliScales.laras_isep laras

-- | Emit from i3 on up.
midi_keys :: [Midi.Key] -> [Midi.Key]
midi_keys keys = trim $ concatMap octave_keys [base_octave + 1 ..]
    -- base_octave + 1 because MIDI starts at octave -1
    where
    trim = take (5 * length keys + 1)
    octave_keys oct = map (Midi.to_key (oct * 12) +) keys
