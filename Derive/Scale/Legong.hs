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
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

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
    id_with suffix = Pitch.ScaleId $ Texts.join2 "-" prefix suffix
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

instrument :: Pitch.Octave -> Pitch.Pitch -> Pitch.Pitch
    -> BaliScales.Instrument
instrument = BaliScales.Instrument BaliScales.ioeua BaliScales.arrow_octaves

-- * config

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
laras = Map.fromList $ Lists.keyOn BaliScales.laras_name $
    laras_rambat : mcphee

laras_rambat :: BaliScales.Laras
laras_rambat = BaliScales.laras "rambat" low_pitch (extend 3 E)
    "From my gender rambat, made in Blabatuh, Gianyar, tuned in\
    \ Munduk, Buleleng."
    [ (51.03,   51.85)  -- 3e, rambat begin
    , (p4 - 12, i $ p4 - 12)
    , (55.05,   55.67)  -- 3u
    , (56.10,   56.50)  -- 3a, trompong begin
    , (p7 - 12, i $ p7 - 12)

    , (59.91,   60.40)  -- 4i
    , (61.80,   62.41)  -- 4o, pemade begin
    , (62.90,   63.27)  -- 4e, reyong begin
    , (p4,      i p4)
    , (67.15,   67.48)  -- 4u
    , (68.06,   68.33)  -- 4a
    , (p7,      i p7)

    , (71.88,   72.15)  -- 5i
    , (73.60,   73.80)  -- 5o, kantilan begin
    , (75.15,   75.38)  -- 5e
    , (p4 + 12, i $ p4 + 12)
    , (79.12,   79.28)  -- 5u, trompong end
    , (80.27,   80.26)  -- 5a
    , (p7 + 12, i $ p7 + 12)

    , (84.09,   84.24)  -- 6i, rambat end, pemade end
    , (u 86.08, 86.08)  -- 6o <- starts from reyong, which is more like i
    , (u 87.82, 87.82)  -- 6e
    , (p4 + 24, i $ p4 + 24)
    , (u 91.82, 91.82)  -- 6u, reyong end
    , (u 92.50, 92.50)  -- 6a
    , (p7 + 24, i $ p7 + 24)

    , (u 96.46, 96.46)  -- 7i, kantilan end
    ]
    where
    -- These are arbitrary since I don't have those notes, but let's pick what
    -- they could have been.
    p4 = 65.9
    p7 = 70
    i = Pitch.add_hz 4
    u = Pitch.add_hz (-4)

allTunings :: [[Pitch.NoteNumber]]
allTunings =
    -- rambat u, i,  reyong
    [ [51.03, 51.85, 0]         -- 3e
    , [55.05, 55.67, 0]
    , [56.10, 56.50, 56.77]
    , [59.91, 60.40, 60.83]     -- 4i
    , [61.80, 62.41, 62.82]
    , [62.90, 63.27, 63.36]
    , [67.15, 67.48, 67.72]
    , [68.06, 68.33, 68.35]
    , [71.88, 72.15, 72.60]     -- 5i
    , [73.60, 73.80, 74.09]
    , [75.13, 75.38, 75.54]
    , [79.12, 79.28, 79.45]
    , [80.27, 80.26, 80.50]
    , [84.09, 84.24, 84.53]     -- 6i
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
