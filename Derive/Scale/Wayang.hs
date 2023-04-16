-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Saih gender wayang.

    I use ding deng dong dung dang.  I don't know if this is ever actually used
    for gender, but the notation is compact and I don't think there are any
    other conventions.

    @
    3o  3e  3u  3a  4i  4o  4e  4u  4a  5i  5o  5e  5u  5a  6i
    pemade -------------------------------
                        kantilan -----------------------------
    3a  4i  4o  4e  4u  4a  5i  5o  5e  5u  5a  6i  6o  6e  6u
    36  41  42  43  45  46  51
    3d  4s  4r  4g  4p  4d  5s
    @
-}
module Derive.Scale.Wayang where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Bali as Bali
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.McPhee as McPhee
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ShowVal as ShowVal

import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = map (Scale.Simple . Scales.add_doc "Saih gender wayang.")
    [ BaliScales.make_scale scale_id $
        BaliScales.scale_map (config laras_sawan)
            BaliScales.ioeua_absolute Nothing
    , BaliScales.make_scale "wayang-a" $
        BaliScales.scale_map (rebase (Pitch.pitch 1 U) (config laras_sawan))
            BaliScales.ioeua_absolute Nothing
    , BaliScales.make_scale "wayang-srg" $
        BaliScales.scale_map (rebase (Pitch.pitch 1 U) (config laras_sawan))
            sargam_absolute Nothing
    , Scales.add_doc
        "Pemade scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.make_scale "wayang-pemade"
            (inst_scale_map laras_sawan_pemade pemade)
    , Scales.add_doc
        "Kantilan scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.make_scale "wayang-kantilan"
            (inst_scale_map laras_sawan_kantilan kantilan)
    ]
    where
    inst_scale_map laras = BaliScales.instrument_scale_map (config laras)

sargam_absolute :: TheoryFormat.Format
sargam_absolute = TheoryFormat.make_absolute_format "[1-9][srgpd]" $
    TheoryFormat.make_degrees $ map Text.singleton "srgpd"

pemade :: BaliScales.Instrument
pemade = instrument 4 (Pitch.pitch 3 O) (Pitch.pitch 5 I)

kantilan :: BaliScales.Instrument
kantilan = instrument 5 (Pitch.pitch 4 O) (Pitch.pitch 6 I)

instrument :: Pitch.Octave -> Pitch.Pitch -> Pitch.Pitch
    -> BaliScales.Instrument
instrument = BaliScales.Instrument BaliScales.ioeua BaliScales.arrow_octaves

config :: BaliScales.Laras -> BaliScales.Config
config default_laras = BaliScales.Config
    { config_layout = layout
    , config_keys = mempty
    , config_default_key = default_key
    , config_laras = laras
    , config_default_laras = default_laras
    }
    where
    layout = Theory.diatonic_layout 5
    default_key = Theory.key (Pitch.Degree 0 0) "default" (replicate 5 1) layout

rebase :: Pitch.Pitch -> BaliScales.Config -> BaliScales.Config
rebase base config = config
    { BaliScales.config_laras = set <$> BaliScales.config_laras config
    , BaliScales.config_default_laras =
        set $ BaliScales.config_default_laras config
    }
    where
    set laras = laras { BaliScales.laras_base = base }

-- | Start octave for the extended scale.
base_oct :: Pitch.Octave
base_oct = 1

scale_id :: Pitch.ScaleId
scale_id = "wayang"

-- * laras

data Pitch = I | O | E | U | A deriving (Eq, Enum, Show)

laras :: Map Text BaliScales.Laras
laras = Map.fromList $ Lists.keyOn BaliScales.laras_name $
    laras_sawan
    : laras_sawan_pemade
    : laras_sawan_kantilan
    : mcphee

laras_sawan :: BaliScales.Laras
laras_sawan = BaliScales.laras "sawan" (Pitch.pitch 1 I)
    (extend (BaliScales.inst_low pemade))
    "Tuning from my gender wayang, made in Sawan, Singaraja." $
    -- Of course the overlapping parts of the scales are a bit different, and
    -- I have to pick one.  So I choose the pemade version.
    BaliScales.laras_nns laras_sawan_pemade
    ++ drop 5 (BaliScales.laras_nns laras_sawan_kantilan)

laras_sawan_pemade :: BaliScales.Laras
laras_sawan_pemade = BaliScales.laras "sawan-pemade" (Pitch.pitch 3 1) id
    "Sawan tuning for pemade."
    [ (52.27, 52.94)
    , (54.55, 55.15)
    , (57.35, 57.90)
    , (59.85, 60.32)

    , (62.50, 63.00) -- 4i
    , (64.45, 64.72)
    , (67.29, 67.60)
    , (69.25, 69.48)
    , (71.83, 72.11)
    , (74.66, 74.85) -- 5i
    ]

laras_sawan_kantilan :: BaliScales.Laras
laras_sawan_kantilan = BaliScales.laras "sawan-kantilan" (Pitch.pitch 4 1) id
    "Sawan tuning for kantilan"
    [ (64.31, 64.70)
    , (67.13, 67.45)
    , (69.22, 69.46)
    , (71.81, 72.00)

    , (74.57, 74.80) -- 5i
    , (76.75, 76.88)
    , (79.37, 79.50)
    , (81.53, 81.65)
    , (84.02, 84.13)
    , (86.79, 86.90) -- 6i
    ]

mcphee :: [BaliScales.Laras]
mcphee = map (make . McPhee.extract low_pitch high_pitch) McPhee.slendro
    where
    make (name, (nns, doc)) =
        BaliScales.laras name (Pitch.pitch 1 0) id doc
            (map (\nn -> (nn, nn)) nns)

-- | Extend down two octaves so that I start at 1i, and up two octaves to 8i.
--
-- pemade starts at 3o - 4i - 5i, kanti is 4o - 5i - 6i
extend :: Pitch.Pitch -> [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend from = Bali.extend_scale 5 low_pitch high_pitch from

low_pitch, high_pitch :: Pitch.Pitch
low_pitch = Pitch.pitch 1 I
high_pitch = Pitch.pitch 8 I

undo_extend :: [a] -> [a]
undo_extend = take 15 . drop (1 + 5 + 5)
    -- take (kantilan_high - kantilan_low) . drop (pemade_low - 1 I)

-- * instrument integration

instrument_scale :: Bool -> BaliScales.Laras -> BaliScales.Tuning -> Patch.Scale
instrument_scale extended laras tuning =
    Patch.make_scale ("wayang " <> ShowVal.show_val tuning) $
        zip (midi_keys extended)
            ((if extended then id else undo_extend) (Vector.toList nns))
    where
    nns = case tuning of
        BaliScales.Umbang -> BaliScales.laras_umbang laras
        BaliScales.Isep -> BaliScales.laras_isep laras

-- | If extended is True, emit from i1 on up.  Otherwise, give pemade to
-- kantilan range.
midi_keys :: Bool -> [Midi.Key]
midi_keys extended = trim $ concatMap keys [base_oct + 1 ..]
    -- base_oct + 1 because MIDI starts at octave -1
    where
    trim
        | extended = take (7*5 + 1)
        | otherwise = take (3*5) . drop (1 + 3*5)
    keys oct = map (Midi.to_key (oct * 12) +) -- i o e u a
        [Key2.e_2, Key2.f_2, Key2.a_2, Key2.b_2, Key2.c_1]
