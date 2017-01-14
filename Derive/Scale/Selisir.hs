-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is like "Derive.Scale.Legong", except specialized to the selisir
-- mode.
module Derive.Scale.Selisir where
import qualified Data.Vector as Vector

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import Global


scales :: [Scale.Make]
scales = Legong.make_scale_set config scale_id "The usual saih lima."

scale_id :: Pitch.ScaleId
scale_id = "selisir"

config :: BaliScales.Config
config = BaliScales.Config
    { config_layout = layout
    , config_base_octave = base_oct
    , config_keys = mempty
    , config_default_key = default_key
    , config_saihs = saihs
    , config_default_saih = Legong.default_saih
    }
    where
    layout = Theory.diatonic_layout 5
    default_key = Theory.key (Pitch.Degree 0 0) "default" (replicate 5 1) layout

-- | Lowest note start on this octave.
base_oct :: Pitch.Octave
base_oct = 3

saihs :: Map Text BaliScales.Saih
saihs = Legong.pitu_to_lima <$> Legong.saihs

saih_rambat :: BaliScales.Saih
saih_rambat = Legong.pitu_to_lima Legong.saih_rambat

-- TODO duplicated with Legong
instrument_scale ::
    ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Saih -> BaliScales.Tuning -> Patch.Scale
instrument_scale take_range saih tuning =
    Patch.make_scale ("selisir " <> ShowVal.show_val tuning) $
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
    trim = take (5*5 + 1)
    keys oct = map (Midi.to_key (oct * 12) +)
        [Key.c_1, Key.d_1, Key.e_1, Key.g_1, Key.a_1] -- i o e u a
