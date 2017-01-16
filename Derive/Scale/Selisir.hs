-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is like "Derive.Scale.Legong", except specialized to the selisir
-- mode.
module Derive.Scale.Selisir where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Util.Seq as Seq
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Bali as Bali
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.McPhee as McPhee
import qualified Derive.Scale.Theory as Theory

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
    , config_base_octave = base_octave
    , config_keys = mempty
    , config_default_key = default_key
    , config_saihs = saihs
    , config_default_saih = Legong.default_saih
    }
    where
    layout = Theory.diatonic_layout 5
    default_key = Theory.key (Pitch.Degree 0 0) "default" (replicate 5 1) layout

-- | Lowest note start on this octave.
base_octave :: Pitch.Octave
base_octave = 3

saihs :: Map Text BaliScales.Saih
saihs = (pitu_to_lima <$> Legong.saihs)
    <> Map.fromList (("pegulingan-teges", pegulingan_teges) : mcphee)

mcphee :: [(Text, BaliScales.Saih)]
mcphee =
    map (make . McPhee.extract Legong.low_pitch Legong.high_pitch)
        McPhee.selisir
    where
    make (name, (nns, doc)) =
        (name, BaliScales.saih id doc (map (\nn -> (nn, nn)) nns))

-- | Exported for instruments to use.
saih_rambat :: BaliScales.Saih
saih_rambat = pitu_to_lima Legong.saih_rambat

-- | Strip extra notes to get back to saih lima.
pitu_to_lima :: BaliScales.Saih -> BaliScales.Saih
pitu_to_lima (BaliScales.Saih doc umbang isep) = BaliScales.Saih
    { saih_doc = doc
    , saih_umbang = strip umbang
    , saih_isep = strip isep
    }
    where
    strip = Vector.fromList
        . concatMap (\nns -> mapMaybe (Seq.at nns) [0, 1, 2, 4, 5])
        . Seq.chunked 7 . Vector.toList

data Pitch = I | O | E | U | A
    deriving (Eq, Ord, Enum, Show, Bounded)

-- TODO what is the ombak?
pegulingan_teges :: BaliScales.Saih
pegulingan_teges = BaliScales.saih (extend 4 U)
    "From Teges Semar Pegulingan, via Bob Brown's 1972 recording."
    $ map (\nn -> (nn, nn))
    [ 69.55 -- 4u
    , 70.88 -- 4a
    , 75.25 -- 5i
    , 76.90 -- 5o, kantilan begin
    , 77.94 -- 5e
    , 81.80 -- 5u... should I agree with the lower octave?
    ]

-- | Extend down to the "Legong" range.
extend :: Pitch.Octave -> Pitch -> [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend oct pc =
    Bali.extend_scale 7 Legong.low_pitch Legong.high_pitch (Pitch.pitch oct pc)

instrument_scale ::
    ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Saih -> BaliScales.Tuning -> Patch.Scale
instrument_scale = Legong.make_instrument_scale "selisir"
    [Key.c_1, Key.d_1, Key.e_1, Key.g_1, Key.a_1] -- i o e u a
