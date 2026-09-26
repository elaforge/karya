-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is like "Derive.Scale.Legong", except specialized to the selisir
-- mode.
module Derive.Scale.Selisir where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Bali as Bali
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.McPhee as McPhee
import qualified Derive.Scale.Theory as Theory

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = Legong.makeScaleSet config scaleId "The usual saih lima."

scaleId :: Pitch.ScaleId
scaleId = "selisir"

config :: BaliScales.Config
config = BaliScales.Config
    { configLayout = layout
    , configKeys = mempty
    , configDefaultKey = defaultKey
    , configLaras = laras
    , configDefaultLaras = larasRambat
    }
    where
    layout = Theory.diatonicLayout 5
    defaultKey = Theory.key (Pitch.Degree 0 0) "default" (replicate 5 1) layout

laras :: Map Text BaliScales.Laras
laras = BaliScales.larasMap $ concat
    [ map pituToLima (Map.elems Legong.laras)
    , [pegulinganTeges]
    , mcphee
    ]

mcphee :: [BaliScales.Laras]
mcphee =
    map (make . McPhee.extract Legong.lowPitch Legong.highPitch)
        McPhee.selisir
    where
    make (name, (nns, doc)) =
        BaliScales.laras name lowPitch id doc (map (\nn -> (nn, nn)) nns)

-- | Exported for instruments to use.
larasRambat :: BaliScales.Laras
larasRambat = pituToLima Legong.larasRambat

-- | Strip extra notes to get back to saih lima.
pituToLima :: BaliScales.Laras -> BaliScales.Laras
pituToLima laras = laras
    { BaliScales.larasUmbang = strip $ BaliScales.larasUmbang laras
    , BaliScales.larasIsep = strip $ BaliScales.larasIsep laras
    }
    where
    strip = Vector.fromList
        . concatMap (\nns -> mapMaybe (Lists.at nns) [0, 1, 2, 4, 5])
        . Lists.chunked 7 . Vector.toList

data Pitch = I | O | E | U | A
    deriving (Eq, Ord, Enum, Show, Bounded)

lowPitch :: Pitch.Pitch
lowPitch = Legong.lowPitch

-- TODO what is the ombak?
pegulinganTeges :: BaliScales.Laras
pegulinganTeges = BaliScales.laras "pegulingan-teges" lowPitch (extend 4 U)
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
    Bali.extendScale 7 Legong.lowPitch Legong.highPitch (Pitch.pitch oct pc)

instrumentScale ::
    ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Laras -> BaliScales.Tuning -> Patch.Scale
instrumentScale = Legong.makeInstrumentScale "selisir"
    [Key.c_1, Key.d_1, Key.e_1, Key.g_1, Key.a_1] -- i o e u a
