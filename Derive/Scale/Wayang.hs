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
scales = map (Scale.Simple . Scales.addDoc "Saih gender wayang.")
    [ BaliScales.makeScale scaleId $
        BaliScales.scaleMap (config larasSawan)
            BaliScales.ioeuaAbsolute Nothing
    , BaliScales.makeScale "wayang-a" $
        BaliScales.scaleMap (rebase (Pitch.pitch 1 U) (config larasSawan))
            BaliScales.ioeuaAbsolute Nothing
    , BaliScales.makeScale "wayang-srg" $
        BaliScales.scaleMap (rebase (Pitch.pitch 1 U) (config larasSawan))
            sargamAbsolute Nothing
    , Scales.addDoc
        "Pemade scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.makeScale "wayang-pemade"
            (instScaleMap larasSawanPemade pemade)
    , Scales.addDoc
        "Kantilan scale. This can be used to give the the same score to both\
            \ pemade and kantilan." $
        BaliScales.makeScale "wayang-kantilan"
            (instScaleMap larasSawanKantilan kantilan)
    ]
    where
    instScaleMap laras = BaliScales.instrumentScaleMap (config laras)

sargamAbsolute :: TheoryFormat.Format
sargamAbsolute = TheoryFormat.makeAbsoluteFormat "[1-9][srgpd]" $
    TheoryFormat.makeDegrees $ map Text.singleton "srgpd"

pemade :: BaliScales.Instrument
pemade = instrument 4 (Pitch.pitch 3 O) (Pitch.pitch 5 I)

kantilan :: BaliScales.Instrument
kantilan = instrument 5 (Pitch.pitch 4 O) (Pitch.pitch 6 I)

instrument :: Pitch.Octave -> Pitch.Pitch -> Pitch.Pitch
    -> BaliScales.Instrument
instrument = BaliScales.Instrument BaliScales.ioeua BaliScales.arrowOctaves

config :: BaliScales.Laras -> BaliScales.Config
config defaultLaras = BaliScales.Config
    { configLayout = layout
    , configKeys = mempty
    , configDefaultKey = defaultKey
    , configLaras = laras
    , configDefaultLaras = defaultLaras
    }
    where
    layout = Theory.diatonicLayout 5
    defaultKey = Theory.key (Pitch.Degree 0 0) "default" (replicate 5 1) layout

rebase :: Pitch.Pitch -> BaliScales.Config -> BaliScales.Config
rebase base config = config
    { BaliScales.configLaras = set <$> BaliScales.configLaras config
    , BaliScales.configDefaultLaras =
        set $ BaliScales.configDefaultLaras config
    }
    where
    set laras = laras { BaliScales.larasBase = base }

-- | Start octave for the extended scale.
baseOct :: Pitch.Octave
baseOct = 1

scaleId :: Pitch.ScaleId
scaleId = "wayang"

-- * laras

data Pitch = I | O | E | U | A deriving (Eq, Enum, Show)

laras :: Map Text BaliScales.Laras
laras = Map.fromList $ Lists.keyOn BaliScales.larasName $
    larasSawan
    : larasSawanPemade
    : larasSawanKantilan
    : mcphee

larasSawan :: BaliScales.Laras
larasSawan = BaliScales.laras "sawan" (Pitch.pitch 1 I)
    (extend (BaliScales.instLow pemade))
    "Tuning from my gender wayang, made in Sawan, Singaraja." $
    -- Of course the overlapping parts of the scales are a bit different, and
    -- I have to pick one.  So I choose the pemade version.
    BaliScales.larasNns larasSawanPemade
    ++ drop 5 (BaliScales.larasNns larasSawanKantilan)

larasSawanPemade :: BaliScales.Laras
larasSawanPemade = BaliScales.laras "sawan-pemade" (Pitch.pitch 3 1) id
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

larasSawanKantilan :: BaliScales.Laras
larasSawanKantilan = BaliScales.laras "sawan-kantilan" (Pitch.pitch 4 1) id
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
mcphee = map (make . McPhee.extract lowPitch highPitch) McPhee.slendro
    where
    make (name, (nns, doc)) =
        BaliScales.laras name (Pitch.pitch 1 0) id doc
            (map (\nn -> (nn, nn)) nns)

-- | Extend down two octaves so that I start at 1i, and up two octaves to 8i.
--
-- pemade starts at 3o - 4i - 5i, kanti is 4o - 5i - 6i
extend :: Pitch.Pitch -> [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend from = Bali.extendScale 5 lowPitch highPitch from

lowPitch, highPitch :: Pitch.Pitch
lowPitch = Pitch.pitch 1 I
highPitch = Pitch.pitch 8 I

undoExtend :: [a] -> [a]
undoExtend = take 15 . drop (1 + 5 + 5)
    -- take (kantilan_high - kantilan_low) . drop (pemade_low - 1 I)

-- * instrument integration

instrumentScale :: Bool -> BaliScales.Laras -> BaliScales.Tuning -> Patch.Scale
instrumentScale extended laras tuning =
    Patch.makeScale ("wayang-" <> ShowVal.showVal tuning) $
        zip (midiKeys extended)
            ((if extended then id else undoExtend) (Vector.toList nns))
    where
    nns = case tuning of
        BaliScales.Umbang -> BaliScales.larasUmbang laras
        BaliScales.Isep -> BaliScales.larasIsep laras

-- | If extended is True, emit from i1 on up.  Otherwise, give pemade to
-- kantilan range.
midiKeys :: Bool -> [Midi.Key]
midiKeys extended = trim $ concatMap keys [baseOct + 1 ..]
    -- base_oct + 1 because MIDI starts at octave -1
    where
    trim
        | extended = take (7*5 + 1)
        | otherwise = take (3*5) . drop (1 + 3*5)
    keys oct = map (Midi.toKey (oct * 12) +) -- i o e u a
        [Key2.e_2, Key2.f_2, Key2.a_2, Key2.b_2, Key2.c_1]
