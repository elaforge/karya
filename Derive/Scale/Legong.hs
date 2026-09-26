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
module Derive.Scale.Legong (
    scales
    , calung, jegog, kantilan, pemade
    , penyacah
    , makeInstrumentScale
    , makeScaleSet
    , completeInstrumentScale
    , rambatRange
    , reyongRange
    , trompongRange
    , ugalRange
    , laras
    , lowPitch, highPitch
    , larasRambat
) where
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
scales = makeScaleSet config scaleId "Saih pelegongan, from my instruments."

scaleId :: Pitch.ScaleId
scaleId = "legong"

makeScaleSet :: BaliScales.Config -> Pitch.ScaleId -> Doc.Doc
    -> [Scale.Definition]
makeScaleSet config (Pitch.ScaleId prefix) doc =
    map (Scale.Simple . Scales.addDoc doc)
    [ BaliScales.makeScale (idWith "") (scaleMap completeScale)
    , BaliScales.makeScale (idWith "b") (scaleMap completeScaleBalinese)
    , Scales.addDoc "Use Javanese-style cipher notation." $
        -- TODO use 4 and 7 instead of 3# and 6#.
        -- Use simple_scale like *wayang and *selesir?
        BaliScales.makeScale (idWith "c") (scaleMap cipherScale)
    , instDoc "pemade" $ BaliScales.makeScale (idWith "pemade")
        (instScaleMap pemade)
    , instDoc "pemade" $ BaliScales.makeScale (idWith "pemade-b")
        (instScaleMap (balineseNotation pemade))
    , instDoc "kantilan" $ BaliScales.makeScale (idWith "kantilan")
        (instScaleMap kantilan)
    , instDoc "kantilan" $ BaliScales.makeScale (idWith "kantilan-b")
        (instScaleMap (balineseNotation kantilan))
    ]
    where
    idWith suffix = Pitch.ScaleId $ Texts.join2 "-" prefix suffix
    instDoc name = Scales.addDoc $
        "This is centered around the " <> name <> " range."
    scaleMap fmt = BaliScales.scaleMap config fmt Nothing
    instScaleMap = BaliScales.instrumentScaleMap config

    completeScale = BaliScales.ioeuaRelative True
        (BaliScales.configDefaultKey config) (BaliScales.configKeys config)
    completeScaleBalinese =
        BaliScales.digitOctaveRelative BaliScales.balinese True
            (BaliScales.configDefaultKey config)
            (BaliScales.configKeys config)
    cipherScale = BaliScales.cipherRelativeDotted 5
        (BaliScales.configDefaultKey config)
        (BaliScales.configKeys config)

jegog, calung, penyacah :: BaliScales.Instrument
jegog = instrument 1 (Pitch.pitch 3 I) (Pitch.pitch 3 As)
calung = instrument 2 (Pitch.pitch 4 I) (Pitch.pitch 4 As)
penyacah = instrument 3 (Pitch.pitch 5 0) (Pitch.pitch 5 As)

pemade, kantilan :: BaliScales.Instrument
pemade = instrument 5 (Pitch.pitch 4 O) (Pitch.pitch 6 I)
kantilan = instrument 6 (Pitch.pitch 5 O) (Pitch.pitch 7 I)

instrument :: Pitch.Octave -> Pitch.Pitch -> Pitch.Pitch
    -> BaliScales.Instrument
instrument = BaliScales.Instrument BaliScales.ioeua BaliScales.arrowOctaves

-- * config

balineseNotation :: BaliScales.Instrument -> BaliScales.Instrument
balineseNotation inst = inst
    { BaliScales.instDegrees = BaliScales.balinese
    , BaliScales.instRelativeOctaves = BaliScales.balineseOctaves
    }

-- | These are from Tenzer's "Gamelan Gong Kebyar", page 29.  This is Dewa
-- Beratha's definition.  McPhee's book has different names for gambuh, but
-- Beratha's is probably more modern.
--
-- These are assigned with @key=...@.  McPhee calls them tekepan (suling) or
-- ambah.  Or I could use patutan / pathet.
config :: BaliScales.Config
config = BaliScales.Config
    { configLayout = layout
    , configKeys = allKeys
    , configDefaultKey = defaultKey
    , configLaras = laras
    , configDefaultLaras = larasRambat
    }
    where
    layout = Theory.layout intervals
    Just defaultKey = Map.lookup (Pitch.Key "selisir") allKeys

intervals :: [Pitch.Semi]
intervals = [1, 1, 2, 1, 2]

allKeys :: ChromaticScales.Keys
allKeys = Map.fromList $ zipWith make [0..]
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
            (BaliScales.configLayout config)
        )

ugalRange, rambatRange, trompongRange, reyongRange :: Scale.Range
ugalRange = Scale.Range (Pitch.pitch 3 O) (Pitch.pitch 5 I)
rambatRange = Scale.Range (Pitch.pitch 3 E) (Pitch.pitch 6 I)
trompongRange = Scale.Range (Pitch.pitch 3 A) (Pitch.pitch 5 U)
reyongRange = Scale.Range (Pitch.pitch 4 E) (Pitch.pitch 6 U)

-- * laras

data Pitch = I | O | E | Es | U | A | As
    deriving (Eq, Ord, Enum, Show, Bounded)

laras :: Map Text BaliScales.Laras
laras = Map.fromList $ Lists.keyOn BaliScales.larasName $
    larasRambat : larasSaba : larasPeteSps7 : mcphee

larasRambat :: BaliScales.Laras
larasRambat = BaliScales.laras "rambat" lowPitch (extend 3 E)
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
    i = Pitch.addHz 4
    u = Pitch.addHz (-4)

-- | Document measurements for different instruments, presumably in the future
-- these are built in to the samples?
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

-- | From legong saba recordings.
larasSaba :: BaliScales.Laras
larasSaba = BaliScales.laras "saba" lowPitch (extend 3 E)
    "Tuning from Legong Bapang Saba recordings."
    (map (\nn -> (nn, Pitch.addHz 4 nn)) legongSaba)

legongSaba :: [Pitch.NoteNumber]
legongSaba =
    [ 56.30     -- 3e
    , 56.30 -- fake 4
    , 60.24
    , 60.94
    , 60.94 -- fake 7
    , 65.00     -- 4i
    , 66.15
    , 68.20
    , 68.20 -- fake 4
    , 72.31
    , 73.40
    ]

larasPeteSps7 :: BaliScales.Laras
larasPeteSps7 = BaliScales.laras "pete-sps7" lowPitch (extend 4 I)
    "Pete's Semar Pegulingan saih pitu"
    (map (\nn -> (nn, Pitch.addHz 4 nn)) peteSps7)

peteSps7 :: [Pitch.NoteNumber]
peteSps7 =
    [ 61.00 -- 1 ??
    , 62.60 -- 2 o
    , 64.00 -- 3 e
    , 67.00 -- 4 ??
    , 68.27 -- 5 u
    , 69.40 -- 6 a
    , 71.21 -- 7 a#
    ]

-- | Extend down to 3i, which is jegog range.
extend :: Pitch.Octave -> Pitch -> [Pitch.NoteNumber] -> [Pitch.NoteNumber]
extend oct pc = Bali.extendScale 7 lowPitch highPitch (Pitch.pitch oct pc)

-- | Lowest note starts on this octave.
baseOctave :: Pitch.Octave
baseOctave = 3

lowPitch, highPitch :: Pitch.Pitch
lowPitch = Pitch.pitch baseOctave I
highPitch = Pitch.pitch 7 I

mcphee :: [BaliScales.Laras]
mcphee = map (make . McPhee.extract lowPitch highPitch) McPhee.saihPitu
    where
    make (name, (nns, doc)) =
        BaliScales.laras name lowPitch id doc
            (map (\nn -> (nn, nn)) nns)

-- * instrument integration

-- | A Scale with the entire theoretical range.  This is for instruments
-- that are normalized to 12tet and then tuned in the patch (e.g. using KSP).
completeInstrumentScale :: BaliScales.Laras -> BaliScales.Tuning
    -> Patch.Scale
completeInstrumentScale = instrumentScale id

instrumentScale ::
    ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Laras -> BaliScales.Tuning -> Patch.Scale
instrumentScale =
    makeInstrumentScale "legong" -- i o e e# u a a#
        [Key.c_1, Key.d_1, Key.e_1, Key.f_1, Key.g_1, Key.a_1, Key.b_1]

makeInstrumentScale :: Text -> [Midi.Key]
    -> ([(Midi.Key, Pitch.NoteNumber)] -> [(Midi.Key, Pitch.NoteNumber)])
    -- ^ drop and take keys for the instrument's range
    -> BaliScales.Laras -> BaliScales.Tuning -> Patch.Scale
makeInstrumentScale name keys takeRange laras tuning =
    Patch.makeScale (name <> "-" <> ShowVal.showVal tuning) $
        takeRange $ zip (midiKeys keys) (Vector.toList nns)
    where
    nns = case tuning of
        BaliScales.Umbang -> BaliScales.larasUmbang laras
        BaliScales.Isep -> BaliScales.larasIsep laras

-- | Emit from i3 on up.
midiKeys :: [Midi.Key] -> [Midi.Key]
midiKeys keys = trim $ concatMap octaveKeys [baseOctave + 1 ..]
    -- base_octave + 1 because MIDI starts at octave -1
    where
    trim = take (5 * length keys + 1)
    octaveKeys oct = map (Midi.toKey (oct * 12) +) keys
