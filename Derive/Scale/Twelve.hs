-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | The equal tempered 12 note scale, aka 12TET.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".  General to specific is more
    aesthetically appealing.

    4c is middle C, and the range is limited to the midi range.  Since
    'Pitch.NoteNumber's also use midi numbering, conversions are trivial.

    > nn 127 = 9g
    > nn 120 = 9c
    > middle c = nn 60 = 4c
    > nn 24 = 1c
    > nn 12 = 0c
    > nn 0 = -1c
-}
module Derive.Scale.Twelve (
    scales, scaleId
    -- * keys
    , lookupKey, defaultKey
    -- * utils
    , showNn, readAbsolutePitch, nnPitch

#ifdef TESTING
    , module Derive.Scale.Twelve
#endif
) where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = map Scale.Simple [scale, relativeScale, keyedScale]

scale :: Scale.Scale
scale = Scales.setDirectInputToNn $
    ChromaticScales.makeScale scaleId absoluteScaleMap $
        "The world-famous equal tempered twelve note scale.\n"
        <> ChromaticScales.twelveDoc

relativeScale :: Scale.Scale
relativeScale = ChromaticScales.makeScale "twelve-r" relativeScaleMap
    "This is 12TET, but spelled relative to the current key and mode."

keyedScale :: Scale.Scale
keyedScale = ChromaticScales.makeScale "twelve-k" keyedScaleMap
    "This variant treats accidentals like staff notation. If a pitch doesn't\
    \ have an accidental, it will inherit from the key signature. It thus\
    \ needs an explicit natural to cancel that out. Unlike staff notation,\
    \ accidentals don't persist until the next barline."

scaleId :: Pitch.ScaleId
scaleId = "twelve"

absoluteScaleMap :: ChromaticScales.ScaleMap
absoluteScaleMap =
    ChromaticScales.scaleMap layout absoluteFmt allKeys defaultTheoryKey

absoluteFmt :: TheoryFormat.Format
absoluteFmt = TheoryFormat.absoluteC

relativeScaleMap :: ChromaticScales.ScaleMap
relativeScaleMap =
    ChromaticScales.scaleMap layout (TheoryFormat.sargam fmt)
        allKeys defaultTheoryKey
    where fmt = ChromaticScales.relativeFmt defaultTheoryKey allKeys

keyedScaleMap :: ChromaticScales.ScaleMap
keyedScaleMap =
    ChromaticScales.scaleMap layout fmt allKeys defaultTheoryKey
    where
    fmt = TheoryFormat.makeAbsoluteFormatKeyed config keyConfig
        (TheoryFormat.makePattern degrees) degrees
    keyConfig = TheoryFormat.KeyConfig
        { keyParse = Scales.getKey defaultTheoryKey allKeys
        , keyDefault = defaultTheoryKey
        }
    degrees = TheoryFormat.makeDegrees TheoryFormat.absoluteCDegrees
    -- I'm worried that 'n' for natural looks ugly, so let's try the symbolic
    -- accidentals.
    config = TheoryFormat.defaultConfig
        { TheoryFormat.configAccidental = TheoryFormat.symbolAccidentals }

-- * keys

lookupKey :: Maybe Pitch.Key -> Maybe Theory.Key
lookupKey = Scales.lookupKey defaultTheoryKey allKeys

defaultKey :: Pitch.Key
defaultKey = Pitch.Key "c-maj"

defaultTheoryKey :: Theory.Key
Just defaultTheoryKey = Map.lookup defaultKey allKeys

showPitch :: Pitch.Pitch -> Maybe Pitch.Note
showPitch = either (const Nothing) Just
    . ChromaticScales.showPitch absoluteScaleMap Nothing

-- * utils

-- | Map NoteNumbers to their nearest Note.
showNn :: Pitch.NoteNumber -> Maybe Pitch.Note
showNn = showPitch . Theory.semisToPitchSharps layout
    . Theory.nnToSemis . round

readAbsolutePitch :: Pitch.Note -> Maybe Pitch.Pitch
readAbsolutePitch = either (const Nothing) Just
    . ChromaticScales.readPitch absoluteScaleMap Nothing

nnPitch :: Pitch.NoteNumber -> PSignal.Pitch
nnPitch nn = PSignal.constantPitch scaleId (fromMaybe "x" (showNn nn)) nn

-- * implementation

allKeys :: Map Pitch.Key Theory.Key
allKeys = ChromaticScales.makeKeys absoluteFmt $
    churchKeys ++ octatonicKeys ++ wholeKeys ++ exoticKeys

churchKeys :: [Theory.Key]
churchKeys = concat (zipWith makeKeys modes intervals)
    where
    modes = ["maj", "dorian", "phrygian", "lydian", "mixolydian", "min",
        "locrian"]
    intervals = [take 7 (drop n major) | n <- [0..6]]
    major = cycle $ Vector.toList (Theory.layoutIntervals layout)

octatonicKeys :: [Theory.Key]
octatonicKeys = makeKeys "octa21" (take 8 (cycle [2, 1]))
    ++ makeKeys "octa12" (take 8 (cycle [1, 2]))

wholeKeys :: [Theory.Key]
wholeKeys = makeKeys "whole" (replicate 6 2)

-- | Keys that are diatonic, but have nonstandard key signatures.
exoticKeys :: [Theory.Key]
exoticKeys = makeKeys "hijaz" [1, 3, 1, 2, 1, 2, 2]

-- | The layout of keys on everyone's favorite boxed harp.
layout :: Theory.Layout
layout = Theory.pianoLayout

makeKeys :: Text -> [Pitch.Semi] -> [Theory.Key]
makeKeys name intervals =
    [Theory.key tonic name intervals layout | tonic <- allDegrees]
    where
    allDegrees = [Pitch.Degree pc accs | pc <- [0..6], accs <- [-1..1]]
