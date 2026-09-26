-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Octa where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch


scales :: [Scale.Definition]
scales = map Scale.Simple
    [ makeScale "octa21" layout21 keys21 absoluteFmt
    , makeScale "octa12" layout12 keys12 absoluteFmt
    , makeScale "octa21-r" layout21 keys21 (relativeFmt keys21)
    , makeScale "octa12-r" layout12 keys12 (relativeFmt keys12)
    ]
    where
    layout21 = Theory.layout [2, 1, 2, 1, 2, 1, 2, 1]
    layout12 = Theory.layout [1, 2, 1, 2, 1, 2, 1, 2]
    keys21 = allKeys layout21
    keys12 = allKeys layout12

absoluteFmt :: TheoryFormat.Format
absoluteFmt =
    TheoryFormat.makeAbsoluteFormat (TheoryFormat.makePattern degrees)
        degrees
    where
    degrees = TheoryFormat.makeDegrees ["a", "b", "c", "d", "e", "f", "g", "h"]

relativeFmt :: ChromaticScales.Keys -> TheoryFormat.Format
relativeFmt keys = make $ TheoryFormat.RelativeFormat
    { relConfig = TheoryFormat.defaultConfig
    , relKeyConfig = TheoryFormat.KeyConfig
        { keyParse = parseKey
        , keyDefault = defaultTheoryKey
        }
    , relShowDegree = TheoryFormat.showDegreeChromatic
    , relToAbsolute = TheoryFormat.chromaticToAbsolute
    }
    where
    parseKey = Scales.getKey defaultTheoryKey keys
    Just defaultTheoryKey = Map.lookup defaultKey keys
    make = TheoryFormat.makeRelativeFormat (TheoryFormat.makePattern degrees)
        degrees
    degrees = TheoryFormat.makeDegrees
        ["一", "二", "三", "四", "五", "六", "七", "八"]

makeScale :: Pitch.ScaleId -> Theory.Layout -> ChromaticScales.Keys
    -> TheoryFormat.Format -> Scale.Scale
makeScale scaleId layout keys fmt =
    ChromaticScales.makeScale scaleId scaleMap doc
    where
    scaleMap = ChromaticScales.scaleMap layout fmt keys defaultTheoryKey
    Just defaultTheoryKey = Map.lookup defaultKey keys
    doc = "Octatonic scales as true 8 note scales, using notes from a-h.\
        \ There are two variants: octa21 starts with a whole step, while\
        \ octa12 starts with a half-step.\n"
        <> ChromaticScales.twelveDoc

defaultKey :: Pitch.Key
defaultKey = Pitch.Key "a"

allDegrees :: [Pitch.Degree]
allDegrees = [Pitch.Degree pc accs | pc <- [0..7], accs <- [-1..1]]

makeKeys :: Theory.Layout -> [Pitch.Semi] -> [Theory.Key]
makeKeys layout intervals =
    [Theory.key tonic "" intervals layout
        | tonic <- allDegrees, abs (Pitch.degreeAccidentals tonic) <= 1]

allKeys :: Theory.Layout -> ChromaticScales.Keys
allKeys layout =
    Map.fromList $ zip (map (TheoryFormat.showKey absoluteFmt) keys) keys
    where
    keys = makeKeys layout $ Vector.toList (Theory.layoutIntervals layout)
