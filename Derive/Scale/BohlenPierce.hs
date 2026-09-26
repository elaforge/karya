-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.BohlenPierce where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import           Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Num as Num
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch

import           Global


scales :: [Scale.Definition]
scales = map Scale.Simple [absoluteScale]

scaleId :: Pitch.ScaleId
scaleId = "bp"

absoluteScale :: Scale.Scale
absoluteScale =
    (ChromaticScales.makeScale scaleId scaleMap $
        "Bohlen-Pierce is derived from 3 instead of 2, and thus has a\
        \ \"tritave\" instead of an octave.\n"
        <> ChromaticScales.twelveDoc)
    { Scale.scaleNoteToCall = noteToCall scaleMap
    , Scale.scaleInputToNn = Scales.computedInputToNn
        (ChromaticScales.inputToNote scaleMap) (noteToCall scaleMap)
    }
    -- TODO that's pretty awkward, ChromaticScales should provide a way to make
    -- a scale but specify my own pitch aspect.

scaleMap :: ChromaticScales.ScaleMap
scaleMap = (ChromaticScales.scaleMap layout absoluteFmt allKeys defaultKey)
    { ChromaticScales.smapSemisToNn = semisToNn }

pscale :: PSignal.Scale
pscale = Pitches.scale absoluteScale

absoluteFmt :: TheoryFormat.Format
absoluteFmt = TheoryFormat.makeAbsoluteFormat
        (TheoryFormat.makePattern degrees) degrees
    where
    degrees = TheoryFormat.makeDegrees
        ["a", "b", "c", "d", "e", "f", "g", "h", "i"]

noteToCall :: ChromaticScales.ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
noteToCall smap note =
    case TheoryFormat.readRelativePitch fmt note of
        Left _ -> ScaleDegree.scaleDegreeInterval pscale namedIntervals note
        Right pitch -> Just $ scaleDegree
            (ChromaticScales.pitchNn smap pitch)
            (ChromaticScales.pitchNote smap pitch)
    where fmt = ChromaticScales.smapFmt smap

-- TODO frac should always be 0, right?
semisToNn :: ChromaticScales.SemisToNoteNumber
semisToNn (PSignal.PitchConfig env controls) fsemi =
    pure $ Pitch.hzToNn $ Num.scale hz1 hz2 frac
    where
    hz1 = degreeToHz baseHz tonic degree
    hz2 = degreeToHz baseHz tonic (degree + 1)
    (degree, frac) = properFraction fsemi
    baseHz = Map.findWithDefault defaultBaseHz JustScales.justBaseControl
        controls
    tonic = Theory.degreeToSemis layout $ Theory.keyTonic key
    key = fromMaybe defaultKey $ do
        key <- Scales.environKey env
        Map.lookup key allKeys

degreeToHz :: Pitch.Hz -> Pitch.Semi -> Pitch.Semi -> Pitch.Hz
degreeToHz baseHz tonic semis = octBase * ratio
    where
    octBase = baseHz * 3 ^^ (octave - Pitch.middleOctave)
    ratio = indexMod justRatios (degree - tonic)
    (octave, degree) = semis `divMod` semisPerOctave

indexMod :: Vector.Vector a -> Int -> a
indexMod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

defaultBaseHz :: Pitch.Hz
defaultBaseHz = Pitch.middleCHz

scaleDegree :: Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scaleDegree = ScaleDegree.scaleDegreeJust pscale namedIntervals 1

-- **

-- | Keyboard layout where the lambda mode in A is analogous to C-major:
--
-- > A - B C D - E F - G H - I A
-- > 1   2 3 4   5 6   7 8   9 1
-- >
-- > │ █ │ │ █ │ █ │ █ │ │
-- > │ █ │ │ █ │ █ │ █ │ │
-- > │ │ │ │ │ │ │ │ │ │ │
-- > └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
-- >  1 2 3 4 5 6 7 8 9 A
-- >  A B C D E F G H I A
layout :: Theory.Layout
layout = Theory.layout lambdaIntervals

lambdaIntervals :: [Int]
lambdaIntervals = [2, 1, 1, 2, 1, 2, 1, 2, 1]

-- 1, 7, 11, or in lambda: A, E, G -- 1 5 7
justRatios :: Vector.Vector Double
justRatios = Vector.fromList (map realToFrac bpRatios)

bpRatios :: [Ratio.Rational]
bpRatios =
    [ 1, 27 % 25, 25 % 21, 9 % 7, 7 % 5, 75 % 49, 5 % 3
    , 9 % 5, 49 % 25, 15 % 7, 7 % 3, 63 % 25, 25 % 9
    ]

-- | Display scale degrees and the intervals between them.
_printIntervals :: Text
_printIntervals =
    Text.unwords $ interleave (map (("("<>) . (<>")")) degrees) intervals
    where
    interleave (x:xs) (y:ys) = x : y : interleave xs ys
    interleave xs [] = xs
    interleave [] ys = ys
    degrees = TheoryFormat.keyDegrees defaultKey absoluteFmt
    intervals = zipWith interval bpRatios (drop 1 bpRatios)
    interval low high = fromMaybe ("no interval: " <> showt (high/low)) $
        Map.lookup (high / low) names
    names = Map.fromList [(v, k) | (k, v) <- Map.toList namedIntervals]

-- | Display the keys and their signatures.
_printScales :: IO ()
_printScales = mapM_ (putStrLn . untxt) $
    map (TheoryFormat.showKeySignature absoluteFmt) $
    filter ((== Pitch.Degree 0 0) . Theory.keyTonic) $ Map.elems allKeys

pcPerOctave :: Pitch.PitchClass
pcPerOctave = length lambdaIntervals

semisPerOctave :: Int
semisPerOctave = Num.sum lambdaIntervals

allKeys :: ChromaticScales.Keys
allKeys = ChromaticScales.makeKeys absoluteFmt $
    concatMap (uncurry makeKeys) modes

defaultKey :: Theory.Key
Just defaultKey = Map.lookup (Pitch.Key "a-lambda") allKeys

-- | BP modes, as documented on <http://www.huygens-fokker.org/bpsite/>
--
-- The only difference is that I wrap the octave at A, rather than C.
--
-- This has keys rooted at every chromatic step, but BP modes rooted on
-- accidentals tend to wind up with crazy key signatures, e.g. with triple
-- sharps, probably due to the lack of a circle-of-fifths organization.
modes :: [(Text, [Pitch.Semi])]
modes = makeModes lambdaNames lambdaIntervals
    ++ makeModes gammaNames gammaIntervals
    where
    makeModes names intervals =
        [ (name, take pcPerOctave (drop n (cycle intervals)))
        | (n, name) <- zip [0..] names
        ]

-- | Lambda family, never has 2 whole steps in a row.
lambdaNames :: [Text]
lambdaNames =
    [ "lambda", "walker-a", "moll-2", "walker-1", "harmonic", "walker-2"
    , "dur-1", "moll-1", "walker-b"
    ]

-- | Gamma family.  The x modes are considered not very useful.
gammaNames :: [Text]
gammaNames = ["gamma", "x1", "x2", "x3", "x4", "x5", "x6", "dur-2", "x7"]
gammaIntervals :: [Int]
gammaIntervals = [1, 2, 1, 2, 1, 1, 2, 2, 1]

makeKeys :: Text -> [Pitch.Semi] -> [Theory.Key]
makeKeys name intervals =
    [Theory.key tonic name intervals layout
        | tonic <- allDegrees, abs (Pitch.degreeAccidentals tonic) <= 1]

allDegrees :: [Pitch.Degree]
allDegrees =
    [Pitch.Degree pc accs | pc <- [0 .. pcPerOctave - 1], accs <- [-1..1]]

namedIntervals :: ScaleDegree.NamedIntervals
namedIntervals = Map.fromList
    [ ("d", minor)
    , ("D", major)
    , ("m-2", small)
    , ("m2", small * minor)
    , ("M2", small * major)
    , ("M+2", small * minor * major)
    ]
    where
    small = 27 % 25
    minor = 245 % 243 -- minor diesis
    major = 3125 % 3087 -- major diesis
