-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Guitar where
import           Prelude hiding (String)
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Perform.Pitch as Pitch

import           Global
import           Ness.Global


steel   = Material 7860   200e9 -- 8050 kg/m^3, 200 GPa
gold    = Material 19300  79e9 -- 19320
uranium = Material 19050  208e9
nylon   = Material 1150   3e9 -- 1.15 g/m^3, 2--4 GPa
hemp    = Material 860    35e9 -- .86 g/m^3, 35 GPa
bronze  = Material 8000   105e9 -- 7400 - 8900, 96--120 GPa
silk    = Material 1300   200e9 -- spider silk: 40--280 GPa as strain increases

renderAll :: SamplingRate -> (Instrument, Score) -> (Text, Text)
renderAll sr (instrument, score) =
    (renderInstrument sr instrument, renderScore (iStrings instrument) score)

verify :: Instrument -> [Text]
verify instrument = map ("duplicate string names: "<>) $ map fst $ snd $
    Lists.partitionDups id $ map sName $ iStrings instrument

-- * Instrument

{- | string_def (array) defines the parameters for each string of the guitar.
    This is a 2-dimensional array with a row for each string. Each row
    contains 7 items: length in metres, Young’s modulus, tension, radius,
    density, T60 at 0Hz, T60 at 1000Hz.
-}
data String = String {
    sLength :: Meters
    , sTension :: Newtons
    , sMaterial :: Material
    , sRadius :: Meters
    , sT60 :: (Double, Double) -- -60db at 0hz and 1k
    -- | This is the logical pitch, in order to select a string, which is
    -- different from the actual pitch.  The actual pitch is calculated with
    -- 'sActualNn'.
    , sNn :: Pitch.NoteNumber
    , sName :: StringName
    , sOutputs :: [Output]
    } deriving (Eq, Ord, Show)
type StringName = Text

data Material = Material {
    mDensity, mYoung :: Double
    } deriving (Eq, Ord, Show)

renderStrings :: [String] -> Text
renderStrings = array2 "string_def" . map list
    where
    list (String len tension (Material density young) radius (t600, t601)
            _ _ _) =
        [len, young, tension, radius, density, t600, t601]

{- | output_def (array) defines the locations of the outputs. This is
    a 2-dimensional array with a row for each output. Each row contains
    2 items: the index of the string from which the output should be taken
    (1-based), and the distance along the string (normalised to the range
    0-1).
-}
data Output = Output {
    oLocation :: Location
    , oPan :: Pan
    } deriving (Eq, Ord, Show)

renderOutputs :: [(StringIndex, Output)] -> Text
renderOutputs = array2 "output_def" . map list
    where list (i, (Output location _pan)) = [fromIntegral i, location]

{- | backboard (array) defines the shape of the backboard. This is
    a 3 element array; the elements (which should be negative) define
    a quadratic function describing the shape of the backboard.

    Where x is length: a + bx + bx^2
    All should be negative or zero (i.e., the backboard is under the strings).
    E.g. -0.001 -0.000 -0.0002
-}
data Backboard = Backboard {
    ba :: Double
    , bb :: Double
    , bc :: Double
    } deriving (Eq, Ord, Show)

renderBackboard :: Backboard -> Text
renderBackboard (Backboard b1 b2 b3) = array "backboard" [b1, b2, b3]

data Fret = Fret {
    fHeight :: Meters -- negative
    , fLocation :: Location
    } deriving (Eq, Ord, Show)

renderFrets :: [Fret] -> Text
renderFrets = array2 "frets" . map list
    where list (Fret height loc) = [loc, height]

{- | barrier_params_def (array) specifies 5 basic parameters for the barrier
    (fret and backboard) collisions. The parameters are: K, alpha, beta, number
    of iterations for Newton solver, and tolerance for Newton solver.

    E.g. 1e10 1.3 10
-}
data Barrier = Barrier {
    -- | stiffness (normally a high number, like 1e10, or 1e13
    bK :: Double
    -- | stiffness exponent (small number, usually between 1 and 3)
    , bAlpha :: Double
    -- | loss parameter (positive or zero...bigger means more loss)
    , bBeta :: Double
    , bSolver :: Solver
    } deriving (Eq, Ord, Show)

renderBarrier :: Barrier -> Text
renderBarrier (Barrier k alpha beta (Solver iterations tolerance)) =
    array "barrier_params_def"
        [k, alpha, beta, fromIntegral iterations, tolerance]

data FingerParams = FingerParams {
    fMass :: Kg
    -- | Stiffness.  A big number, like 1e7, usually... tells you the hardness
    -- of the finger.
    , fStiffness :: Double
    -- | Exponent, should be between 1-3.
    , fExponent :: Double
    -- | Loss.  0 means lossless, greater than zero, means lossy. Usually 1-100
    -- are good values.
    , fLoss :: Double
    } deriving (Eq, Ord, Show)

renderFingerParams :: FingerParams -> Text
renderFingerParams (FingerParams mass k alpha beta) =
    array "finger_params" [mass, k, alpha, beta]

data Solver = Solver {
    nIterations :: Int
    , nTolerance :: Double
    } deriving (Eq, Ord, Show)

-- | 1-based, I think.
type StringIndex = Int

{- | ssconnect_def (array) defines the parameters for each connection between
    strings. This is a 2-dimensional array with a row for each connection. Each
    row contains 9 items: the mass, frequency, loss parameter, collision
    exponent, rattling distance, index of first string, connection point on
    first string (0-1), index of second string, connection point on second
    string. If the second string index is 0, the connection is to a single
    string only. If multiple connections are defined connecting to the same
    point on a string, the latter one will be automatically removed as this is
    not currently supported.
-}
data Connection = Connection {
    cMass :: Double
    , cFrequency :: Double
    , cLoss :: Double
    , cCollisionExponent :: Double
    , cRattlingDistance :: Meters
    , cString1 :: (StringIndex, Location)
    , cString2 :: (StringIndex, Location)
    } deriving (Eq, Ord, Show)

renderConnections :: [Connection] -> Text
renderConnections = array2 "ssconnect_def" . map list
    where
    list (Connection mass freq loss collision rattle (s1, s1Loc) (s2, s2Loc)) =
        [ mass, freq, loss, collision, rattle
        , fromIntegral s1, s1Loc, fromIntegral s2, s2Loc
        ]

data Instrument = Instrument {
    iName :: Text
    , iStrings :: [String]
    , iFrets :: [Fret]
    , iBarrier :: Barrier
    , iBackboard :: Backboard
    , iFingerParams :: FingerParams
    , iNormalizeOutputs :: Bool
    , iSolver :: Solver
    , iConnections :: [Connection]
    } deriving (Eq, Ord, Show)

renderInstrument :: SamplingRate -> Instrument -> Text
renderInstrument sr (Instrument name strings frets barrier backboard
        fingerParams normalizeOutputs solver connections) = Text.unlines
    [ "% gtversion 1.0"
    , "% name: " <> name
    , scalar "SR" sr
    , renderStrings strings
    , renderOutputs [(i, o) | (i, string) <- byIndex, o <- sOutputs string]
    , scalar "itnum" (nIterations solver)
    , scalar "normalize_outputs" normalizeOutputs
    , array "pan" (map oPan (concatMap sOutputs strings))
    , renderBackboard backboard
    , renderFrets frets
    , renderBarrier barrier
    , renderFingerParams fingerParams
    , renderConnections connections
    ]
    where
    byIndex = zip [1..] strings

-- * Score

data Score = Score {
    sDecay :: Seconds
    , sHighpass :: Bool
    , sNotes :: [Note]
    , sFingers :: [Finger]
    }
    deriving (Eq, Show)

renderScore :: [String] -> Score -> Text
renderScore strings (Score decay highpass notes fingers) = Text.unlines
    [ scalar "Tf" duration
    , scalar "highpass" highpass
    , renderNotes indexOf notes
    , renderFingers indexOf fingers
    ]
    where
    duration = fromMaybe 0 (Lists.maximum (map nStart notes)) + decay
    indexOf str = fromMaybe (error $ "no string: " <> show str) $
        Map.lookup str toNum
        where toNum = Map.fromList $ zip (map sName strings) [1..]

data Note = Note {
    nStrike :: Strike
    , nString :: StringName
    , nStart :: Seconds
    , nDuration :: Seconds
    , nLocation :: Location
    , nAmplitude :: Newtons
    } deriving (Eq, Show)

renderNotes :: (StringName -> StringIndex) -> [Note] -> Text
renderNotes indexOf = array2 "exc" . map list
    where
    list (Note strike string start dur loc amp) =
        [fromIntegral (indexOf string), start, loc, dur, amp, fromStrike strike]
    fromStrike Strike = 0
    fromStrike Pluck = 1

data Strike = Strike | Pluck deriving (Eq, Show)

data Finger = Finger {
    fString :: StringName
    , fInitial :: (Location, Velocity)
    , fMovement :: [(Seconds, Location, Newtons)]
    } deriving (Eq, Show)
    -- finger def (cell array) defines all of the fingers in the simulation,
    -- their movements and the forces associated with them. This is
    -- a 2-dimensional cell array. Each row represents one finger and consists
    -- of 3 elements: the index of the string that the finger is on;
    -- a 2-dimensional array defining how the finger position and force changes
    -- over time; and a two element array specifying the finger’s initial
    -- position and velocity. Each row of the middle element contains a time
    -- (in seconds), a position and a force. The position and force are
    -- interpolated between the times given.

renderFingers :: (StringName -> StringIndex) -> [Finger] -> Text
renderFingers _ [] = ""
renderFingers indexOf fingers = Text.unlines
    [ "finger_def = {"
    , Text.intercalate ";\n" (map (("  "<>) . rFinger) fingers)
    , "};"
    ]
    where
    rFinger (Finger str (initP, initV) movement) = Text.intercalate ", "
        [ render (indexOf str)
        , "[" <> Text.intercalate "; " (map bp movement) <> "]"
        , "[" <> render initP <> ", " <> render initV <> "]"
        ]
    bp (sec, p, v) = Text.unwords $ map render [sec, p, v]

-- * render util

array :: Text -> [Double] -> Text
array name array =
    name <> " = [" <> Text.intercalate ", " (map render array) <> "];"

array2 :: Text -> [[Double]] -> Text
array2 name array = mconcat
    [ name <> " = ["
    , Text.intercalate ";\n  " [Text.unwords (map render xs) | xs <- array]
    , "];"
    ]

scalar :: Render a => Text -> a -> Text
scalar name x = name <> " = " <> render x <> ";"

-- * instrument util

makeFrets :: Meters -> Pitch.NoteNumber -> [Pitch.NoteNumber] -> [Fret]
makeFrets height open nns =
    map (Fret height) (map (pitchLocation open) nns)

pitchLocation :: Pitch.NoteNumber -> Pitch.NoteNumber -> Location
pitchLocation f0 f = 1 - 1 / (Pitch.nn_to_hz f / Pitch.nn_to_hz f0)

sActualNn :: String -> Pitch.NoteNumber
sActualNn s = Pitch.hz_to_nn $ sqrt (sTension s / mu) / (2 * sLength s)
    where mu = pi * sRadius s ^ 2 * mDensity (sMaterial s)

freq harmonic = sqrt (tension / (density * area)) * (harmonic / (2 * length))
    where
    length = 0.648 -- about the same
    tension = tensionKgToN 7.36 -- 72.13 n, but model uses 49.2
    density = 7500
    area = hexArea (guage / 2)
    guage = 0.254 / 1000 -- about the same

tensionKgToN = (*g)
    where g = 9.8

hexArea r = (3 * sqrt 3) / 2 * r^2

-- I get 155, which is much lower than freq 1, at 369.
freqStiffness harmonic = sqrt $
    (tension / (density * area)) * (harmonic / 2 * length)^2
    + ((young * moi) / (density * area))
        * ((harmonic^2 * pi) / (2 * length^2))^2
    where
    young = steelYoung
    steelYoung = 200e9
    -- cross-sectional moment of inertia
    moi = moiHex radius

    length = 0.648 -- about the same
    tension = tensionKgToN 7.36 -- 72.13 n, but model uses 49.2
    density = 7500
    area = hexArea radius
    radius = guage / 2
    guage = 0.254 / 1000 -- about the same

moiCircle :: Double -> Double
moiCircle r = (pi / 4) * r^4

moiHex :: Double -> Double
moiHex r = (5 * sqrt 3) / 16 * r^4

-- rho = density

-- massOf len radius density = pi * radius^2 * len * density
--
-- fOf t mass len = sqrt (t / (mass / len)) / (2 * len)
