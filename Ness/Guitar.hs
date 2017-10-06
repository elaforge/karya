module Ness.Guitar where
import Prelude hiding (String)
import qualified Data.Map as Map
import qualified Data.Text as Text

import Global
import Ness.Global
import qualified Ness.Util as Util


{-
% 1 - steel, 2 - gold, 3 - uranium
mat_tab = [7850 2e11;19300 7.9e10;
           19050 2.08e11];
-}
{-
    Questions:

    What is T60?  What does T60 mean?  Also I assume T60 at DC is the same as
    T60 at 0hz?

    For soundboard and guitar, is it possible to stop ringing strings?  They
    seem to have only two hardcoded techniques, "strike" and "pluck", with
    control over the duration, but apparently no ability to just damp the
    string.

    In the guitar instrument definition:
    . "itnum" is presumably iterations for a newton solver... which does what?
    . I guess "tol" would have been its tolerance, but isn't used for the C++
    version?
    . What does inharmonicity as taken by the string_def_gen() actually
    influence in the string_defs array?  Presumably density or radius?  Can
    I see the definitions for the various functions?
-}

run :: Instrument -> Score -> IO FilePath
run instrument score = Util.run "guitar" i s
    where (i, s) = renderAll instrument score

play :: IO ()
play = Util.play "guitar"

-- * Instrument

{- | string_def (array) defines the parameters for each string of the guitar.
    This is a 2-dimensional array with a row for each string. Each row
    contains 7 items: length in metres, Young’s modulus, tension, radius,
    density, T60 at 0Hz, T60 at 1000Hz.
-}
data String = String {
    sLength :: Meters
    , sYoung :: Double
    , sTension :: Newtons
    , sRadius :: Meters
    , sDensity :: Double -- kg/m?
    , sT60At0hz :: Double -- ?
    , sT60At1Khz :: Double -- ?
    , sOutputs :: [Output]
    } deriving (Eq, Ord, Show)

renderStrings :: [String] -> Text
renderStrings = array2 "string_def" . map list
    where
    list (String len young tension radius density t600 t601 _) =
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
-}
data Backboard = Backboard {
    b1 :: Double -- ?
    , b2 :: Double
    , b3 :: Double
    } deriving (Eq, Show)

renderBackboard :: Backboard -> Text
renderBackboard (Backboard b1 b2 b3) = array "backboard" [b1, b2, b3]

data Fret = Fret {
    fLocation :: Location
    , fHeight :: Meters -- negative
    } deriving (Eq, Show)

renderFrets :: [Fret] -> Text
renderFrets = array2 "frets" . map list
    where list (Fret loc height) = [loc, height]

{- | barrier_params_def (array) specifies 5 basic parameters for the barrier
    (fret and backboard) collisions. The parameters are: K, alpha, beta, number
    of iterations for Newton solver, and tolerance for Newton solver.
-}
data Barrier = Barrier {
    bK :: Double
    , bAlpha :: Double
    , bBeta :: Double
    , bSolver :: Solver
    } deriving (Eq, Show)

renderBarrier :: Barrier -> Text
renderBarrier (Barrier k alpha beta (Solver iterations tolerance)) =
    array "barrier_params_def"
        [k, alpha, beta, fromIntegral iterations, tolerance]

data FingerParams = FingerParams {
    fMass :: Double -- kg?
    , fK :: Double
    , fAlpha :: Double
    , fBeta :: Double
    } deriving (Eq, Show)

renderFingerParams :: FingerParams -> Text
renderFingerParams (FingerParams mass k alpha beta) =
    array "finger_params" [mass, k, alpha, beta]

data Solver = Solver {
    nIterations :: Int
    , nTolerance :: Double
    } deriving (Eq, Show)

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
    } deriving (Eq, Show)

renderConnections :: [Connection] -> Text
renderConnections = array2 "ssconnect_def" . map list
    where
    list (Connection mass freq loss collision rattle (s1, s1Loc) (s2, s2Loc)) =
        [ mass, freq, loss, collision, rattle
        , fromIntegral s1, s1Loc, fromIntegral s2, s2Loc
        ]

data Instrument = Instrument {
    iStrings :: [String]
    , iFrets :: [Fret]
    , iBarrier :: Barrier
    , iBackboard :: Backboard
    , iFingerParams :: FingerParams
    , iNormalizeOutputs :: Bool
    , iSolver :: Solver
    , iConnections :: [Connection]
    } deriving (Eq, Show)

renderInstrument :: Instrument -> Text
renderInstrument (Instrument strings frets barrier backboard fingerParams
        normalizeOutputs solver connections) = Text.unlines
    [ "% gtversion 1.0"
    , "SR = 44100;"
    , ""
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

data Note = Note {
    nStrike :: Strike
    , nString :: String
    , nStart :: Seconds
    , nDuration :: Seconds
    , nLocation :: Location
    , nAmplitude :: Double -- ? what units?
    } deriving (Eq, Show)

nEnd :: Note -> Seconds
nEnd n = nStart n + nDuration n

renderNotes :: (String -> StringIndex) -> [Note] -> Text
renderNotes indexOf = array2 "exc" . map list
    where
    list (Note strike string start dur loc amp) =
        [fromIntegral (indexOf string), start, loc, dur, amp, fromStrike strike]
    fromStrike Strike = 0
    fromStrike Pluck = 1

data Strike = Strike | Pluck deriving (Eq, Show)

data Finger = Finger {
    fString :: String
    , fInitial :: (Double, Double) -- position, force
    , fMovement :: [(Seconds, Double, Double)] -- time, position, force
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

renderFingers :: (String -> StringIndex) -> [Finger] -> Text
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

data Score = Score {
    sHighpass :: Bool
    , sNotes :: [Note]
    , sFingers :: [Finger]
    }
    deriving (Eq, Show)

renderAll :: Instrument -> Score -> (Text, Text)
renderAll instrument score =
    (renderInstrument instrument, renderScore (iStrings instrument) score)

renderScore :: [String] -> Score -> Text
renderScore strings (Score highpass notes fingers) = Text.unlines
    [ scalar "Tf" duration
    , scalar "highpass" highpass
    , renderNotes indexOf notes
    , renderFingers indexOf fingers
    ]
    where
    duration = maximum (map nEnd notes) + decay
    decay = 6
    indexOf str = fromMaybe (error $ "no string: " <> show str) $
        Map.lookup str toNum
        where toNum = Map.fromList $ zip strings [1..]


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

