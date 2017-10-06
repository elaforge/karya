module Ness.BowedString where
import Prelude hiding (String)
import qualified Data.Map as Map
import qualified Data.Text as Text

import Global
import Ness.Global
import qualified Ness.Util as Util


{-
instrument (string) instrument type to simulate. Valid values are ’violin’,
’viola’ and ’cello’.

instr_numb (scalar) specifies which of the preset instruments to use. Valid
values are 1-5 for violins, 1-2 for violas and 1-3 for cellos.
-}

run :: Instrument -> Score -> IO FilePath
run instrument score = Util.run "bstring" i s
    where (i, s) = renderAll instrument score

play :: IO ()
play = Util.play "bstring"

renderAll :: Instrument -> Score -> (Text, Text)
renderAll instrument score =
    (render instrument, renderScore indexOf score)
    where
    indexOf str = Map.findWithDefault (error $ "no string: " <> show str)
        str strings
    strings = Map.fromList $ zip (iStrings instrument) [1..]

-- * instrument

data Instrument = Instrument {
    -- | Normalises the outputs separately before saving them.
    iNormalizeOuts :: Bool
    , iStrings :: [String]
    , iBow :: Bow
    , iFinger :: Finger
    } deriving (Eq, Show)

instance Render Instrument where
    render (Instrument normalize strings bow finger) = Text.unlines $
        [ "% bsversion 1.0"
        , "Fs = 44100;"
        , scalar "normalize_outs" normalize
        , renderStrings strings
        , render bow
        , render finger
        ]

{- | strings (array of structs) can be used to specify all the string
    parameters manually instead of using a preset instrument. Each string has
    members f0 (frequency), rho (density), rad (radius), E (Young’s Modulus),
    T60 (array of T60 values at 0Hz and 1KHz), and L (length).
-}
data String = String {
    sFrequency :: Frequency
    , sDensity :: Double
    , sRadius :: Double
    , sYoung :: Double
    , sT60 :: (Double, Double) -- at 0 and 1khz
    , sLength :: Meters
    } deriving (Eq, Ord, Show)

renderStrings :: [String] -> Text
renderStrings strings = Text.unlines $
    scalar "Nstrings" (length strings)
    : "strings = struct;"
    : concatMap string (zip [1..] strings)
    where
    string (i, String frequency density radius young (t60, t601) len) =
        map (("strings(" <> showt i <> ").")<>)
            [ scalar "f0" frequency
            , scalar "rho" density
            , scalar "rad" radius
            , scalar "E" young
            , "T60 = [" <> render t60 <> "; " <> render t601 <> "];"
            , scalar "L" len
            ]

-- bow (struct) specifies parameters for the bows. Members are Kw, alpha, beta,
-- lambda and M.
data Bow = Bow { -- TODO?
    bKw :: Double
    , bAlpha :: Double
    , bBeta :: Double
    , bLambda :: Double
    , bM :: Double
    } deriving (Eq, Show)

instance Render Bow where
    render (Bow kw alpha beta lambda m) =
        Text.unlines $ "bow = struct();" : map (("bow."<>) . uncurry scalar)
            [ ("Kw", kw)
            , ("alpha", alpha)
            , ("beta", beta)
            , ("lambda", lambda)
            , ("M", m)
            ]

-- fing (struct) specifies parameters for the fingers. Members are Kw, Ku,
-- alpha, beta, lambda and M.
data Finger = Finger { -- TODO?
    fKw :: Double
    , fKu :: Double
    , fAlpha :: Double
    , fBeta :: Double
    , fLambda :: Double
    , fM :: Double
    } deriving (Eq, Show)

instance Render Finger where
    render (Finger kw ku alpha beta lambda m) =
        Text.unlines $ "fing = struct();" : map (("fing."<>) . uncurry scalar)
            [ ("Kw", kw)
            , ("Ku", ku)
            , ("alpha", alpha)
            , ("beta", beta)
            , ("lambda", lambda)
            , ("M", m)
            ]

-- * score

data Score = Score {
    sDuration :: Seconds
    , sBows :: [BowMovement]
    , sFingers :: [FingerMovement]
    } deriving (Eq, Show)

renderScore :: (String -> Int) -> Score -> Text
renderScore indexOf (Score dur bows fingers) = Text.unlines $ concat
    [ [scalar "Tf" dur]
    , map (uncurry (renderBowMovement indexOf)) (zip [1..] bows)
    , map (uncurry (renderFingerMovement indexOf)) (zip [1..] fingers)
    ]

type Position = Double
type Velocity = Double

{- | bowgest (array of structs) specifies the movement of the bows in the
    simulation. Each bow has the following members: stringnumber (which string
    the bow is on, numbered from 1), w0, vw0, u0, vu0 (initial positions and
    velocities), times (an array of times in seconds for which the bow position
    and force are given), pos (an array of bow positions at the specified
    times), force w (an array of vertical force values at the specified times),
    and force u (an array of horizontal force values at the specified times).
-}
data BowMovement = BowMovement {
    bString :: String
    , bInitialVertical :: (Position, Velocity)
    , bInitialHorizontal :: (Position, Velocity)
    , bBreakpoints :: [(Seconds, Breakpoint)]
    } deriving (Eq, Show)

{- | finggest (array of structs) specifies the movement of the fingers in the
    simulation. Each finger has the same members described in the bow gesture
    above, but with one addition: a vibrato array. This is a 2D array, each row
    of which contains 5 values: a start time, an end time, a ramp time (all in
    seconds), an amplitude and a frequency, for a vibrato motion.
-}
data FingerMovement = FingerMovement {
    fString :: String
    , fInitialVertical :: (Position, Velocity)
    , fInitialHorizontal :: (Position, Velocity)
    , fBreakpoints :: [(Seconds, Breakpoint)]
    , fVibrato :: Vibrato
    } deriving (Eq, Show)

data Vibrato = Vibrato {
    vStart :: Seconds
    , vEnd :: Seconds
    , vRamp :: Seconds
    , vAmplitude :: Double -- 0 to 1?
    , vFrequency :: Frequency
    } deriving (Eq, Show)

renderMovement :: Text -> (String -> Int) -> Int
    -> (String, (Position, Velocity), (Position, Velocity),
        [(Seconds, Breakpoint)])
    -> [Text] -> Text
renderMovement name indexOf i (str, vertical, horizontal, bps) fields =
    Text.unlines $ map ((name <> "(" <> showt i <> ").") <>) $
        [ scalar "stringnumber" (indexOf str)
        , scalar "w0" (fst vertical)
        , scalar "vw0" (snd vertical)
        , scalar "u0" (fst horizontal)
        , scalar "vu0" (snd horizontal)
        , array "times" ts
        , array "pos" (map bPosition bs)
        , array "force_w" (map bVertical bs)
        , array "force_u" (map bHorizontal bs)
        ] ++ fields
        where (ts, bs) = unzip bps

renderBowMovement :: (String -> Int) -> Int -> BowMovement -> Text
renderBowMovement indexOf i (BowMovement str v h bps) =
    renderMovement "bowgest" indexOf i (str, v, h, bps) []

renderFingerMovement :: (String -> Int) -> Int -> FingerMovement -> Text
renderFingerMovement indexOf i (FingerMovement str v h bps vibrato) =
    renderMovement "finggest" indexOf i (str, v, h, bps)
        [array "vibrato" (list vibrato)]
    where list (Vibrato start end ramp amp freq) = [start, end, ramp, amp, freq]

type Force = Double

data Breakpoint = Breakpoint {
    bPosition :: Position
    , bVertical :: Force
    , bHorizontal :: Force
    } deriving (Eq, Show)


-- * util

scalar :: Render a => Text -> a -> Text
scalar name val = name <> " = " <> render val <> ";"

array :: Render a => Text -> [a] -> Text
array name vals = name <> " = [" <> Text.unwords (map render vals) <> "];"
