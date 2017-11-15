-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Ness.Soundboard where
import Prelude hiding (String)
import qualified Data.Map as Map
import qualified Data.Text as Text

import Global
import Ness.Global


renderAll :: SamplingRate -> (Instrument, Score) -> (Text, Text)
renderAll sr (instrument, score) =
    (renderInstrument sr instrument, renderScore stringNameOf score)
    where
    stringNameOf str =
        Map.findWithDefault (error $ "no string: " <> show str) str m
        where
        m = Map.fromList $ zip (iStrings instrument) (map stringName [1..])

stringName :: Int -> Text
stringName i = "string" <> showt i

-- * instrument

data Instrument = Instrument {
    iStrings :: [String]
    , iPlate :: Plate
    , iCollision :: Collision
    } deriving (Eq, Show)

renderInstrument :: SamplingRate -> Instrument -> Text
renderInstrument sr (Instrument strings plate collision) = Text.unlines $ concat
    [ [ "# sbversion 0.1"
      , "samplerate " <> render sr
      ]
    , map (uncurry renderString) nameStrings
    , [ render plate, render collision ]
    , [renderStringOutput n o | (n, s) <- nameStrings, o <- sOutputs s]
    , map render (pOutputs plate)
    ]
    where
    nameStrings = zip (map stringName [1..]) strings

data String = String {
    sLength :: Meters
    , sDensity :: Double -- kg/m
    , sTension :: Newtons
    , sYoung :: Double -- ^ Young's modulus
    , sRadius :: Meters
    , sT60AtDC :: Seconds -- ?
    , sT60At1Khz :: Seconds -- ?
    , sStart :: (Double, Double)
    , sEnd :: (Double, Double)
    , sFrets :: Int
    , sFretHeight :: Meters -- ^ negative, from below strings
    , sBaseboardHeight :: Meters -- ^ below frets
    -- | variation in baseboard profile, small positive
    , sBaseboardProfile :: Double

    , sOutputs :: [StringOutput]
    } deriving (Eq, Ord, Show)

renderString :: Text -> String -> Text
renderString name
       (String len density tension young radius t60dc t601
        (sx, sy) (ex, ey) frets fretHeight baseHeight baseProfile _outputs) =
    Text.unwords $ concat
        [ ["string", name]
        , map render
            [ len, density, tension, young, radius, t60dc, t601
            , sx, sy, ex, ey
            ]
        , [render frets]
        , map render [fretHeight, baseHeight, baseProfile]
        ]

data StringOutput = StringOutput {
    sLocation :: Location
    , sPan :: Pan
    } deriving (Eq, Ord, Show)

renderStringOutput :: Text -> StringOutput -> Text
renderStringOutput name (StringOutput location pan) = Text.unwords $ concat
    [ ["string_out", name]
    , map render [location, pan]
    ]

data Plate = Plate {
    pDensity :: Double -- kg/m
    , pThickness :: Meters
    , pYoung :: Double
    , pPoisson :: Double -- ? Poisson's ratio
    , pTension :: Newtons
    , pSize :: (Meters, Meters)
    , pT60AtDC :: Seconds
    , pT60At1Khz :: Seconds

    , pOutputs :: [PlateOutput]
    } deriving (Eq, Ord, Show)

instance Render Plate where
    render (Plate density thickness young poisson tension (w, h) t60dc t601
            _outs) =
        Text.unwords $ "plate" : map render
            [density, thickness, young, poisson, tension, w, h, t60dc, t601]

data PlateOutput = PlateOutput {
    pPosition :: (Location, Location) -- 0 to 1
    , pPan :: Pan
    } deriving (Eq, Ord, Show)

instance Render PlateOutput where
    render (PlateOutput (x, y) pan) = Text.unwords $
        "plate_out" : map render [x, y, pan]

data Collision = Collision {
    cStiffness :: Double
    , cNonlinearityExponent :: Double
    , cIterations :: Int
    } deriving (Eq, Ord, Show)

instance Render Collision where
    render (Collision stiff nonlin iterations) = Text.unwords $
        "collision" : [render stiff, render nonlin, render iterations]

-- * score

data Score = Score {
    sDecay :: Seconds
    , sNotes :: [Note]
    } deriving (Eq, Show)

renderScore :: (String -> Text) -> Score -> Text
renderScore stringNameOf (Score decay notes) = Text.unlines $
    "duration " <> render (maximum (map nEnd notes) + decay)
    : map (renderNote stringNameOf) notes

data Strike = Strike | Pluck deriving (Eq, Show)

instance Render Strike where
    render Strike = "strike"
    render Pluck = "pluck"

data Note = Note {
    nString :: String
    , nStrike :: Strike
    , nStart :: Seconds
    , nDuration :: Seconds
    , nForce :: Newtons
    , nLocation :: Location
    } deriving (Eq, Show)

nEnd :: Note -> Seconds
nEnd n = nStart n + nDuration n

renderNote :: (String -> Text) -> Note -> Text
renderNote nameOf (Note str strike start dur force loc) = Text.unwords $
    [render strike, nameOf str] ++ map render [start, dur, force, loc]
