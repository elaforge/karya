module Ness.Multiplate where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import Global
import Ness.Global
import qualified Ness.Util as Util


run :: Instrument -> Score -> IO FilePath
run instrument score = Util.run "multiplate" i s
    where (i, s) = renderAll instrument score

play :: IO ()
play = Util.play "multiplate"

renderAll :: Instrument -> Score -> (Text, Text)
renderAll instrument score =
    (renderInstrument instrument, renderScore plateNameOf score)
    where
    plateNameOf p = Map.findWithDefault (error $ "no plate: " <> show p) p m
        where
        m = Map.fromList $ map (second plateName) $
            zip (iPlates instrument) [1..]

-- * instrument

data Instrument = Instrument {
    iNormalize :: Bool
    , iAirbox :: Airbox
    , iPlates :: [Plate]
    , iMembranes :: [Membrane]
    , iDrumshells :: [Drumshell]
    } deriving (Eq, Show)

renderInstrument :: Instrument -> Text
renderInstrument (Instrument normalize airbox plates membranes drumshells) =
    Text.unlines $ concat
        [ [ "# mpversion 0.1"
          , "samplerate 44100"
          , "normalise_outs " <> render normalize
          , render airbox
          ]
        , renderPlates namePlates
        , map render (aOutputs airbox)
        , renderPlateOutputs namePlates
        , renderMembranes nameMembranes
        , renderDrumshells nameDrumshells
        ]
    where
    namePlates = map (first plateName) $ zip [1..] plates
    nameMembranes = map (first membraneName) $ zip [1..] membranes
    nameDrumshells = map (first drumshellName) $ zip [1..] drumshells

plateName, membraneName, drumshellName :: Int -> Text
plateName i = "plate" <> showt i
membraneName i = "membrane" <> showt i
drumshellName i = "drumshell" <> showt i

{- | • airbox defines the dimensions and other parameters of the airbox.
    Parameters are the width, the depth, the height, c_a and rho_a. Only one
    airbox can be defined currently.
-}
data Airbox = Airbox {
    aWidth, aDepth, aHeight :: Meters
    , aC_a, aRho_a :: Double
    , aOutputs :: [AirboxOutput]
    } deriving (Eq, Show)

instance Render Airbox where
    render = Text.unwords . ("airbox":) . map render . list
        where
        list (Airbox width depth height ca rhoa _) =
            [width, depth, height, ca, rhoa]

{- | • airbox output defines an output taken from within the airbox. The
    parameters are its X, Y and Z position.
-}
data AirboxOutput = AirboxOutput { aoX, aoY, aoZ :: Meters } deriving (Eq, Show)

instance Render AirboxOutput where
    render = Text.unwords . ("airbox_output":) . map render . list
        where list (AirboxOutput x y z) = [x, y, z]

{- | • plate defines a plate within the airbox. The first parameter is a name
    for the plate which must be a unique string and is used to refer to it for
    the purposes of outputs and strikes. The numeric parameters are size X,
    size Y, centre X, centre Y, centre Z, rho, H, E, nu, T60, sig1.
-}
data Plate = Plate {
    pSize :: (Meters, Meters)
    , pCenter :: (Meters, Meters, Meters)
    , pMaterial :: Material
    , pOutputs :: [PlateOutput]
    } deriving (Eq, Ord, Show)

renderPlates :: [(Text, Plate)] -> [Text]
renderPlates namePlates =
    [ Text.unwords $ "plate" : name : map render (list plate)
    | (name, plate) <- namePlates
    ]
    where
    list (Plate (sx, sy) (cx, cy, cz) (Material rho h e nu t60 sig1) _) =
        [sx, sy, cx, cy, cz, rho, h, e, nu, t60, sig1]

data Material = Material {
    mRho, mH, mE, mNu, mT60, mSig1 :: Double
    } deriving (Eq, Ord, Show)

{- | • plate output defines an output taken from a plate. The parameters are
    the name of the plate and the X and Y position for the output. The position
    values are normalised to the range -1 to +1.
-}
data PlateOutput = PlateOutput { poX, poY :: Meters } deriving (Eq, Ord, Show)

renderPlateOutputs :: [(Text, Plate)] -> [Text]
renderPlateOutputs namePlates =
    [ Text.unwords ["plate_output", name, render x, render y]
    | (name, p) <- namePlates
    , PlateOutput x y <- pOutputs p
    ]

{- | • membrane defines a circular drum membrane within the airbox. The first
    parameter is a name for the membrane which must be a unique string and is
    used to refer to it for the purposes of outputs and strikes. The numeric
    parameters are the radius, centre X, centre Y, centre Z, rho, H, T, E, nu,
    T60 and sig1.
-}
data Membrane = Membrane {
    mRadius :: Meters
    , mCenter :: (Meters, Meters, Meters)
    , mMaterial :: Material
    , mT :: Double
    } deriving (Eq, Show)

renderMembranes :: [(Text, Membrane)] -> [Text]
renderMembranes nameMembranes =
    [ Text.unwords $ "membrane" : name : map render (list membrane)
    | (name, membrane) <- nameMembranes
    ]
    where
    list (Membrane radius (cx, cy, cz) (Material rho h e nu t60 sig1) t) =
        [radius, cx, cy, cz, rho, h, t, e, nu, t60, sig1]

{- | • drumshell defines a cylindrical drum shell that acts as a barrier within
    the airbox. The first parameter is a name for the drum shell which must be
    a unique string. The numeric parameters are centre X, centre Y, bottom Z,
    radius and shell height.
-}
data Drumshell = Drumshell {
    dCenter :: (Meters, Meters)
    , dBottomZ :: Meters
    , dRadius :: Meters
    , dHeight :: Meters
    } deriving (Eq, Show)

renderDrumshells :: [(Text, Drumshell)] -> [Text]
renderDrumshells nameDrumshells =
    [ Text.unwords $ "drumshell" : name : map render (list drumshell)
    | (name, drumshell) <- nameDrumshells
    ]
    where
    list (Drumshell (cx, cy) bz radius height) = [cx, cy, bz, radius, height]

{- |
    • bassdrum defines a bass drum embedded in an airbox. This is just
    a shortcut for defining an airbox, a drum shell and two identical drum
    membranes in one go. The drum is centred within the airbox. The parameters
    are: airbox width, airbox depth, airbox height, c a, rho a, drum shell
    height, drum radius, membrane rho, H, T, E, nu, T60, sig1. For the purposes
    of adding strikes and taking outputs, the top membrane is named ’drumtop’
    and the bottom one ’drumbottom’.
-}

-- * score

data Score = Score {
    sStrikes :: [Strike]
    } deriving (Eq, Show)

renderScore :: (Plate -> Text) -> Score -> Text
renderScore plateNameOf (Score strikes) = Text.unlines $
    "duration " <> render (end + decay)
    : map (renderStrike plateNameOf) strikes
    where
    end = fromMaybe 0 $ Seq.maximum $ map sEnd strikes
    decay = 2

type Force = Double

{- | The first parameter of a strike is the start time. The other parameters
    are the name of the plate, the X position, the Y position, the duration,
    and the maximum force. The position values are normalised to the range 0-1.
-}
data Strike = Strike {
    sPlate :: Plate
    , sStart :: Seconds
    , sDuration :: Seconds
    , sPosition :: (Meters, Meters)
    , sForce :: Force
    } deriving (Eq, Show)

sEnd :: Strike -> Seconds
sEnd s = sStart s + sDuration s

renderStrike :: (Plate -> Text) -> Strike -> Text
renderStrike plateNameOf (Strike plate start dur (x, y) force) = Text.unwords
    [ "strike", render start, plateNameOf plate, render x, render y
    , render dur, render force
    ]
