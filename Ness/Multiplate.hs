-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ness.Multiplate where
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lists as Lists

import           Global
import           Ness.Global


renderAll :: SamplingRate -> (Instrument, Score) -> (Text, Text)
renderAll sr (instrument, score) =
    (renderInstrument sr instrument, render score)

verify :: Instrument -> Score -> [Text]
verify instrument score =
    map ("duplicate object name: "<>) duplicates
        ++ mapMaybe strike (sStrikes score)
    where
    (objects, duplicates) = bimap Set.fromList (map fst) $
        Lists.partitionDups id (iObjects instrument)
    strike s
        | sObject s `Set.notMember` objects =
            Just $ "strike at " <> pretty (sStart s) <> ": unknown object "
                <> sObject s
        | otherwise = Nothing

-- * instrument

data Instrument = Instrument {
    iName :: Text
    , iNormalize :: Bool
    , iAirbox :: Airbox
    , iPlates :: [Plate]
    , iMembranes :: [Membrane]
    , iDrumshells :: [Drumshell]
    } deriving (Eq, Ord, Show)

iObjects :: Instrument -> [Text]
iObjects i = map pName (iPlates i) ++ map mName (iMembranes i)

renderInstrument :: SamplingRate -> Instrument -> Text
renderInstrument sr
        (Instrument _ normalize airbox plates membranes drumshells) =
    Text.unlines $ concat
        [ [ "# mpversion 0.1"
          , "samplerate " <> render sr
          , "normalise_outs " <> render normalize
          , render airbox
          ]
        , map render plates
        , map render (aOutputs airbox)
        , renderPlateOutputs plates
        , map render membranes
        , map render drumshells
        ]

{- | • airbox defines the dimensions and other parameters of the airbox.
    Parameters are the width, the depth, the height, c_a and rho_a. Only one
    airbox can be defined currently.
-}
data Airbox = Airbox {
    aWidth, aDepth, aHeight :: Meters
    , aC_a, aRho_a :: Double
    , aOutputs :: [AirboxOutput]
    } deriving (Eq, Ord, Show)

instance Render Airbox where
    render = Text.unwords . ("airbox":) . map render . list
        where
        list (Airbox width depth height ca rhoa _) =
            [width, depth, height, ca, rhoa]

{- | • airbox output defines an output taken from within the airbox. The
    parameters are its X, Y and Z position.
-}
data AirboxOutput = AirboxOutput { aoX, aoY, aoZ :: Meters }
    deriving (Eq, Ord, Show)

instance Render AirboxOutput where
    render = Text.unwords . ("airbox_output":) . map render . list
        where list (AirboxOutput x y z) = [x, y, z]

{- | • plate defines a plate within the airbox. The first parameter is a name
    for the plate which must be a unique string and is used to refer to it for
    the purposes of outputs and strikes. The numeric parameters are size X,
    size Y, centre X, centre Y, centre Z, rho, H, E, nu, T60, sig1.
-}
data Plate = Plate {
    pName :: Text
    , pSize :: (Meters, Meters)
    , pCenter :: (Meters, Meters, Meters)
    , pMaterial :: Material
    , pOutputs :: [PlateOutput]
    } deriving (Eq, Ord, Show)

instance Render Plate where
    render plate =
        Text.unwords $ "plate" : pName plate : map render (list plate)
        where
        list (Plate _ (sx, sy) (cx, cy, cz) (Material rho h e nu t60 sig1) _) =
            [sx, sy, cx, cy, cz, rho, h, e, nu, t60, sig1]

data Material = Material {
    mRho, mH, mE, mNu, mT60, mSig1 :: Double
    } deriving (Eq, Ord, Show)

{- | • plate output defines an output taken from a plate. The parameters are
    the name of the plate and the X and Y position for the output. The position
    values are normalised to the range -1 to +1.
-}
data PlateOutput = PlateOutput { poX, poY :: Meters } deriving (Eq, Ord, Show)

renderPlateOutputs :: [Plate] -> [Text]
renderPlateOutputs plates =
    [ Text.unwords ["plate_output", pName plate, render x, render y]
    | plate <- plates
    , PlateOutput x y <- pOutputs plate
    ]

{- | • membrane defines a circular drum membrane within the airbox. The first
    parameter is a name for the membrane which must be a unique string and is
    used to refer to it for the purposes of outputs and strikes. The numeric
    parameters are the radius, centre X, centre Y, centre Z, rho, H, T, E, nu,
    T60 and sig1.
-}
data Membrane = Membrane {
    mName :: Text
    , mRadius :: Meters
    , mCenter :: (Meters, Meters, Meters)
    , mMaterial :: Material
    , mT :: Double
    } deriving (Eq, Ord, Show)

instance Render Membrane where
    render m = Text.unwords $ "membrane" : mName m : map render (list m)
        where
        list (Membrane _ radius (cx, cy, cz) (Material rho h e nu t60 sig1) t) =
            [radius, cx, cy, cz, rho, h, t, e, nu, t60, sig1]

{- | • drumshell defines a cylindrical drum shell that acts as a barrier within
    the airbox. The first parameter is a name for the drum shell which must be
    a unique string. The numeric parameters are centre X, centre Y, bottom Z,
    radius and shell height.
-}
data Drumshell = Drumshell {
    dName :: Text
    , dCenter :: (Meters, Meters)
    , dBottomZ :: Meters
    , dRadius :: Meters
    , dHeight :: Meters
    } deriving (Eq, Ord, Show)

instance Render Drumshell where
    render d = Text.unwords $ "drumshell" : dName d : map render (list d)
        where
        list (Drumshell _ (cx, cy) bz radius height) =
            [cx, cy, bz, radius, height]

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
    sDecay :: Seconds
    , sStrikes :: [Strike]
    } deriving (Eq, Show)

instance Render Score where
    render (Score decay strikes) = Text.unlines $
        "duration " <> render (end + decay) : map render strikes
        where
        end = fromMaybe 0 $ Lists.maximum $ map sStart strikes

-- | Probably Newtons?
type Force = Double

{- | The first parameter of a strike is the start time. The other parameters
    are the name of the plate, the X position, the Y position, the duration,
    and the maximum force. The position values are normalised to the range 0-1.
-}
data Strike = Strike {
    sObject :: Text
    , sStart :: Seconds
    , sDuration :: Seconds
    , sPosition :: (Meters, Meters)
    , sForce :: Force
    } deriving (Eq, Show)

instance Render Strike where
    render (Strike name start dur (x, y) force) = Text.unwords
        [ "strike", render start, name, render x, render y
        , render dur, render force
        ]
