-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Ness.Multiplate.Score where
import Ness.Global
import Ness.Multiplate
import qualified Ness.Util as Util


Util.Interactive {..} = Util.interactive "multiplate" renderAll
    (instrument1, score1)

score1 = Score 6 $ map strike
    [ (low , 0, (0.6653034, 0.3507947),     1000)
    , (high, 2, (0.5177853, 0.41139928754), 500)
    , (mid,  4, (0.68823711, 0.363045233),  500)
    , (high, 6, (0.5177853, 0.41139928754), 750)
    , (low,  7.8, (0.6653034, 0.3507947),   500)
    , (low,  7.9, (0.6653034, 0.3507947),   600)
    , (low,  8, (0.6653034, 0.3507947),     800)
    , (high, 10, (0.5177853, 0.41139928754),500)
    , (mid,  12, (0.68823711, 0.363045233),1000)
    ]
    where
    strike (plate, start, pos, force) = Strike
        { sObject = pName plate
        , sStart = start
        , sDuration = 0.007
        , sPosition = pos
        , sForce = force
        }

instrument1 = Instrument
    { iName = "i1"
    , iNormalize = False
    , iAirbox = Airbox
        { aWidth = 1.32
        , aDepth = 1.32
        , aHeight = 1.37
        , aC_a = 340.0
        , aRho_a = 1.21
        , aOutputs = map (uncurry3 AirboxOutput)
            [ (0.01, 0.02, 0.6)
            , (-0.6, 0.012, 0.15)
            , (-0.6, 0.012, -0.15)
            , (0.01, 0.02, -0.6)
            , (0.01, 0.7, 0.01)
            , (-0.01, -0.7, -0.01)
            , (0.6, 0.012, 0.15)
            , (0.6, 0.012, -0.15)
            ]
        }
    , iPlates = [low, high, mid]
    , iMembranes = []
    , iDrumshells = []
    }

membrane1 = Membrane
    { mName = "mem1"
    , mRadius = 0.5
    , mCenter = (0, 0, 0.22)
    , mMaterial = Material 7800 0.002 8e11 0.33 4 0.001
    , mT = 0
    }

low = Plate
    { pName = "low"
    , pSize = (0.81, 0.87)
    , pCenter = (0, 0, 0.22)
    , pMaterial = Material 7800 0.002 8e11 0.33 4 0.001
    , pOutputs = map (uncurry PlateOutput)
        [ (0.141421356237310, 0.113137084989848)
        , (-0.282842712474619, 0.056568542494924)
        ]
    }
high = Plate
    { pName = "high"
    , pSize = (0.39, 0.42)
    , pCenter = (-0.1, -0.1, 0)
    , pMaterial = Material 7800 0.002 8e11 0.33 4 0.001
    , pOutputs = map (uncurry PlateOutput)
        [ (0.141421356237310, 0.113137084989848)
        , (-0.282842712474619, 0.056568542494924)
        ]
    }
mid = Plate
    { pName = "mid"
    , pSize = (0.65, 0.61)
    , pCenter = (0.1, 0.1, -0.27)
    , pMaterial = Material 7800 0.002 8e11 0.33 4 0.001
    , pOutputs = map (uncurry PlateOutput)
        [ (0.141421356237310, 0.113137084989848)
        , (-0.282842712474619, 0.056568542494924)
        ]
    }
