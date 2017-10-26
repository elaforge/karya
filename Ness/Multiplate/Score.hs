{-# LANGUAGE RecordWildCards #-}
module Ness.Multiplate.Score where
import Ness.Global
import Ness.Multiplate
import qualified Ness.Util as Util


Util.Interactive {..} = Util.interactive "multiplate" renderAll
    (instrument0, score0)

score0 = Score 6
    [ Strike plate1 0 dur (0.6653034, 0.3507947) 1000
    , Strike plate2 2 dur (0.5177853, 0.41139928754) 500
    , Strike plate3 4 dur (0.68823711, 0.363045233) 500
    , Strike plate2 6 dur (0.5177853, 0.41139928754) 750
    , Strike plate1 7.8 dur (0.6653034, 0.3507947) 500
    , Strike plate1 7.9 dur (0.6653034, 0.3507947) 600
    , Strike plate1 8 dur (0.6653034, 0.3507947) 800
    , Strike plate2 10 dur (0.5177853, 0.41139928754) 500
    , Strike plate3 12 dur (0.68823711, 0.363045233) 1000
    ]
    where
    dur = 0.007

instrument0 = Instrument
    { iNormalize = False
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
    , iPlates = [plate1, plate2, plate3]
    , iMembranes = []
    , iDrumshells = []
    }

-- membrane1 = Membrane
--     { mRadius = 0.5
--     , mCenter = (0, 0, 0.22)
--     , mMaterial = Material 7800 0.002 8e11 0.33 4 0.001
--     , mT = 0
--     }

plate1 = Plate
    { pSize = (0.81, 0.87)
    , pCenter = (0, 0, 0.22)
    , pMaterial = Material 7800 0.002 8e11 0.33 4 0.001
    , pOutputs = map (uncurry PlateOutput)
        [ (0.141421356237310, 0.113137084989848)
        , (-0.282842712474619, 0.056568542494924)
        ]
    }
plate2 = Plate
    { pSize = (0.39, 0.42)
    , pCenter = (-0.1, -0.1, 0)
    , pMaterial = Material 7800 0.002 8e11 0.33 4 0.001
    , pOutputs = map (uncurry PlateOutput)
        [ (0.141421356237310, 0.113137084989848)
        , (-0.282842712474619, 0.056568542494924)
        ]
    }
plate3 = Plate
    { pSize = (0.65, 0.61)
    , pCenter = (0.1, 0.1, -0.27)
    , pMaterial = Material 7800 0.002 8e11 0.33 4 0.001
    , pOutputs = map (uncurry PlateOutput)
        [ (0.141421356237310, 0.113137084989848)
        , (-0.282842712474619, 0.056568542494924)
        ]
    }
