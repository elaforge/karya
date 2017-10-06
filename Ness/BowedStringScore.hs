{-# LANGUAGE RecordWildCards #-}
module Ness.BowedStringScore where
import Ness.BowedString
import qualified Ness.Util as Util


Util.Interactive {..} = Util.interactive "bstring" renderAll
    instrument0 score0

score0 = Score
    { sDuration = 4
    , sBows = bows
    , sFingers = fingers
    }

ttb = 0.05 -- transition time for bow gestures
end = 2

bows =
    [ BowMovement
        { bString = str1
        , bInitialVertical = (1e-8, 0)
        , bInitialHorizontal = (0, 0)
        , bBreakpoints =
            [ (0,       Breakpoint 0.86 0 0)
            , (ttb,     Breakpoint 0.86 (-0.3) 0)
            , (0.5,     Breakpoint 0.86 (-0.3) 0)
            , (0.5+ttb, Breakpoint 0.86 (-0.3) 2)
            , (end,     Breakpoint 0.9 (-0.4) 2.5)
            ]
        }
    -- , BowMovement
    --     { bString = str2
    --     , bInitialVertical = (1e-8, 0)
    --     , bInitialHorizontal = (0, 0)
    --     , bBreakpoints =
    --         [ (0,       Breakpoint 0.86 0 0)
    --         , (ttb,     Breakpoint 0.86 (-0.3) 0)
    --         , (0.5,     Breakpoint 0.86 (-0.3) 0)
    --         , (0.5+ttb, Breakpoint 0.86 (-0.3) 2)
    --         , (end,     Breakpoint 0.89 (-0.41) 2.43)
    --         ]
    --     }
    ]

fingers =
    [ FingerMovement
        { fString = str1
        , fInitialVertical = (1e-8, 0)
        , fInitialHorizontal = (0, 0)
        , fBreakpoints =
            [ (0,       Breakpoint p 0 0)
            , (ttb,     Breakpoint p (-3) 0)
            , (end,     Breakpoint 0.5 (-3) 0)
            ]
        , fVibrato = Vibrato 0.8 2 0.5 0.02 5
        }
    , FingerMovement
        { fString = str2
        , fInitialVertical = (1e-8, 0)
        , fInitialHorizontal = (0, 0)
        , fBreakpoints =
            [ (0,       Breakpoint 0.250846461561659 0 0)
            , (ttb,     Breakpoint 0.250846461561659 (-3) 0)
            , (end,     Breakpoint 0.579551792373143 (-3) 0)
            ]
        , fVibrato = Vibrato 0 0 0 0 0
        }
    ]
    where p = 0.159103584746285

instrument0 = Instrument
    { iNormalizeOuts = False
    , iStrings = [str1, str2]
    , iBow = Bow
        { bKw = 1e6
        , bAlpha = 2.0
        , bBeta = 20
        , bLambda = 10
        , bM = 0.1
        }
    , iFinger = Finger
        { fKw = 1e5
        , fKu = 1e3
        , fAlpha = 2.2
        , fBeta = 50
        , fLambda = 20
        , fM = 0.05
        }
    }

str1 = String
    { sFrequency = 440
    , sDensity = 7e-4
    , sRadius = 3e-4
    , sYoung = 2e11
    , sT60 = (10, 8)
    , sLength = 0.35
    }
str2 = String
    { sFrequency = 180
    , sDensity = 8e-4
    , sRadius = 1e-4
    , sYoung = 2e11
    , sT60 = (10, 8)
    , sLength = 0.34
    }
