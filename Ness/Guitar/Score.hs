{-# LANGUAGE RecordWildCards #-}
module Ness.Guitar.Score where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Perform.Pitch as Pitch
import Global
import Ness.Global
import Ness.Guitar
import qualified Ness.Util as Util

Util.Interactive {..} = Util.interactive "guitar" renderAll
    (instrument, mkScore notes fingers)

testJawari = multiple "jawari4" (take 1 jawariVars)

jawariVars =
    [ ( Seq.join "-" [z hx, z lx,  "ht",  fmt h, "loc", fmt loc]
      , [set h loc amps]
      )
    | (hx, h) <- zip [0..] heights, (lx, loc) <- zip [0..] locations
    ]
    where
    z = zeroPad 2
    fmt = untxt . Num.showFloat 6
    set height loc amps =
        ( instrument { iFrets = [Fret height loc] }
        , (mkScore (notesEvery 1 amps) []) { sDecay = 3 }
        )
    -- heights = [-0.0005, -0.0002, -0.0001, -0.00005, -0.000025, -0.00001]
    -- locations = [0.02, 0.01, 0.005, 0.0025, 0.001, 0.0001]
    -- amps = [0.01, 0.025, 0.05, 0.1, 0.2, 0.5, 0.75, 1]
    -- The last 3 amps get buzz.

    heights =
        [ -0.0001, -0.00005, -0.000025
        , -0.00001, -0.000005, -0.000001
        ]
    locations = [0.001]
    amps = [0.05, 0.1, 0.2, 0.5, 0.75, 1, 1.5, 2]
    {- amp  .05 .1  .2  .5  .75 1   1.5 2
        0               -   *   *   *
        1               +   *   *   *
        2               +   *   *   * <- also nice
        3               +   *   *   * <- nice
        4           -   +   *   *   *
        5   x   x   x   x   x   x   x
    -}

(notes, fingers) = (oneNote 0.15, [])
frets = [jawariFret]
strings = [lowString] -- guitar

-- there's rattle from amp .75 to .4
-- duration makes a more rounded sound around 0.007 to .015, but becomes no
-- sound around .03

-- string, start, position, duration, amplitude, 0=strike 1=pluck
oneNote amp = map mkNote
    [ (head strings, 0, amp)
    ]
    where

notesEvery dur amps =
    [mkNote (head strings, t, amp) | (t, amp) <- zip (iterate (+dur) 0) amps]

mkNote (str, start, amp) = Note
    { nStrike = Strike
    , nString = str
    , nStart = start
    , nDuration = 0.0013
    , nLocation = 0.8
    , nAmplitude = amp
    }

-- (seconds, location, force)
fingerSlide1 =
    Finger (head strings) (0.01, 0)
        [ (0, 0.05, 0), (1, 0.05, 0)
        , (1.35, 0.05, force), (4, 0.8, force)
        , (5, 0.8, force)
        ]
    where
    force = 0.15

mkScore notes fingers = Score
    { sDecay = 4
    , sHighpass = True
    , sNotes = notes
    , sFingers = fingers
    }

instrument = Instrument
    { iSR = 48000
    , iStrings = strings
    , iFrets = frets
    , iBarrier = Barrier 1e10 1.3 10 (Solver 20 1e-12)
    , iBackboard = Backboard (-0.001) (-0.0001) 0
    , iFingerParams = FingerParams 0.005 1e7 3.3 100
    , iNormalizeOutputs = False
    , iSolver = Solver 20 0
    , iConnections = []
    }

str0 = String
    { sLength = 0.68
    , sTension = 12.1
    , sMaterial = steel
    , sRadius = 0.002
    , sT60 = (15, 5)
    , sOutputs = []
    }

stringSets =
    [ ("guitar",
        [ -- string_def = [0.68 2e11 12.1 0.0002 7850 15 5; 0.68 2e11 12.3 0.00015 7850 15 5; 0.68 2e11 21.9 0.00015 7850 15 5; 0.68 2e11 39.2 0.00015 7850 15 7; 0.68 2e11 27.6 0.0001 7850 15 5; 0.68 2e11 49.2 0.0001 7850 15 8];
        ])
    , ("bass",
        [ -- string_def = [0.88 2e11 4.8 0.0002 7850 15 3; 0.88 2e11 9.3 0.0002 7850 15 3; 0.88 2e11 9.2 0.00015 7850 15 3; 0.88 2e11 10.5 0.00012 7850 15 3];
        ])
    ]

lowString = String
    { sLength = 0.68
    , sTension = 12.1
    , sMaterial = steel
    , sRadius = 0.0002
    , sT60 = (15, 5)
    , sOutputs = [Output 0.9 (-0.5), Output 0.9 0.5]
    }

-- length, young, tension, radius, density, t60, t60
-- lowString = string_def = [0.68 2e11 12.1 0.0002 7850 15 5];

guitar = map make
    [ (12.1, 0.00020, 5, 0.2)
    , (12.3, 0.00015, 5, 0.3)
    , (21.9, 0.00015, 5, 0.4)
    , (27.6, 0.00015, 5, 0.5)
    , (39.2, 0.00015, 7, 0.6)
    , (49.2, 0.00010, 8, 0.7)
    ]
    where
    make (tension, radius, t60, pan) =
        String 0.78 tension silk radius (15, t60) [Output 0.9 pan]

pipa = map make
    [ (40, 0.0010, 5) -- it's actually 0.0016 but that gets inharmonic
    , (40, 0.0009, 5)
    , (40, 0.00079, 7)
    , (40, 0.0004, 9)
    ]
    where
    make (tension, radius, t60) =
        String 0.6985 tension nylon radius (15, t60) [Output 0.9 0.5]

jawariFret = Fret { fHeight = (-0.0001), fLocation = 0.01 }

wholeFrets = map (Fret (-0.0005))
    [ 0.109101281859661
    , 0.206299474015900
    , 0.292893218813452
    , 0.370039475052563
    , 0.438768975845313
    , 0.500000000000000
    , 0.554550640929830
    , 0.603149737007950
    , 0.646446609406726
    , 0.685019737526282
    ]

chromaticFrets = map (Fret (-0.0005))
    [ 0.056125687318306
    , 0.109101281859661
    , 0.159103584746285
    , 0.206299474015900
    , 0.250846461561659
    , 0.292893218813452
    , 0.332580072914983
    , 0.370039475052563
    , 0.405396442498639
    , 0.438768975845313
    , 0.470268452820352
    , 0.500000000000000
    , 0.528062843659153
    , 0.554550640929830
    , 0.579551792373143
    , 0.603149737007950
    , 0.625423230780830
    , 0.646446609406726
    , 0.666290036457491
    , 0.685019737526282
    ]

legongFrets = makeFrets (-0.0005) (head legong) (tail legong)

legong :: [Pitch.NoteNumber]
legong = map Pitch.nn
    [ 51.82  -- 3e, rambat begin
    , 55.70  -- 3u
    , 56.82  -- 3a, trompong begin

    , 60.73  -- 4i
    , 62.80  -- 4o, pemade begin
    , 63.35  -- 4e, reyong begin
    , 67.70  -- 4u
    , 68.20  -- 4a

    , 72.46  -- 5i
    , 73.90  -- 5o, kantilan begin
    , 75.50  -- 5e
    , 79.40  -- 5u, trompong end
    , 80.50  -- 5a

    , 84.46  -- 6i, rambat end, pemade end
    ]
