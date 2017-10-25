{-# LANGUAGE RecordWildCards #-}
module Ness.Guitar.Score where
import Prelude hiding (String)

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Perform.Pitch as Pitch
import Global
import Ness.Global
import Ness.Guitar
import qualified Ness.Guitar.Bali as Bali
import qualified Ness.Util as Util

Util.Interactive {..} = Util.interactive "guitar" renderAll
    -- (instrument, mkScore notes fingers)
    (Bali.sangsih, mkScore notes fingers)


(notes, fingers) = sampleSet

frets = [] -- legongFrets
-- strings = [lowerString, lowString] -- guitar
strings = Bali.legongStrings2

lowSR = True

testJawari = variations "jawari3" jawariVars
jawariVars :: [(FilePath, [(Instrument, Score)])]
jawariVars =
    [ ( Seq.join "-" ["str" <> show strx, z hx, "ht",  fmt h]
      , [makeScore str h loc amps]
      )
    | (strx, str) <- zip [0..] (take 1 strings)
    , (hx, h) <- zip [0..] (take 1 heights)
    ]
    where
    z = Util.zeroPad 2
    fmt = untxt . Num.showFloat 6
    makeScore str height loc amps =
        ( instrument { iFrets = [Fret height loc] }
        , mkScore (repeatNote str 1 amps) []
        )
    heights =
        [ -0.00001, -0.000005, -0.000001
        ]
    loc = 0.001
    amps = [0.1, 0.15, 0.2, 0.25, 0.4, 0.5, 0.75, 1, 1.5, 2]
    {- amp  .05 .1  .2  .5  .75 1   1.5 2
      ht0               +   *   *   * <- nice
      ht1           -   +   *   *   *
      ht2   x   x   x   x   x   x   x
    -}

testBackboard = variations "backboard2" backboardVars
backboardVars :: [(FilePath, [(Instrument, Score)])]
backboardVars =
    [ (Seq.join "-" ["backboard", fmt distance], [makeScore distance])
    | distance <- distances
    ]
    where
    makeScore distance =
        ( instrument { iBackboard = backboard distance }
        , mkScore (repeatNote (head strings) 1 standardAmps) []
        )
    distances = [-0.002, -0.0017, -0.0015, -0.0013,  -0.001]
    fmt = show . abs . round . (*1e4)

    --          1 2 3 4 5 6 7 8 9 a b c d e f 10
    --          1/16  .25     .5      .75     1
    -- -0.0020: - - - - - - - - - - - - - - + +
    -- -0.0017: - - - - - - - - - - - + + + * *
    -- -0.0015: - - - - - - - - - - + + + + * *
    -- -0.0013: - - - - - - - - - + + + + + * *
    -- -0.0010: - - - - - - - + + + + + + * * *
    backboard distance = Backboard
        { ba = distance
        , bb = 0
        , bc = 0
        }

sampleSet = (eachAmpEachString (take 1 strings) standardAmps decay, [])
    where decay = 6 -- time for each string to go to 0

standardAmps = drop 1 $ Seq.range 0 maxAmp (maxAmp/16)

maxAmp = 0.65

-- (notes, fingers) = ([strike (head strings, 0.8, 0.25)], [])

-- (notes, fingers) = strikeEachFret (head strings) 0.5

-- (notes, fingers) = (ns, slideUp ns 3.5)
--     where ns = take 1 $ eachOpenString 0.3 1 4

-- (notes, fingers) = (ns, fingerUp ns 0.5 6)
--     where ns = take 4 $ eachOpenString 0.3 1 4

-- (notes, fingers) = (ns, []) where ns = eachOpenString (0.7*maxAmp) 0 0.5
-- (notes, fingers) = (ns, []) where ns = eachAmpEachString standardAmps 0.5

eachOpenString amp start dur =
    [strike (str, t, amp) | (str, t) <- zip strings (iterate (+dur) start)]

eachAmpEachString strings amps dur =
    [strike (str, t, amp) | (t, (str, amp)) <- strikes]
    where
    strikes = zip ts [(str, amp) | str <- strings, amp <- amps]
    ts = iterate (+dur) 0

-- Strike with the finger right below each fret.
fingerUp notes dur noteCount = do
    n <- notes
    return $ Finger (nString n) (0.01, 0) $ concat $ do
        (t, fret) <- zip (drop 2 $ iterate (+dur) (nStart n)) (take noteCount frets)
        let loc = fLocation fret - offset
        let end = t + dur
        return
            [ (t, loc, 0), (t+eta, loc, amp)
            , (end-eta-eta, loc, amp), (end-eta, loc, 0)
            ]
    where
    eta = 0.05
    amp = 0.5
    offset = 0.01 -- below the fret

-- strikeEachFret str dur =
--     ( repeatNote str dur (replicate (length frets) amp)
--     , [Finger str (0, 0) $ (dur-eta, 0, 0) : concat
--         [ [(t, loc f, force), (t+dur-eta, loc f, force)]
--         | (t, f) <- zip (iterate (+dur) 0) frets
--         ]]
--     )
--     where
--     loc = subtract 0.01 . fLocation
--     amp = 0.25
--     force = 0.15
--     eta = 0.05

-- fingersUp :: [Note] -> Seconds -> Int -> [Finger]
-- fingersUp notes dur noteCount = do
--     n <- notes
--     (t, fret) <- zip (iterate (+dur) (nStart n))
--         (take noteCount (map fLocation frets))
--     return $ Finger (nString n) (fret, 0)
--         [ (t, fret, 0), (t+eta, fret, amp) ]
--     where
--     eta = 0.05
--     amp = 0.15
--     -- One Finger with a strike right below each fret.

slideUp notes dur = do
    n <- notes
    let start = nStart n + 0.5
    let end = nStart n + dur
    return $ Finger (nString n) (0.01, 0)
        [ (start, 0.05, 0), (start+eta, 0.05, amp)
        , (end, 0.5, amp), (end + eta, 0.5, 0)
        ]
    where
    amp = 0.15
    eta = 0.05

-- there's rattle from amp .75 to .4
-- duration makes a more rounded sound around 0.007 to .015, but becomes no
-- sound around .03

-- string, start, position, duration, amplitude, 0=strike 1=pluck
oneNote amp = map strike
    [ (head strings, 0, amp)
    ]

repeatNote :: String -> Seconds -> [Newtons] -> [Note]
repeatNote str dur amps =
    [strike (str, t, amp) | (t, amp) <- zip (iterate (+dur) 0) amps]

strike :: (String, Seconds, Newtons) -> Note
strike (str, start, amp) = Note
    { nStrike = Pluck
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
    { iName = "test"
    , iSR = if lowSR then 11000 else 44100
    , iStrings = strings
    , iFrets = [] -- frets
    , iBarrier = Barrier 1e10 1.3 10 (Solver 20 1e-12)
    , iBackboard = Backboard -- a + bx + bx^2, where x is length
        -- distances = [-0.002, -0.0017, -0.0015, -0.0013,  -0.001]
        { ba = -0.0015
        , bb = 0
        , bc = 0
        }
    , iFingerParams = FingerParams
        -- { fMass = 0.005
        -- , fStiffness = 2e7
        -- , fExponent = 3
        -- , fLoss = 10
        -- }
        { fMass = 0.005
        , fStiffness = 1e7
        , fExponent = 3.3
        , fLoss = 100
        }
    , iNormalizeOutputs = True
    , iSolver = Solver 20 0
    , iConnections = []
    }

lowString = String
    { sLength = 0.68
    , sTension = 12.1
    , sMaterial = steel
    , sRadius = 0.0002
    , sT60 = (15, 5)
    , sNn = 0 -- TODO
    , sOutputs = [Output 0.9 (-0.5), Output 0.8 0.5]
    }

lowerString = String
    { sLength = 0.9
    , sTension = 12.1
    , sMaterial = steel
    , sRadius = 0.0002
    , sT60 = (15, 5)
    , sNn = 0 -- TODO
    , sOutputs = [Output 0.8 (-0.8), Output 0.5 0.8]
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
        String 0.78 tension steel radius (15, t60) 0 [Output 0.9 pan]

-- middle C = 60nn
--
-- 4 4 2M 3m 6
-- semitones: 5 5 2 3 9

samePitchStrings = map make
    [ (0.78, 12.1, 0.00020, 5, 0.2)
    , (1.05, 12.3, 0.00015, 5, 0.3) -- *
    , (1.40, 21.9, 0.00015, 5, 0.4) -- *
    , (1.55, 27.6, 0.00015, 5, 0.5) -- *
    , (1.89, 39.2, 0.00015, 7, 0.6) -- *
    , (3.23, 49.2, 0.00010, 8, 0.7) -- *
    ]
    where
    make (len, tension, radius, t60, pan) =
        String len tension steel radius (15, t60) 0 -- TODO pitch
            [Output 0.9 pan]

pipa = map make
    [ (40, 0.0010, 5) -- it's actually 0.0016 but that gets inharmonic
    , (40, 0.0009, 5)
    , (40, 0.00079, 7)
    , (40, 0.0004, 9)
    ]
    where
    make (tension, radius, t60) =
        String 0.6985 tension nylon radius (15, t60) 0 [Output 0.9 0.5]

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

-- legongFrets = makeFrets (-0.0005) (head legong) (tail legong)
legongFrets = makeFrets (-0.0005) (head legong) (tail legong)

legong :: [Pitch.NoteNumber]
legong = drop 3 $ map Pitch.nn
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
