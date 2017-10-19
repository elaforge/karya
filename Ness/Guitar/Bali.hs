{-# LANGUAGE RecordWildCards #-}
module Ness.Guitar.Bali where
import Prelude hiding (String)

import qualified Util.Seq as Seq
import qualified Perform.Pitch as Pitch
import Ness.Global
import Ness.Guitar
import qualified Ness.Util as Util


(notes, fingers) = (take 1 each_string, take 1 slide_each_string)

-- (notes, fingers) =
--     (same_string, [each_pitch (head strings) 0.5 (head scale) scale])

same_string = take (length scale)
    [note (head strings) t 0.25 | t <- iterate (+0.5) 0]

each_string = [note str t 0.65 | (str, t) <- zip strings (iterate (+2) 0)]

rolled_strings =
    concat [roll str t 2 | (str, t) <- zip strings (iterate (+2) 0)]
    where
    roll str t dur =
        [ (str, t, dyn)
        | (t, dyn) <- zip (Seq.range' t (t+dur) 0.05) (0.65 : repeat 0.03)
        ]


each_pitch :: String -> Seconds -> Pitch.NoteNumber -> [Pitch.NoteNumber]
    -> Finger
each_pitch str dur open pitches = Finger str (0, 0) notes
    where
    notes = concat $ do
        (p, t) <- zip pitches (iterate (+dur) 0)
        let loc = pitchLocation open p
        return [(t - 0.025, loc, 0.6), (t+dur-eta, loc, 0.6)]
        -- return [(t+eta, loc, 0.6), (t+dur, loc, 0.6)]
    eta = 0.15

slide_each_string =
    [slide str t (t+2) | (str, t) <- zip strings (iterate (+2) 0)]

slide str start end = Finger
    { fString = str
    , fInitial = (0, 0)
    , fMovement = [(start, 0, 0.6), (end, 0.5, 0.6)]
    }

--

note :: String -> Seconds -> Double -> Note
note str start amp = Note
    { nStrike = Strike
    , nString = str
    , nStart = start
    , nDuration = 0.0013
    , nLocation = 0.8
    , nAmplitude = amp
    }

score0 = Score
    { sDecay = 2
    , sHighpass = True
    , sNotes = notes
    , sFingers = fingers
    }

instrument0 = Instrument
    { iSR = 44100
    , iStrings = strings
    , iFrets = frets
    , iBarrier = Barrier 1e10 1.3 10 (Solver 20 1e-12)
    , iBackboard = Backboard (-0.002) (-0.001) (-0.0002)
    , iFingerParams = FingerParams 0.005 1e7 3.3 100
    , iNormalizeOutputs = True
    , iSolver = Solver 20 0
    , iConnections = []
    }

strings = map make
    -- [ (1.5, 10.1, 0.00020, 5, 0.2) -- *
    -- , (1.05, 12.3, 0.00015, 5, 0.3) -- ^

    [ (0.78, 12.1, 0.00020, 5, 0.2) -- *
    , (1.05, 12.3, 0.00015, 5, 0.3) -- ^
    , (1.40, 21.9, 0.00015, 5, 0.4) -- *
    , (1.60, 27.6, 0.00015, 5, 0.5) -- *
    , (1.88, 39.2, 0.00015, 7, 0.6) -- *
    , (3.15, 49.2, 0.00010, 8, 0.7) -- ^
    ]
    where
    make (length, tension, radius, t60, pan) =
        String length tension silk radius (15, t60) [Output 0.9 pan]

frets = makeFrets (-0.0005) (head scale) (tail scale)

scale = minor

minor :: [Pitch.NoteNumber]
minor = map Pitch.nn $ take 21 $ scanl (+) 48 (cycle intervals)
    where
    intervals = [2, 1, 2, 2, 1, 2, 2]

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

Util.Interactive {..} = Util.interactive "guitar-bali" renderAll
    (instrument0, score0)
