-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Ness.Guitar.Patch where
import Prelude hiding (String)
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Control as Control
import Ness.Global
import Ness.Guitar
import Global


c_location :: Control.Control
c_location = "location"

c_finger :: Control.Control
c_finger = "finger"

instruments :: [Instrument]
instruments =
    [ instrument "polos" (-0.0017) legongStrings
    , instrument "sangsih" (-0.0013) legongStrings2
    , instrument "g12-1" (-0.0020) guitarStrings
    , instrument "g12-2" (-0.0015) guitarStrings2
    ]


-- * implementation

instrument name backboard strings = Instrument
    { iName = name
    , iStrings = strings
    , iFrets = [] -- frets
    , iBarrier = Barrier 1e10 1.3 10 (Solver 20 1e-12)
    , iBackboard = Backboard -- a + bx + bx^2, where x is length
        -- distances = [-0.002, -0.0017, -0.0015, -0.0013,  -0.001]
        { ba = backboard
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

guitarStrings = map make
    [ (12.1, 0.00020, 5, NN.e3, 0.1)
    , (12.3, 0.00015, 5, NN.a3, 0.15)
    , (21.9, 0.00015, 5, NN.d4, 0.2)
    , (39.2, 0.00015, 7, NN.g4, 0.25)
    , (27.6, 0.00010, 5, NN.b4, 0.3)
    , (49.2, 0.00010, 8, NN.e5, 0.35)
    ]
    where
    make (tension, radius, t60, nn, pan) = String
        { sLength = 0.68
        , sTension = tension
        , sMaterial = steel
        , sRadius = radius
        , sT60 = (15, t60)
        , sNn = nn
        , sName = NN.karya_name nn
        , sOutputs = [Output 0.9 pan, Output 0.7 (pan + 0.2)]
        }

guitarStrings2 = map make
    [ (12.2, 0.00020, 5, NN.e3, 0.6)
    , (12.4, 0.00015, 5, NN.a3, 0.65)
    , (22.0, 0.00015, 5, NN.d4, 0.7)
    , (39.3, 0.00015, 7, NN.g4, 0.75)
    , (27.7, 0.00010, 5, NN.b4, 0.8)
    , (49.3, 0.00010, 8, NN.e5, 0.85)
    ]
    where
    make (tension, radius, t60, nn, pan) = String
        { sLength = 0.68
        , sTension = tension
        , sMaterial = steel
        , sRadius = radius
        , sT60 = (15, t60)
        , sNn = nn
        , sName = NN.karya_name nn
        , sOutputs = [Output 0.9 (pan - 0.2), Output 0.7 pan]
        }

bassStrings = map make
    [ (4.8, 0.0002, NN.e1)
    , (9.3, 0.0002, NN.a1)
    , (9.2, 0.00015, NN.d2)
    , (10.5, 0.00012, NN.g2)
    ]
    where
    make (tension, radius, nn) = String
        { sLength = 0.88
        , sTension = tension
        , sMaterial = steel
        , sRadius = radius
        , sT60 = (15, 3)
        , sNn = nn
        , sName = NN.karya_name nn
        , sOutputs =  outputsAt 0.5
        }

legongNames :: [(Text, Pitch.NoteNumber)]
legongNames = head legong : drop 5 legong

legongStrings = zipWith withName legongNames $ concat
    [ map (lenBy 0.5 . make) strings
    , map (lenBy 0.25 . make) (drop 2 strings)
    ]
    where
    strings =
        [ (1.56, 11.0, 0.00020, 5, 0.0)
        , (0.78, 11.0, 0.00020, 5, 0.0)
        , (0.78, 08.0, 0.00015, 5, 0.1)
        , (0.78, 08.5, 0.00015, 5, 0.2)
        , (0.78, 14.0, 0.00015, 5, 0.3)
        , (0.78, 15.0, 0.00015, 7, 0.4)
        , (0.78, 16.1, 0.00012, 8, 0.5)
        ]
    make (len, tension, radius, t60, pan) =
        String len tension steel radius (15, t60) 0 "" (outputsAt pan)
    lenBy n str = str { sLength = sLength str * n }

legongStrings2 = zipWith withName legongNames $ concat
    [ map (lenBy 0.5 . make) strings
    , map (lenBy 0.25 . make) (drop 2 strings)
    ]
    where
    strings =
        [ (1.56, 11.1, 0.00020, 5, 0.3)
        , (0.78, 11.1, 0.00020, 5, 0.3)
        , (0.78, 08.2, 0.00015, 5, 0.4)
        , (0.78, 08.7, 0.00015, 5, 0.5)
        , (0.78, 14.2, 0.00015, 5, 0.6)
        , (0.78, 15.3, 0.00015, 7, 0.7)
        , (0.78, 16.7, 0.00012, 8, 0.8)
        ]
    make (len, tension, radius, t60, pan) =
        String len tension steel radius (15, t60) 0 "" (outs pan)
    lenBy n str = str { sLength = sLength str * n }
    outs pan = [Output 0.8 pan, Output 0.6 (pan - 0.2)]

withName (name, nn) str = str { sName = name, sNn = nn }

outputsAt pan = [Output 0.9 pan, Output 0.7 (pan + 0.2)]

(notes, fingers) = (take 1 eachString, take 1 slideEachString)

eachString = [note str t 0.65 | (str, t) <- zip strings (iterate (+2) 0)]

rolledStrings =
    concat [roll str t 2 | (str, t) <- zip strings (iterate (+2) 0)]
    where
    roll str t dur =
        [ (str, t, dyn)
        | (t, dyn) <- zip (Lists.range' t (t+dur) 0.05) (0.65 : repeat 0.03)
        ]


eachPitch :: String -> Seconds -> Pitch.NoteNumber -> [Pitch.NoteNumber]
    -> Finger
eachPitch str dur open pitches = Finger (sName str) (0, 0) notes
    where
    notes = concat $ do
        (p, t) <- zip pitches (iterate (+dur) 0)
        let loc = pitchLocation open p
        return [(t - 0.025, loc, 0.6), (t+dur-eta, loc, 0.6)]
        -- return [(t+eta, loc, 0.6), (t+dur, loc, 0.6)]
    eta = 0.15

slideEachString =
    [slide str t (t+2) | (str, t) <- zip strings (iterate (+2) 0)]

slide str start end = Finger
    { fString = sName str
    , fInitial = (0, 0)
    , fMovement = [(start, 0, 0.6), (end, 0.5, 0.6)]
    }

--

note :: String -> Seconds -> Double -> Note
note str start amp = Note
    { nStrike = Strike
    , nString = sName str
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
    { iName = "i0"
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
    [ (0.78, 12.1, 0.00020, 5, 0.2)
    , (1.05, 12.3, 0.00015, 5, 0.3)
    , (1.40, 21.9, 0.00015, 5, 0.4)
    , (1.60, 27.6, 0.00015, 5, 0.5)
    , (1.88, 39.2, 0.00015, 7, 0.6)
    , (3.15, 49.2, 0.00010, 8, 0.7)
    ]
    where
    make (length, tension, radius, t60, pan) =
        String length tension silk radius (15, t60) 0 "" [Output 0.9 pan]
        -- TODO add sNn and sName

frets = makeFrets (-0.0005) (head scale) (tail scale)

scale = minor

minor :: [Pitch.NoteNumber]
minor = map Pitch.nn $ take 21 $ scanl (+) 48 (cycle intervals)
    where
    intervals = [2, 1, 2, 2, 1, 2, 2]

legong :: [(Text, Pitch.NoteNumber)]
legong = zip names $ map Pitch.nn
    [ 48.73 -- 3i
    , 50.80 -- 3o
    , 51.82 -- 3e, rambat begin
    , 55.70 -- 3u
    , 56.82 -- 3a, trompong begin

    , 60.73 -- 4i
    , 62.80 -- 4o, pemade begin
    , 63.35 -- 4e, reyong begin
    , 67.70 -- 4u
    , 68.20 -- 4a

    , 72.46 -- 5i
    , 73.90 -- 5o, kantilan begin
    , 75.50 -- 5e
    , 79.40 -- 5u, trompong end
    , 80.50 -- 5a

    , 84.46 -- 6i, rambat end, pemade end
    ]
    where
    names = [showt oct <> Text.singleton c | oct <- [3..], c <- "ioeua"]
