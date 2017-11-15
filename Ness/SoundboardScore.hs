-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE RecordWildCards #-}
module Ness.SoundboardScore where
import Prelude hiding (String)

import Ness.Soundboard
import qualified Ness.Util as Util


Util.Interactive {..} = Util.interactive "soundboard" renderAll
    (instrument0, score0)

score0 = Score 4 $
    map (uncurry note) [(str1, 0), (str2, 1), (str3, 2), (str4, 3)]
    where
    note str start = Note
        { nStrike = Strike
        , nString = str
        , nStart = start
        , nDuration = 0.0015
        , nForce = 2
        , nLocation = 0.9
        }

instrument0 = Instrument
    { iStrings = [str1, str2, str3, str4]
    , iPlate = plate
    , iCollision = Collision
        { cStiffness = 1.793912177273139e+15
        , cNonlinearityExponent = 1.555095115459269
        , cIterations = 50
        }
    }

plate = Plate
    { pDensity = 7850
    , pThickness = 0.001
    , pYoung = 2e11
    , pPoisson = 0.3
    , pTension = 0
    , pSize = (0.5, 0.2)
    , pT60AtDC = 10
    , pT60At1Khz = 9
    , pOutputs =
        [ PlateOutput
            (0.075854289563064, 0.779167230102011) 0.137647321744385
        , PlateOutput
            (0.053950118666607, 0.934010684229183) (-0.061218717883588)
        , PlateOutput
            (0.530797553008973, 0.129906208473730) (-0.976195860997517)
        ]
    }

str1, str2, str3, str4 :: String
str1 = String
    { sLength = 0.662944737278636
    , sDensity = 0.020632359246225
    , sTension = 79.150136708685949
    , sYoung = 2.191433389648589e+11
    , sRadius = 0.000384352256525255
    , sT60AtDC = 11.311481398313173
    , sT60At1Khz = 8.357470309715547
    , sStart = (0.655477890177557, 0.276922984960890)
    , sEnd = (0.694828622975817, 0.438744359656398)
    , sFrets = 3
    , sFretHeight = -0.001418729661716
    , sBaseboardHeight = -0.002689803992052
    , sBaseboardProfile = 0.019597439585161
    , sOutputs = []
    }
str2 = String
    { sLength = 0.681158387415124
    , sDensity = 0.020097540404999
    , sTension = 79.297770703985535
    , sYoung = 2.097075129744568e+11
    , sRadius =  0.000483147105037813
    , sT60AtDC = 10.071423357148380
    , sT60At1Khz = 8.515480261156666
    , sStart = (0.171186687811562, 0.046171390631154)
    , sEnd = (0.317099480060861, 0.381558457093008)
    , sFrets = 9
    , sFretHeight = -0.001509373363965
    , sBaseboardHeight = -0.003674776529611
    , sBaseboardProfile = 0.013403857266661
    , sOutputs =
        [ StringOutput 0.549723608291140 0.507458188556991
        , StringOutput 0.917193663829810 (-0.239108306049287)
        ]
    }
str3 = String
    { sLength = 0.525397363258701
    , sDensity = 0.020278498218867
    , sTension = 63.152261633550964
    , sYoung = 2.160056093777760e+11
    , sRadius = 0.000458441465911911
    , sT60AtDC = 11.698258611737554
    , sT60At1Khz = 8.486264936249832
    , sStart = (0.706046088019609, 0.097131781235848)
    , sEnd = (0.950222048838355, 0.765516788149002)
    , sFrets = 8
    , sFretHeight = -0.000552050153997
    , sBaseboardHeight = -0.003762004636883
    , sBaseboardProfile = 0.015852677509798
    , sOutputs = [StringOutput 0.585264091152724 0.514400458221443]
    }
str4 = String
    { sLength = 0.682675171227804
    , sDensity = 0.020546881519205
    , sTension = 79.411855635212305
    , sYoung = 2.028377267725443e+11
    , sRadius = 0.000491898485278581
    , sT60AtDC = 11.867986495515101
    , sT60At1Khz = 7.784454039068336
    , sStart = (0.031832846377421, 0.823457828327293)
    , sEnd = (0.034446080502909, 0.795199901137063)
    , sFrets = 12
    , sFretHeight = -0.001359405353707
    , sBaseboardHeight = -0.003003271896036
    , sBaseboardProfile = 0.012238119394911
    , sOutputs = [StringOutput 0.285839018820374 0.135643281450442]
    }
