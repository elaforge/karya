{-# LANGUAGE RecordWildCards #-}
module Ness.Soundboard where
import Prelude hiding (String)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Tuple as Tuple
import Ness.Global

import Global

{-
. yangqin?
. play string like reyong, with a hand damp
. string like gangsa with noltol
. jawari like tambura or sitar
. solkattu for metal percussion thing
. can I hand damp metal percussion?
-}

-- Can't change any parameters after strike.
-- Also no way to damp a ringing string?  What does Duration do?

-- Collect string definitions from notes, and make instrument.
--
-- Calculate score duration, I guess I have to add some for the decay of the
-- final note.
renderAll :: Score -> (Text, Text)
renderAll (Score plate collision notes) =
    (Text.unlines instrument, Text.unlines score)
    where
    strings = zip [0..] $ Set.toList $ Set.fromList $ map nString notes
    instrument = concat
        [ [ "# sbversion 0.1"
          , "samplerate 44100"
          , ""
          ]
        , map (uncurry renderString) strings
        , [render plate, render collision]
        , [renderStringOutput n o | (n, s) <- strings, o <- sOutputs s]
        , map render (pOutputs plate)
        ]
    nameOf str = stringName $ fromMaybe (error $ "no string: " <> show str) $
        Map.lookup str toNum
        where toNum = Map.fromList $ map Tuple.swap strings
    score = "duration " <> render (maximum (map nEnd notes) + decay)
        : map (renderNote nameOf) notes
    decay = 2

data String = String {
    sLength :: Meters
    , sDensity :: Double -- kg/m
    , sTension :: Newtons
    , sYoung :: Double -- ^ Young's modulus
    , sRadius :: Meters
    , sT60AtDC :: Seconds -- ?
    , sT60At1Khz :: Seconds -- ?
    , sStart :: (Double, Double)
    , sEnd :: (Double, Double)
    , sFrets :: Int
    , sFretHeight :: Meters -- ^ negative, from below strings
    , sBaseboardHeight :: Meters -- ^ below frets
    -- | variation in baseboard profile, small positive
    , sBaseboardProfile :: Double

    , sOutputs :: [StringOutput]
    } deriving (Eq, Ord, Show)

renderString :: Int -> String -> Text
renderString n
       (String len density tension young radius t60dc t601
        (sx, sy) (ex, ey) frets fretHeight baseHeight baseProfile _outputs) =
    Text.unwords $ concat
        [ ["string", stringName n]
        , map render
            [ len, density, tension, young, radius, t60dc, t601
            , sx, sy, ex, ey
            ]
        , [render frets]
        , map render [fretHeight, baseHeight, baseProfile]
        ]

stringName :: Int -> Text
stringName n = "s" <> showt n

data StringOutput = StringOutput {
    sLocation :: Location
    , sPan :: Pan
    } deriving (Eq, Ord, Show)

renderStringOutput :: Int -> StringOutput -> Text
renderStringOutput n (StringOutput location pan) = Text.unwords $ concat
    [ ["string_out", stringName n]
    , map render [location, pan]
    ]

data Plate = Plate {
    pDensity :: Double -- kg/m
    , pThickness :: Meters
    , pYoung :: Double
    , pPoisson :: Double -- ? Poisson's ratio
    , pTension :: Newtons
    , pSize :: (Meters, Meters)
    , pT60AtDC :: Seconds
    , pT60At1Khz :: Seconds

    , pOutputs :: [PlateOutput]
    } deriving (Eq, Ord, Show)

instance Render Plate where
    render (Plate density thickness young poisson tension (w, h) t60dc t601
            _outs) =
        Text.unwords $ "plate" : map render
            [density, thickness, young, poisson, tension, w, h, t60dc, t601]

-- plate
--  density = 7850 thickness = 0.001
--  young = 2e11 0.3
--  tension = 0
--  sizex = 0.5 sizey = 0.2
--  t60dc = 10 9

data PlateOutput = PlateOutput {
    pPosition :: (Location, Location) -- 0 to 1
    , pPan :: Pan
    } deriving (Eq, Ord, Show)

instance Render PlateOutput where
    render (PlateOutput (x, y) pan) = Text.unwords $
        "plate_out" : map render [x, y, pan]

data Collision = Collision {
    cStiffness :: Double
    , cNonlinearityExponent :: Double
    , cIterations :: Int
    } deriving (Eq, Ord, Show)

instance Render Collision where
    render (Collision stiff nonlin iterations) = Text.unwords $
        "collision" : [render stiff, render nonlin, render iterations]

data Strike = Strike | Pluck deriving (Eq, Show)

instance Render Strike where
    render Strike = "strike"
    render Pluck = "pluck"

data Note = Note {
    nStrike :: Strike
    , nString :: String
    , nStart :: Seconds
    , nDuration :: Seconds
    , nForce :: Newtons
    , nLocation :: Location
    } deriving (Eq, Show)

nEnd :: Note -> Seconds
nEnd n = nStart n + nDuration n

renderNote :: (String -> Text) -> Note -> Text
renderNote nameOf (Note typ str start dur force loc) = Text.unwords $
    [render typ, nameOf str] ++ map render [start, dur, force, loc]

data Score = Score {
    sPlate :: Plate
    , sCollision :: Collision
    , sNotes :: [Note]
    } deriving (Eq, Show)

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

collision = Collision
    { cStiffness = 1.793912177273139e+15
    , cNonlinearityExponent = 1.555095115459269
    , cIterations = 50
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

score = Score
    { sPlate = plate
    , sCollision = collision
    , sNotes = map (uncurry note) [(str1, 0), (str2, 1), (str3, 2), (str4, 3)]
        -- [ Note Strike str1 0.001 0.001505957051665
        --     5.736077649819015 0.929263623187228
        -- , Note Pluck str2 0.002 0.001699076722657
        --     3.693122214143396 0.349983765984809
        -- , Note Strike str3 0.003 0.001890903252536
        --     3.746470027795287 0.196595250431208
        -- , Note Pluck str4 0.004 0.001959291425205
        --     4.287541270618682 0.251083857976031
        -- ]
    }
    where
    note str start = Note Pluck str start 0.0015 4 0.9

