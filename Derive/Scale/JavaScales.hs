-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Derive.Scale.JavaScales (
    ScaleMap(..)
    , Instrument(..)
    , Absolute(..)
    , makeScale
    , Layout(..)
    , makeLayout
    -- * Format
    , Format(..)
    , cipherAbsolute
    , cipherOctaveRelative
#ifdef TESTING
    , addDiatonic
    , module Derive.Scale.JavaScales
#endif
) where
import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Pitch as Pitch

import           Global
import           Util.Affine


data ScaleMap = ScaleMap
    { layout :: Layout
    , larasMap :: Map Text BaliScales.Laras
    , defaultLaras :: BaliScales.Laras
    , format :: Format
    }

data Instrument = Instrument
    { center :: Pitch.Octave
    , bottom :: Absolute
    -- | Unlike most ranges, this is inclusive, so top is a valid pitch.
    -- This seems more intuitive for instruments, and besides there's no
    -- need for an empty range.
    , top :: Absolute
    } deriving (Show)

checkRange :: Instrument -> Absolute -> Either DeriveT.PitchError Absolute
checkRange inst pitch
    | inst.bottom <= pitch && pitch <= inst.top = Right pitch
    | otherwise = Left $ DeriveT.PitchError $
        "out of instrument range " <> pretty inst.bottom <> " to "
        <> pretty inst.top <> ": " <> pretty pitch

-- | An absolute pitch as parsed from Pitch.Note, so e.g. 1-7 (for 1-7).  This
-- is the "chromatic" representation, while Pitch is the diatonic one, taking
-- pathet into account.
--
-- TheoryFormat has a similar notion of relative to absolute, but it's
-- the other way around, in that Pitch.Pitch is the absolute one, while
-- Pitch.Note is relative.
data Absolute = Absolute !Pitch.Octave !Pitch.PitchClass
    deriving (Show, Eq, Ord)

instance Pretty Absolute where
    pretty (Absolute oct pc) = showt oct <> showt pc

makeScale :: Pitch.ScaleId -> ScaleMap -> Doc.Doc -> Scale.Scale
makeScale scaleId smap doc = Scale.Scale
    { scaleId = scaleId
    , scalePattern = smap.format.pattern
    , scaleSymbols = []
    , scaleTransposers = Scales.standardTransposers
    , scaleRead = \_env -> smap.format.read
    , scaleShow = \_env -> smap.format.show
    -- TODO technically it can change per laras, but can I forbid that?
    , scaleBottom = BaliScales.larasBase smap.defaultLaras
    , scaleLayout = smap.layout.intervals
    , scaleTranspose = transpose smap
    , scaleEnharmonics = Scales.noEnharmonics
    , scaleNoteToCall = noteToCall scale smap
    , scaleInputToNote = inputToNote smap
    , scaleInputToNn = Scales.computedInputToNn
        (inputToNote smap) (noteToCall scale smap)
    -- TODO
    , scaleCallDoc = Scales.scaleDegreeDoc ScaleDegree.scaleDegree
    }
    where
    scale = PSignal.Scale scaleId Scales.standardTransposers

-- TODO rename to Key or Pathet?
data Layout = Layout
    -- | 'Absolute' (chromatic) step on which 'intervals' starts, to
    -- map from Absolute to Pitch and back.
    { start :: Pitch.PitchClass
    , intervals :: Intervals
    , theory :: Theory.Layout -- TODO remove, only BaliScales.semis_to_nn
    -- | Cache diatonic to chromatic mappings.
    , dToC :: Vector.Vector Chromatic
    , cToD :: Vector.Vector (Diatonic, ChromaticSteps)
    -- | This is like d_to_c, except it includes chromatic steps as
    -- accidentals, so it can map from the ascii kbd.
    -- TODO merge with d_to_c.
    , degreeToPc :: Map Pitch.Degree Pitch.PitchClass
    } deriving (Show)

type Intervals = Vector.Vector Pitch.Semi

makeLayout :: Int -> [Pitch.Semi] -> Layout
makeLayout start intervals = Layout
    { start = start
    , intervals = intervalsV
    , theory = Theory.layout intervals
    , dToC = makeDToC intervalsV
    , cToD = makeCToD intervalsV
    , degreeToPc = Map.fromList $ zip toDegree [0..]
    }
    where
    toDegree = makeStepToDegree intervals
    intervalsV = Vector.fromList intervals

makeStepToDegree :: [Pitch.Semi] -> [Pitch.Degree]
makeStepToDegree = concat . zipWith make [0..]
    where make pc acc = map (Pitch.Degree pc) [0 .. acc-1]

lima = makeLayout 0 [1, 1, 2, 1, 2]
barang = makeLayout 1 [1, 2, 1, 1, 2] -- 23567

noteToCall :: DeriveT.Scale -> ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
noteToCall scale smap note = case smap.format.read note of
    Left _ -> Nothing
    Right pitch -> Just $ ScaleDegree.scaleDegree scale
        (pitchNn pitch) (pitchNote pitch)
    where
    pitchNn :: Pitch.Pitch -> Scale.PitchNn
    pitchNn pitch config@(PSignal.PitchConfig _env controls) =
        chromaticToNn smap config $
        pitchToTransposed layout pitch controls
    pitchNote :: Pitch.Pitch -> Scale.PitchNote
    pitchNote pitch (PSignal.PitchConfig _env controls) =
        smap.format.show $ chromaticToPitch layout $
        roundChromatic $ pitchToTransposed layout pitch controls
    layout = smap.layout

-- Adapt to ChromaticScales.SemisToNoteNumber
-- TODO which should use typed FChromatic instead of untyped Pitch.FSemi
chromaticToNn :: ScaleMap -> PSignal.PitchConfig -> FChromatic
    -> Either DeriveT.PitchError Pitch.NoteNumber
chromaticToNn smap config fc =
    BaliScales.semisToNn smap.layout.theory smap.larasMap smap.defaultLaras
        config (toSemis fc)
    where
    -- Adapt to Pitch.FSemi taken by SemisToNoteNumber.  Pitch.Pitch is
    -- relative to layout.start, so I have to add it back Because FSemis are
    -- absolute, while Chromatic is scale-relative.
    toSemis (FChromatic fc) = fc + fromIntegral smap.layout.start

-- | Pitch plus transposition to absolute chromatic.
pitchToTransposed :: Layout -> Pitch.Pitch -> ScoreT.ControlValMap
    -> FChromatic
pitchToTransposed layout pitch controls =
    fc .+^ (octave * perOct + dsteps + csteps)
    where
    fc = fchromatic $ pitchToChromatic layout pitch
    perOct = fromIntegral $ cPerOct layout.intervals
    dsteps = diatonicToChromaticFrac
        layout (pitchToChromatic layout pitch) diatonic
    octave = FChromaticSteps $ get Controls.octave
    csteps = FChromaticSteps $ get Controls.chromatic
    diatonic = FDiatonicSteps $ get Controls.diatonic
    get m = Map.findWithDefault 0 m controls

inputToNote :: ScaleMap -> Scales.InputToNote
inputToNote smap _env (Pitch.Input kbdType pitch _frac) = do
    -- Tonic is to reverse to_absolute, but JavaScales are the opposite way,
    -- displayed absolute, see 'Absolute'.
    let tonic = 0
    pitch <- Scales.kbdToScale kbdType perOctave tonic pitch
    smap.format.show pitch
    where perOctave = dPerOct smap.layout.intervals


-- ** transpose

transpose :: ScaleMap -> Derive.Transpose
transpose smap transposition _env steps pitch = case transposition of
    Scale.Diatonic ->
        Right $ transposeDiatonic smap.layout pitch (DiatonicSteps steps)
    Scale.Chromatic ->
        Right $ transposeChromatic smap.layout pitch (ChromaticSteps steps)

transposeDiatonic :: Layout -> Pitch.Pitch -> DiatonicSteps -> Pitch.Pitch
transposeDiatonic layout pitch steps =
    chromaticToPitch layout $
        addDiatonic layout (pitchToChromatic layout pitch) steps

transposeChromatic :: Layout -> Pitch.Pitch -> ChromaticSteps -> Pitch.Pitch
transposeChromatic layout pitch steps =
    chromaticToPitch layout (pitchToChromatic layout pitch .+^ steps)

pitchToChromatic :: Layout -> Pitch.Pitch -> Chromatic
pitchToChromatic layout (Pitch.Pitch oct (Pitch.Degree pc acc)) =
    layout.dToC Vector.! d
        .+^ ChromaticSteps (acc + (oct+oct2) * cPerOct layout.intervals)
    where (oct2, d) = pc `divMod` dPerOct layout.intervals

chromaticToPitch :: Layout -> Chromatic -> Pitch.Pitch
chromaticToPitch layout c = Pitch.Pitch oct (Pitch.Degree pc acc)
    where
    (Diatonic d, ChromaticSteps acc) = toDiatonic layout c
    (oct, pc) = d `divMod` dPerOct layout.intervals

-- | Convert a fractional number of diatonic steps to chromatic steps, starting
-- from the given chromatic pitch.
--
-- Chromatic is not absolute in that it doesn't include octave, but it is 1-7.
-- I could make absolute easily, or just add octaves back on later.
diatonicToChromaticFrac :: Layout -> Chromatic -> FDiatonicSteps
    -> FChromaticSteps
diatonicToChromaticFrac layout start steps
    | steps == 0 = 0
    | steps > 0 = Num.scale (transpose isteps) (transpose (isteps+1))
        (FChromaticSteps frac)
    | otherwise = Num.scale (transpose (isteps-1)) (transpose isteps)
        (FChromaticSteps (1 - abs frac))
    where
    (isteps, frac) = splitDiatonic steps
    transpose steps = fcsteps $ addDiatonic layout start steps .-. start

-- | Convert diatonic steps to chromatic steps, starting from the given
-- chromatic pitch.
--
-- Not every Chromatic corresponds to a Diatonic.  So it's unclear how to
-- diatonically transpose a note that's not diatonic in the first place.  I've
-- defined that the first diatonic step will take the note to the next
-- Diatonic.
addDiatonic :: Layout -> Chromatic -> DiatonicSteps -> Chromatic
addDiatonic layout start steps
    | steps == 0 = start
    | otherwise = toChromatic layout (d .+^ steps2)
    where
    (d, cs) = toDiatonic layout start
    -- cs is the remainder for a non-diatonic start.  If >0, d has been rounded
    -- down.  So when going down, the first diatonic step is already accounted
    -- for in the round down.
    steps2 = if steps < 0 && cs > 0 then steps + 1 else steps

-- | Cannot convert Chromatic to Diatonic because not every chromatic step is
-- in a scale.  So I need a leftover.
toDiatonic :: Layout -> Chromatic -> (Diatonic, ChromaticSteps)
toDiatonic layout (Chromatic c) =
    (d .+^ DiatonicSteps (oct * dPerOct layout.intervals), cs)
    where
    (d, cs) = layout.cToD Vector.! c2
    (oct, c2) = c `divMod` cPerOct layout.intervals

toChromatic :: Layout -> Diatonic -> Chromatic
toChromatic layout (Diatonic d) =
    layout.dToC Vector.! d2 .+^ ChromaticSteps (oct * cPerOct intervals)
    where
    (oct, d2) = d `divMod` dPerOct intervals
    intervals = layout.intervals

-- | This should return DiatonicSteps, but only one caller wants that.
dPerOct :: Intervals -> Int
dPerOct = Vector.length

-- | This should return ChromaticSteps, but only one caller wants that.
cPerOct :: Intervals -> Int
cPerOct = Vector.sum

makeDToC :: Intervals -> Vector.Vector Chromatic
makeDToC = Vector.map Chromatic . Vector.scanl (+) 0

makeCToD :: Intervals -> Vector.Vector (Diatonic, ChromaticSteps)
makeCToD intervals = Vector.fromList
    [ (Diatonic pc, ChromaticSteps acc)
    | (pc, steps) <- zip [0..] (Vector.toList intervals)
    , acc <- Lists.range' 0 steps 1
    ]

-- * conversions

roundChromatic :: FChromatic -> Chromatic
roundChromatic (FChromatic fc) = Chromatic (round fc)

fchromatic :: Chromatic -> FChromatic
fchromatic (Chromatic a) = FChromatic (fromIntegral a)

fcsteps :: ChromaticSteps -> FChromaticSteps
fcsteps (ChromaticSteps a) = FChromaticSteps (fromIntegral a)

fdiatonic :: DiatonicSteps -> FDiatonicSteps
fdiatonic (DiatonicSteps a) = FDiatonicSteps (fromIntegral a)

splitDiatonic :: FDiatonicSteps -> (DiatonicSteps, Double)
splitDiatonic (FDiatonicSteps d) = first DiatonicSteps (properFraction d)


-- * Format

data Format = Format
    { show :: Pitch.Pitch -> Either DeriveT.PitchError Pitch.Note
    , read :: Pitch.Note -> Either DeriveT.PitchError Pitch.Pitch
    , pattern :: Text
    }

cipherAbsolute :: Layout -> Format
cipherAbsolute layout = Format
    { show = showAbsolute layout
    , read = readAbsolute layout
    , pattern = "[0-9][1-" <> showt (cPerOct layout.intervals) <> "]"
    }

cipherOctaveRelative :: Layout -> Instrument -> Format
cipherOctaveRelative layout inst = Format
    { show = showDottedCipher layout inst
    , read = readDottedCipher layout inst.center
    , pattern = degree <> "|`" <> degree <> "[.^]+`"
    }
    where
    degree = "[1-" <> showt pcPerOctave <> "]"
    pcPerOctave = cPerOct layout.intervals

showDottedCipher :: Layout -> Instrument -> Pitch.Pitch
    -> Either PSignal.PitchError Pitch.Note
showDottedCipher layout inst pitch = do
    Absolute oct pc <- checkRange inst
        =<< maybe (Left DeriveT.InvalidInput) Right (toAbsolute layout pitch)
    let delta = oct - inst.center
    let degree = showt pc
    pure $ Pitch.Note $ if delta == 0 then degree else mconcat
        [ "`", degree
        , Text.replicate (abs delta) (if delta > 0 then "^" else ".")
        , "`"
        ]

readDottedCipher :: Layout -> Pitch.Octave -> Pitch.Note
    -> Either PSignal.PitchError Pitch.Pitch
readDottedCipher layout center =
    parse $ (mkpitch 0 =<< parsePc) <|> pWithOctave
    where
    pWithOctave = do
        A.char '`'
        pc <- parsePc
        octs <- A.many' $ A.satisfy $ \c -> c == '.' || c == '^'
        A.char '`'
        let oct = Lists.count (=='^') octs - Lists.count (=='.') octs
        mkpitch oct pc
    mkpitch oct pc = pure $ fromAbsolute layout (Absolute (oct+center) pc)
    parsePc = pPc (cPerOct layout.intervals)

showAbsolute :: Layout -> Pitch.Pitch -> Either PSignal.PitchError Pitch.Note
showAbsolute layout pitch = do
    -- show_pitch is used by input_to_note, InvalidInput is correct for that at
    -- least.
    Absolute oct pc <- maybe (Left DeriveT.InvalidInput) Right $
        toAbsolute layout pitch
    pure $ Pitch.Note $ showt oct <> showt pc

readAbsolute :: Layout -> Pitch.Note -> Either PSignal.PitchError Pitch.Pitch
readAbsolute layout note = do
    absolute <- parse
        (Absolute <$> pOctave <*> pPc (cPerOct layout.intervals)) note
    pure $ fromAbsolute layout absolute

fromAbsolute :: Layout -> Absolute -> Pitch.Pitch
fromAbsolute layout (Absolute oct pc) =
    Pitch.Pitch (oct+oct2) (Pitch.Degree d cs)
    where
    (oct2, pc2) = (pc - 1 - layout.start) `divMod` cPerOct layout.intervals
    (Diatonic d, ChromaticSteps cs) = layout.cToD Vector.! pc2

toAbsolute :: Layout -> Pitch.Pitch -> Maybe Absolute
toAbsolute layout (Pitch.Pitch oct degree) =
    case Map.lookup degree layout.degreeToPc of
        Nothing -> Nothing
        Just pc -> Just $ Absolute (oct+oct2) (pc2+1)
            where
            (oct2, pc2) = (pc + layout.start)
                `divMod` cPerOct layout.intervals

parse :: A.Parser a -> Pitch.Note -> Either DeriveT.PitchError a
parse p note = maybe (Left $ DeriveT.UnparseableNote note) Right $
    ParseText.maybeParse p (Pitch.noteText note)

pPc :: Pitch.PitchClass -> A.Parser Pitch.PitchClass
pPc pcPerOct = pDigit 1 (pcPerOct+1)

pOctave :: A.Parser Pitch.Octave
pOctave = pDigit 0 10

pDigit :: Int -> Int -> A.Parser Int
pDigit low high = do
    n <- maybe mzero pure . Num.readDigit =<< A.satisfy ParseText.isDigit
    guard $ Num.inRange low high n
    pure n
