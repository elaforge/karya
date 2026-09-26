-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JustScales where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import           Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Doc as Doc
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch

import           Global


type Ratios = Vector.Vector (Ratio.Ratio Int)

-- | Hz frequency of 1/1.
justBaseControl :: ScoreT.Control
justBaseControl = "just-base"

-- | Bundle up data needed to construct a just scale.
data ScaleMap = ScaleMap
    { smapFmt :: TheoryFormat.Format
    , smapKeys :: Keys
    , smapDefaultKey :: Key
    -- | Use this Tuning if there is no EnvKey.tuning.  If Nothing, don't even
    -- look for EnvKey.tuning, and use "" for 'key_ratios'.
    , smapDefaultTuning :: Maybe Tuning
    -- | Previously I would default to the 12TET of the tonic when just-base
    -- isn't set, but that doesn't work when the scale doesn't use 12TET names.
    , smapDefaultBaseHz :: Pitch.Hz
    , smapNamedIntervals :: ScaleDegree.NamedIntervals
    -- | Sharps multiply by this, flats divide by this.
    --
    -- While these scales recognize sharps and flats, and 'input_to_note' will
    -- create them, they are still considered diatonic scales, and the
    -- accidentals are not considered part of the scale.  So chromatic
    -- transposition is treated the same as diatonic transposition, and you
    -- can't root a key on a note with an accidental.
    , smapAccidentalInterval :: Double
    }

type Tuning = Text
type Keys = Map Pitch.Key Key

-- | Make a just scale with the given set of keys.  A \"key\" in a just scale
-- is the set of ratios and the tonic.  The number of ratios should be the same
-- as the number of scale degrees as defined by the 'TheoryFormat.Format'.  If
-- there are too many, the extras will never be reached, if too few, they'll
-- wrap around.
scaleMap :: Keys -> Key -> Maybe Tuning -> TheoryFormat.Format -> ScaleMap
scaleMap keys defaultKey defaultTuning fmt = ScaleMap
    { smapFmt = fmt
    , smapKeys = keys
    , smapDefaultKey = defaultKey
    , smapDefaultTuning = defaultTuning
    , smapDefaultBaseHz = Pitch.nnToHz NN.middleC
    , smapNamedIntervals = defaultNamedIntervals
    , smapAccidentalInterval = 16 / 15
    }

-- | TODO this should be 'repeat 2', but as long as this is also controlling
-- the input layout, it has to be piano layout, or it won't work properly with
-- kbd and midi entry.
layout :: Theory.Layout
layout = Theory.pianoLayout

makeScale :: Pitch.ScaleId -> ScaleMap -> Doc.Doc -> [(Doc.Doc, Doc.Doc)]
    -> Scale.Scale
makeScale scaleId smap doc docFields = Scale.Scale
    { scaleId = scaleId
    , scalePattern = TheoryFormat.fmtPattern fmt
    , scaleSymbols = []
    , scaleTransposers = Scales.standardTransposers
    , scaleRead = TheoryFormat.readPitch fmt . Scales.environKey
    , scaleShow = TheoryFormat.scaleShowPitch fmt . Scales.environKey
    , scaleBottom = Theory.semisToPitchSharps layout $ Theory.nnToSemis 0
    , scaleLayout = Scale.diatonicLayout $ TheoryFormat.fmtPcPerOctave fmt
    , scaleTranspose = transpose fmt
    , scaleEnharmonics = enharmonics layout fmt
    , scaleNoteToCall = noteToCall scale smap
    , scaleInputToNote = inputToNote smap
    , scaleInputToNn = Scales.computedInputToNn (inputToNote smap)
        (noteToCall scale smap)
    , scaleCallDoc = Scales.annotateCallDoc Scales.standardTransposers
        allDoc docFields dummyCall
    }
    where
    allDoc = mconcat [doc, "\n", defaultsDoc, "\n", justDoc]
    defaultsDoc = Texts.join ", " $
        [ "Defaults: " <> ShowVal.doc justBaseControl <> ": "
            <> Doc.pretty (smapDefaultBaseHz smap)
        -- TODO This should be the symbolic key name, but I don't have it here.
        , "key: " <> Doc.pretty (smapDefaultKey smap)
        ] ++ case smapDefaultTuning smap of
            Nothing -> []
            Just deflt -> ["tuning: " <> ShowVal.doc deflt]
    scale = PSignal.Scale scaleId Scales.standardTransposers
    dummyCall = Scales.scaleDegreeDoc $ \scale ->
        ScaleDegree.scaleDegreeJust scale (smapNamedIntervals smap) 0
    fmt = smapFmt smap

-- | Call doc that assumes I'm using 'default_named_intervals'.
defaultCallDoc :: Derive.DocumentedCall
defaultCallDoc = Scales.scaleDegreeDoc $ \scale ->
    ScaleDegree.scaleDegreeJust scale defaultNamedIntervals 0

-- | Group keys and format them into fields suitable to pass to 'make_scale'.
-- The 'Key's are expected to be relative, so their 'key_tonic's are ignored.
groupRelativeKeys :: [(Pitch.Key, Key)] -> [(Doc.Doc, Doc.Doc)]
groupRelativeKeys = mapMaybe fmt . Lists.groupStable snd
    where
    fmt ((name, key) :| rest) =
        Just (fmtNames (name : map fst rest),
            Doc.Doc $ showRatios (keyRatios key))
    fmtNames = Doc.commas . map ShowVal.doc

showRatios :: Map Tuning Ratios -> Text
showRatios tunings = Text.intercalate "; "
    [ Texts.join2 ": " tuning (ratios r)
    | (tuning, r) <- Map.toList tunings
    ]
    where
    ratios = Text.intercalate ", " . map Pretty.improperRatio . Vector.toList

justDoc :: Doc.Doc
justDoc =
    "\nJust scales are tuned by ratios from a base frequency, taken from the "
    <> ShowVal.doc justBaseControl
    <> " control, as hz. Typically the \"key\" will select the set\
    \ of ratios used, dependent on the scale.\
    \\nJust scales recognize accidentals as an offset by a fixed ratio,\
    \ but are inherently diatonic, so chromatic transposition is the same\
    \ as diatonic transposition.\
    \\nIf the scale has tuning variations, they are set with the "
    <> ShowVal.doc EnvKey.tuning <> " env var."

-- * input_to_note

enharmonics :: Theory.Layout -> TheoryFormat.Format -> Derive.Enharmonics
enharmonics layout fmt env note = do
    let key = Scales.environKey env
    pitch <- TheoryFormat.readPitch fmt key note
    pure $ map (TheoryFormat.showPitch fmt key) $
        Theory.enharmonicsOf layout pitch

inputToNote :: ScaleMap -> Scales.InputToNote
inputToNote smap env (Pitch.Input kbdType pitch _frac) = do
    key <- readKey smap (Scales.environKey env)
    pitch <- Scales.kbdToScale kbdType pcPerOctave (keyTonic key) pitch
    pure $ TheoryFormat.showPitch (smapFmt smap) Nothing pitch
    where pcPerOctave = TheoryFormat.fmtPcPerOctave (smapFmt smap)


-- * transpose

transpose :: TheoryFormat.Format -> Derive.Transpose
transpose fmt _transposition _key steps pitch =
    pure $ Pitch.addPc perOct steps pitch
    where perOct = TheoryFormat.fmtPcPerOctave fmt

-- * note_to_call

-- | To modulate to another scale: @just-base = (hz (4g)) | key = g-maj@
-- The order is important, so the @(hz (4g))@ happens in the context of the old
-- key.
noteToCall :: PSignal.Scale -> ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
noteToCall scale smap note =
    case TheoryFormat.readRelativePitch fmt note of
        Left _ -> ScaleDegree.scaleDegreeInterval
            scale (smapNamedIntervals smap) note
        Right relative -> Just $ ScaleDegree.scaleDegreeJust
                scale (smapNamedIntervals smap)
                (smapAccidentalInterval smap ^^ acc)
                (pitchNn smap stripped) (pitchNote fmt stripped)
            where
            (acc, stripped) = case relative of
                TheoryFormat.RelativePitch oct pc acc ->
                    (fromMaybe 0 acc,
                        TheoryFormat.RelativePitch oct pc (Just 0))
    where fmt = smapFmt smap

pitchNn :: ScaleMap -> TheoryFormat.RelativePitch -> Scale.PitchNn
pitchNn smap relative (PSignal.PitchConfig env controls) = do
    key <- readKey smap $ Scales.environKey env
    tuning <- case smapDefaultTuning smap of
        Nothing -> pure ""
        Just deflt -> Scales.readEnviron id (Just deflt) EnvKey.tuning env
    ratios <- tryJust
        (DeriveT.EnvironError EnvKey.tuning
            (Just $ "unknown: " <> pretty tuning))
        (Map.lookup tuning (keyRatios key))
    pitch <- TheoryFormat.fmtToAbsolute (smapFmt smap)
        (Scales.environKey env) relative
    when (baseHz == 0) $
        Left $ PSignal.ControlError justBaseControl "==0"
    let hz = transposeToHz perOctave baseHz
            (keyTonic key) ratios
            (octave * fromIntegral perOctave + chromatic + diatonic) pitch
    pure $ Pitch.hzToNn hz
    where
    perOctave = TheoryFormat.fmtPcPerOctave (smapFmt smap)
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls
    baseHz = Map.findWithDefault (smapDefaultBaseHz smap)
        justBaseControl controls

pitchNote :: TheoryFormat.Format -> TheoryFormat.RelativePitch
    -> Scale.PitchNote
pitchNote fmt relative (PSignal.PitchConfig env controls) = do
    let maybeKey = Scales.environKey env
    -- TODO wait, shouldn't I display the relative pitch?
    pitch <- TheoryFormat.fmtToAbsolute fmt maybeKey relative
    let transposed = Pitch.addPc
            (TheoryFormat.fmtPcPerOctave fmt)
            (round (chromatic + diatonic)) pitch
    Right $ TheoryFormat.showPitch fmt maybeKey transposed
    where
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls

transposeToHz :: Pitch.PitchClass -> Pitch.Hz -> TheoryFormat.Tonic -> Ratios
    -> Double -> Pitch.Pitch -> Pitch.Hz
transposeToHz perOct baseHz tonic ratios fracSteps pitch =
    Num.scale hz1 hz2 frac
    where
    -- The 'frac' must be positive for it to fall between pitch1 and pitch2.
    (steps, frac) = splitFraction fracSteps
    pitch1 = Pitch.addPc perOct (steps - tonic) pitch
    pitch2 = Pitch.addPc perOct 1 pitch1
    hz1 = degreeToHz ratios baseHz pitch1
    hz2 = degreeToHz ratios baseHz pitch2

-- | Like 'properFraction', but the fraction is always positive.
splitFraction :: (RealFrac a, Integral b) => a -> (b, a)
splitFraction frac
    | f < 0 = (i - 1, f + 1)
    | otherwise = (i, f)
    where (i, f) = properFraction frac

-- | Given a Key, convert a pitch in that key to its hz value, calculated as
-- a ratio from the given base hz.
degreeToHz :: Ratios -> Pitch.Hz
    -> Pitch.Pitch -- ^ should be relative to scale's tonic
    -> Pitch.Hz
degreeToHz ratios baseHz pitch = base * realToFrac ratio
    where
    base = adjustedBase * 2 ^^ Pitch.pitchOctave pitch
    -- Normalize the base_hz to lie within the first octave.
    -- Add an octave because of NOTE [middle-c].
    adjustedBase = Pitch.nnToHz $ Pitch.hzToNn baseHz `Num.fmod` 12 + 12
    ratio = indexMod ratios $ Pitch.degreePc (Pitch.pitchDegree pitch)

indexMod :: Vector.Vector a -> Int -> a
indexMod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

-- * key

data Key = Key
    { keyTonic :: TheoryFormat.Tonic
    , keyRatios :: Map Tuning Ratios
    } deriving (Eq, Show)

instance Pretty Key where
    pretty (Key tonic ratios) =
        "(Key " <> showt tonic <> " [" <> showRatios ratios <> "])"

readKey :: ScaleMap -> Maybe Pitch.Key -> Either DeriveT.PitchError Key
readKey smap = Scales.getKey (smapDefaultKey smap) (smapKeys smap)

makeKeys :: [Text] -> [(Text, [(Tuning, Ratios)])] -> Keys
makeKeys degrees keyTuningRatios = Map.fromList
    [ (Pitch.Key (degree <> "-" <> key),
        (Key tonic (Map.fromList tuningRatios)))
    | (key, tuningRatios) <- keyTuningRatios
    , (degree, tonic) <- zip degrees [0..]
    ]

-- * format

makeRelativeFmt :: Keys -> Key -- ^ only 'key_tonic' is used
    -> TheoryFormat.RelativeFormat TheoryFormat.Tonic
makeRelativeFmt keys defaultKey = TheoryFormat.RelativeFormat
    { relConfig = TheoryFormat.defaultConfig
    , relKeyConfig = TheoryFormat.KeyConfig
        { keyParse = fmap keyTonic . Scales.getKey defaultKey keys
        , keyDefault = 0
        }
    , relShowDegree = TheoryFormat.showDegreeDiatonic
    , relToAbsolute = TheoryFormat.diatonicToAbsolute
    }

-- * named intervals

defaultNamedIntervals :: ScaleDegree.NamedIntervals
defaultNamedIntervals = Map.fromList
    [ ("m2-", 25 % 24) -- 71, 5-limit minor half-step
    , ("m2", 16 % 15) -- 112, 5-limit major half-step
    , ("M2-", 10 % 9) -- 182, minor whole tone
    , ("M2", 9 % 8) -- 204, 5-limit major second
    , ("m3", 6 % 5) -- 316, 5-limit minor third
    , ("M3", 5 % 4) -- 386, 5-limit major third
    , ("P4", 4 % 3) -- 498, perfect fourth
    , ("tt11", 11 % 8) -- 551, undecimal tritone
    , ("tt7-", 7 % 5) -- 583, septimal tritone
    , ("tt", 45 % 32) -- 590, high 5-limit tritone
    , ("tt7+", 10 % 7) -- 618, septimal tritone
    , ("wolf", 40 % 27) -- 681, wolf 5-limit 5th
    , ("P5", 3 % 2) -- 702, perfect fifth
    , ("m6", 8 % 5) -- 5-limit minor sixth
    , ("M6", 5 % 3) -- 5-limit major sixth
    , ("m7", 9 % 5) -- 5-limit large minor seventh
    , ("M7", 15 % 8) -- 5-limit major seventh
    ]


{- Retuning scales:

    build :: Ratios -> Pitch.Degree -> Pitch.Degree -> Pitch.Hz -> [Pitch.Hz]
    build ratios per_oct degree base =
        List.sort $ map calc (zip [degree..] (Vector.toList ratios))
        where
        calc (d, rat)
            | d >= per_oct = f / 2
            | otherwise = f
            where f = base * realToFrac rat

    interpolated ratios per_oct degree from_base to_base frac =
        where
        v1 = build ratios per_oct degree from_base
        v2 = build ratios per_oct degree to_base

             A       B       C       D       E       F       G       A
    A440     440     495     550     586.6   660     773.3   825     880
    C550     458.3   515.625 550     618.75  687.5   733.3   825     916.6
    A458.3   458.3   515.587 527.875 611.06  687.45  763.83  859.312 970.6

    So if I'm going A->C I can't go through B.  I have to figure out the
    frequencies for C550 and then interpolate to those.

    The tuning pitch must be [(C, A, 0.5)].  So it starts A440, then has C.
    C maps to ratio scale of 5:4, so we set
    (-base = 440, -from = 'a', -to = 550, %-frac = %)
    (-base = 550, -from = 'c', -to = 458.3, %-frac = %)

    What I really need is base_from and base_to, where base is always A.  So
    if the tuning pitch says C then I have to figure out the A of C, based on
    the previous frequency.  I think I can do this if

    But how could this work if pitches are moving up and down?

    f0 = build major_ratios 7
    b1 = f0 0 440
    b2 = f0 2 550
    b3 = f0 0 (458 + 1/3)
-}
