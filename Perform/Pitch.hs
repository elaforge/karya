-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Representation for scales, pitches, and frequencies (note numbers).

    There are many representations for a pitch, at many different levels of
    abstraction.
-}
module Perform.Pitch (
    -- * Note
    Note(..), noteText

    -- * Pitch
    , Pitch(..), pitch, Degree(..)
    , Octave, PitchClass, Accidentals, Semi, FSemi, Step
    , pitchAccidentals, pitchPc
    , addOctave, addPc, diffPc
    , middleOctave, middleC

    -- * Input
    , Input(..), KbdType(..), Frac

    -- * NoteNumber
    , NoteNumber(..), nn, nnToDouble
    , Cent, nnToCents
    , nnsEqual, nnsClose

    -- * Hz
    , Hz, addHz, modifyHz, nnToHz, hzToNn, middleCHz

    -- * Scale
    , ScaleId(..), emptyScale, twelve
    , Transpose(..), zeroTranspose, modifyTranspose
    , Key(Key), keyText
) where
import qualified Data.String as String
import qualified Data.Text as Text

import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import qualified Util.Test.ApproxEq as ApproxEq

import qualified Derive.ShowVal as ShowVal

import           Global


-- * Note

-- | A Note is the most abstract representation of pitch, in that it's simply
-- an unparsed bit of text representing that pitch.  Given a Scale, it's
-- expected to name a val call exported by that scale.
newtype Note = Note Text
    deriving (Eq, Ord, Show, String.IsString, Serialize.Serialize)

noteText :: Note -> Text
noteText (Note s) = s

instance Pretty Note where pretty (Note n) = n


-- * pitch

-- | A Pitch is a parsed 'Note'.  Functions that want to manipulate notes
-- in a scale-independent way can ask the scale to convert to and from a Note.
-- Not all scales use all the fields.
data Pitch = Pitch
    { pitchOctave :: !Octave
    , pitchDegree :: !Degree
    } deriving (Eq, Ord, Read, Show)

pitch :: Enum pc => Octave -> pc -> Pitch
pitch oct pc = Pitch oct (Degree (fromEnum pc) 0)

instance Pretty Pitch where
    pretty (Pitch oct degree) = showt oct <> "-" <> pretty degree

-- | This relies on the presence of a @pitch@ val call.
instance ShowVal.ShowVal Pitch where
    showVal (Pitch oct (Degree pc accs)) =
        "\"(pitch " <> Text.unwords args <> ")"
        where args = map showt $ oct : pc : if accs == 0 then [] else [accs]

instance Serialize.Serialize Pitch where
    put (Pitch a b) = Serialize.put a >> Serialize.put b
    get = Pitch <$> Serialize.get <*> Serialize.get

-- | A scale degree, without reference to an octave.
data Degree = Degree
    { degreePc :: !PitchClass
    -- | Ignored for diatonic scales.
    , degreeAccidentals :: !Accidentals
    } deriving (Eq, Ord, Read, Show)

instance Pretty Degree where
    pretty (Degree pc acc) = showt pc
        <> if acc < 0 then Text.replicate (abs acc) "b"
            else Text.replicate acc "#"

instance Serialize.Serialize Degree where
    put (Degree a b) = Serialize.put a >> Serialize.put b
    get = Degree <$> Serialize.get <*> Serialize.get

-- | Just a way to label an octave, either relative or absolute.
type Octave = Int

-- | A PitchClass maps directly to a scale degree, which is a letter in
-- traditional Western notation, though this PitchClass may have fewer or
-- greater than 7 notes.  The PitchClass is absolute in that it doesn't depend
-- on the tonic of a key.
--
-- These numbers are expected to wrap around at the octave, so they usually use
-- modular arithmetic, but if an octave is not handy (i.e. they're not in
-- a 'Pitch'), then steps exceeding an octave will be wrapped into an octave
-- when one is available.
type PitchClass = Int

-- | Positive for sharps, negative for flats.
type Accidentals = Int

-- | Number of semitones.  This is an absolute measure from octave 0,
-- regardless of whether the scale has notes at octave 0.  This is so you can
-- convert between Semis and the similarly absolute 'Pitch' in the same way for
-- all scales.
type Semi = Int

-- | This is like 'Semi', but floating point.
type FSemi = Double

-- | This is a relative amount of transposition.  It could be either chromatic
-- or diatonic.
type Step = Int

pitchAccidentals :: Pitch -> Accidentals
pitchAccidentals = degreeAccidentals . pitchDegree

pitchPc :: Pitch -> PitchClass
pitchPc = degreePc . pitchDegree

addOctave :: Octave  -> Pitch -> Pitch
addOctave oct (Pitch octave degree) = Pitch (oct + octave) degree

-- | Add diatonic steps.  This doesn't deal with key signatures or non-diatonic
-- scales.
addPc :: PitchClass -> PitchClass -> Pitch -> Pitch
addPc perOctave steps (Pitch octave (Degree pc accs)) =
    Pitch (oct + octave) (Degree pc2 accs)
    where (oct, pc2) = (pc + steps) `divMod` perOctave

diffPc :: PitchClass -> Pitch -> Pitch -> PitchClass
diffPc perOctave (Pitch oct1 (Degree pc1 _)) (Pitch oct2 (Degree pc2 _)) =
    octDiff + pc1 - pc2
    where octDiff = perOctave * (oct1 - oct2)

-- | The middle octave.  The \"center\" of a scale should be oriented around
-- this.
middleOctave :: Octave
middleOctave = 4

middleC :: Pitch
middleC = Pitch middleOctave (Degree 0 0)

-- * Input

-- | A physically played note on some input device.  This hasn't been mapped to
-- a scale yet, so the Pitch is in the context of the device's layout.
--
-- I have 3 kinds of kbds:
--
-- ASCII has 10 white keys, and black keys between each one.  It should be
-- relative, so that C or sa is always on Q and Z, and if the octave is <10
-- then it will wrap on the same row.
--
-- MIDI has the usual piano layout.  It's absolute, so that a relative scale
-- can start at keys other than C, if that would be convenient for the layout.
-- The octave is rounded up to the nearest multiple of 7, and the extra keys
-- are unused, so the octave always starts at C.
--
-- Continuum has no keys, just NNs.  So it gets the scale degree that's
-- closest to the given NN.  That's different from the MIDI kbd because the
-- MIDI kbd never wants a key to emit something between notes.  TODO not
-- supported yet
data Input = Input !KbdType !Pitch !Frac
    deriving (Eq, Show)

data KbdType
    -- | An absolute kbd maps the same key to the same absolute pitch,
    -- regardless of the key.  This is the case for a piano style kbd.
    -- This is consistent with convention, but also the piano kbd has a fixed
    -- layout of white and black keys.  So if you e.g. transpose A-major to
    -- start on C, then you have a mysterious black key in between B and C, and
    -- no way to play C#.
    = PianoKbd
    -- | A relative kbd always maps the same key to the same relative pitch.
    -- This is appropriate for the ASCII kbd, because it has \"black keys\"
    -- between every white key, so scales can be transposed freely.
    | AsciiKbd
    deriving (Eq, Show)

-- | A number between -1 and 1 exclusive, representing the portion of the way
-- between two scale degrees.  I could have used \"Cents\" for this, but that
-- implies equal temperedness.
type Frac = Double

instance Pretty Input where
    pretty (Input kbd pitch frac) = showt kbd <> ":" <> pretty pitch
        <> if frac == 0 then "" else "+" <> pretty frac

-- * NoteNumber

-- | This is equal tempered scale notes with the same definition as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz, and is -1c.  Middle C (4c) is
-- NoteNumber 60.
--
-- 'Derive.PSignal.PSignal's are converted into this before performance
-- since performance doesn't understand scales.
newtype NoteNumber = NoteNumber Double
    deriving (ApproxEq.ApproxEq, Eq, Ord, Fractional, Real, RealFrac, Num,
        Serialize.Serialize)

instance Show NoteNumber where
    show (NoteNumber nn) = show nn <> "nn"
instance Read NoteNumber where
    readPrec = do
        n <- Read.readPrec
        Read.lift $ ReadP.skipSpaces >> ReadP.string "nn"
        pure (NoteNumber n)

instance ShowVal.ShowVal NoteNumber where
    showVal (NoteNumber nn) = ShowVal.showVal nn <> "nn"
    -- The suffix should be the same as DeriveT.type_to_code Nn

instance Pretty NoteNumber where
    pretty (NoteNumber nn) = Num.showFloat0 (Just 3) nn <> "nn"

nn :: Real a => a -> NoteNumber
nn = NoteNumber . realToFrac

nnToDouble :: NoteNumber -> Double
nnToDouble (NoteNumber nn) = nn

type Cent = Int

nnToCents :: NoteNumber -> Cent
nnToCents = round . (*100)

-- | True if the NoteNumbers are close enough that they sound the same.
nnsEqual :: NoteNumber -> NoteNumber -> Bool
nnsEqual = nnsClose 3 -- almost certainly less than JND

nnsClose :: Cent -> NoteNumber -> NoteNumber -> Bool
nnsClose cents nn1 nn2 = abs (nnToCents nn1 - nnToCents nn2) <= cents

-- * Hz

-- | This is absolute non-logarithmic frequency.
type Hz = Double

addHz :: Hz -> NoteNumber -> NoteNumber
addHz 0 nn = nn -- hz_to_nn . nn_to_hz adds a tiny bit of inaccuracy
addHz hz nn = hzToNn (hz + nnToHz nn)

modifyHz :: (Hz -> Hz) -> NoteNumber -> NoteNumber
modifyHz f = hzToNn . f . nnToHz

nnToHz :: NoteNumber -> Hz
nnToHz (NoteNumber nn) = exp (nn * _hzScale + _hzOffset)

-- | Negative hz will result in NaN.  TODO take an abs or throw an error, or
-- let the NaN propagate?
hzToNn :: Hz -> NoteNumber
hzToNn hz = NoteNumber $ (log hz - _hzOffset) / _hzScale

-- | Constants to calculate equal tempered conversions.
_hzScale, _hzOffset :: Hz
_hzScale = log 2 / 12
_hzOffset = log aHz - (aNn * _hzScale)
    where
    -- NoteNumber is defined with these values.  Ultimately it's because midi
    -- synthesizers are by default defined with these values.
    aHz = 440
    aNn = 69

middleCHz :: Hz
middleCHz = nnToHz 60

-- Alternate implementation that also introduces a bit of imprecision.  Seems
-- to be about the same as the one above.
--
-- nn_to_hz :: NoteNumber -> Hz
-- nn_to_hz (NoteNumber nn) = a_hz * rt12 ** (nn - a_nn)
--     where rt12 = 2**(1/12)
--
-- hz_to_nn :: Hz -> NoteNumber
-- hz_to_nn hz = NoteNumber $ logBase rt12 (hz / a_hz * (rt12**a_nn))
--     where rt12 = 2**(1/12)
--
-- a_hz, a_nn :: Double
-- a_hz = 440
-- a_nn = 69


-- * scale

newtype ScaleId = ScaleId Text
    deriving (Eq, Ord, Read, Show, String.IsString, Serialize.Serialize)

instance Pretty ScaleId where pretty (ScaleId s) = s

-- | Usually this means to use the scale currently in scope.
emptyScale :: ScaleId
emptyScale = ""

twelve :: ScaleId
twelve = "twelve"

-- | A generic transposition, for operations that can transpose diatonically,
-- chromatically, or by absolute NoteNumber.
data Transpose = Chromatic Double | Diatonic Double
    -- | Nn is scale-independent, so it's not suitable for symbolic
    -- transposition, but it's still useful for pitch transposition.
    | Nn Double
    deriving (Eq, Ord, Show)

instance Pretty Transpose where pretty = ShowVal.showVal
instance ShowVal.ShowVal Transpose where
    -- TODO convert to a ScoreT.Typed Signal.Y and use its ShowVal
    showVal (Chromatic d) = ShowVal.showVal d <> "c"
    showVal (Diatonic d) = ShowVal.showVal d <> "d"
    showVal (Nn d) = ShowVal.showVal d <> "nn"

zeroTranspose :: Transpose -> Bool
zeroTranspose t = case t of
    Chromatic d -> d == 0
    Diatonic d -> d == 0
    Nn d -> d == 0

modifyTranspose :: (Double -> Double) -> Transpose -> Transpose
modifyTranspose f t = case t of
    Chromatic d -> Chromatic (f d)
    Diatonic d -> Diatonic (f d)
    Nn d -> Nn (f d)

-- | Diatonic transposition often requires a Key for context.
--
-- This is not very strongly typed, because it's intended to be scale
-- independent, and not every scale will have the same values for key and
-- mode.
newtype Key = Key Text
    deriving (Eq, Ord, Read, Show, ShowVal.ShowVal, Serialize.Serialize)

keyText :: Key -> Text
keyText (Key t) = t

instance Pretty Key where format (Key s) = Pretty.text s
