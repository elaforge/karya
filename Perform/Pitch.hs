-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Representation for scales, pitches, and frequencies (note numbers).

    There are many representations for a pitch, at many different levels of
    abstraction.
-}
module Perform.Pitch (
    -- * Note
    Note(..), note_text

    -- * Pitch
    , Pitch(..), Degree(..)
    , Octave, PitchClass, Accidentals, Semi, Step
    , pitch_accidentals, pitch_pc
    , add_octave, add_pc
    , middle_octave, middle_c

    -- * Input
    , Input(..), KbdType(..), Frac

    -- * NoteNumber
    , NoteNumber(..), nn, nn_to_double

    -- * Hz
    , Hz, add_hz, modify_hz, nn_to_hz, hz_to_nn, middle_c_hz

    -- * Scale
    , ScaleId(..), empty_scale, twelve
    , Transpose(..), modify_transpose
    , Key(Key), key_text
) where
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.ApproxEq as ApproxEq
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import qualified Derive.ShowVal as ShowVal


-- * Note

-- | A Note is the most abstract representation of pitch, in that it's simply
-- an unparsed bit of text representing that pitch.  Given a Scale, it's
-- expected to name a val call exported by that scale.
newtype Note = Note Text deriving (Eq, Ord, Show, String.IsString)

note_text :: Note -> Text
note_text (Note s) = s

instance Pretty.Pretty Note where
    pretty (Note n) = untxt n


-- * pitch

-- | A Pitch is a parsed 'Note'.  Functions that want to manipulate notes
-- in a scale-independent way can ask the scale to convert to and from a Note.
-- Not all scales use all the fields.
data Pitch = Pitch {
    pitch_octave :: !Octave
    , pitch_degree :: !Degree
    } deriving (Eq, Ord, Read, Show)

instance Pretty.Pretty Pitch where
    pretty (Pitch oct degree) = show oct <> "-" <> Pretty.pretty degree

-- | This relies on the presence of a @pitch@ val call.
instance ShowVal.ShowVal Pitch where
    show_val (Pitch oct (Degree pc accs)) =
        "(pitch " <> Text.unwords args <> ")"
        where args = map showt $ oct : pc : if accs == 0 then [] else [accs]

instance Serialize.Serialize Pitch where
    put (Pitch a b) = Serialize.put a >> Serialize.put b
    get = Pitch <$> Serialize.get <*> Serialize.get

-- | A scale degree, without reference to an octave.
data Degree = Degree {
    degree_pc :: !PitchClass
    -- | Ignored for diatonic scales.
    , degree_accidentals :: !Accidentals
    } deriving (Eq, Ord, Read, Show)

instance Pretty.Pretty Degree where
    pretty (Degree pc acc) = show pc
        <> if acc < 0 then replicate (abs acc) 'b' else replicate acc '#'

instance Serialize.Serialize Degree where
    put (Degree a b) = Serialize.put a >> Serialize.put b
    get = Degree <$> Serialize.get <*> Serialize.get

-- | Just a way to label an octave, either relative or absolute.
type Octave = Int

-- | A PitchClass maps directly to a scale degree, which is a letter in
-- traditional Western notation, though this PitchClass may have fewer or
-- greater than 7 notes.  The PitchClass is absolute in that it doesn't depend
-- on the tonic of a key.
type PitchClass = Int

-- | Positive for sharps, negative for flats.
type Accidentals = Int

-- | Number of semitones.  This is a relative measure representing chromatic
-- steps.  If the scale has no concept of chromatic steps, then it's just scale
-- steps.
type Semi = Int

-- | Like Semi, but could be diatonic or chromatic.
--
-- TODO I use PitchClass for diatonic steps, even though it's meant to be an
-- absolute measure.  I don't separate absolute and relative types in general
-- anyway, but perhaps Semi should be merged with Step.
type Step = Int

pitch_accidentals :: Pitch -> Accidentals
pitch_accidentals = degree_accidentals . pitch_degree

pitch_pc :: Pitch -> PitchClass
pitch_pc = degree_pc . pitch_degree

add_octave :: Octave  -> Pitch -> Pitch
add_octave oct (Pitch octave degree) = Pitch (oct + octave) degree

-- | Add diatonic steps.  This doesn't deal with key signatures or non-diatonic
-- scales.
add_pc :: PitchClass -> PitchClass -> Pitch -> Pitch
add_pc per_octave steps (Pitch octave (Degree pc accs)) =
    Pitch (oct + octave) (Degree pc2 accs)
    where (oct, pc2) = (pc + steps) `divMod` per_octave

-- | The middle octave.  The \"center\" of a scale should be oriented around
-- this.
middle_octave :: Octave
middle_octave = 4

middle_c :: Pitch
middle_c = Pitch middle_octave (Degree 0 0)

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

data KbdType =
    -- | An absolute kbd maps the same key to the same absolute pitch,
    -- regardless of the key.  This is the case for a piano style kbd.
    -- This is consistent with convention, but also the piano kbd has a fixed
    -- layout of white and black keys.  So if you e.g. transpose A-major to
    -- start on C, then you have a mysterious black key in between B and C, and
    -- no way to play C#.
    PianoKbd
    -- | A relative kbd always maps the same key to the same relative pitch.
    -- This is appropriate for the ASCII kbd, because it has \"black keys\"
    -- between every white key, so scales can be transposed freely.
    | AsciiKbd
    deriving (Eq, Show)

-- | A number between -1 and 1 exclusive, representing the portion of the way
-- between two scale degrees.  I could have used \"Cents\" for this, but that
-- implies equal temperedness.
type Frac = Double

instance Pretty.Pretty Input where
    pretty (Input kbd pitch frac) = show kbd <> ":" <> Pretty.pretty pitch
        <> if frac == 0 then "" else "+" <> Pretty.pretty frac

-- * NoteNumber

-- | This is equal tempered scale notes with the same definition as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz, and is -1c.  Middle C (4c) is
-- NoteNumber 60.
--
-- 'Derive.PitchSignal.Signal's are converted into this before performance
-- since performance doesn't understand scales.
newtype NoteNumber = NoteNumber Double
    deriving (ApproxEq.ApproxEq, Eq, Ord, Fractional, Real, RealFrac, Num,
        Serialize.Serialize)

instance Show NoteNumber where
    show (NoteNumber nn) = Pretty.show_float0 3 nn ++ "nn"
instance Read NoteNumber where
    readPrec = do
        n <- Read.readPrec
        Read.lift $ ReadP.skipSpaces >> ReadP.string "nn"
        return (NoteNumber n)

instance Pretty.Pretty NoteNumber where
    pretty = show

nn :: (Real a) => a -> NoteNumber
nn = NoteNumber . realToFrac

nn_to_double :: NoteNumber -> Double
nn_to_double (NoteNumber nn) = nn

-- * Hz

-- | This is absolute non-logarithmic frequency.
type Hz = Double

add_hz :: Hz -> NoteNumber -> NoteNumber
add_hz 0 nn = nn -- hz_to_nn . nn_to_hz adds a tiny bit of inaccuracy
add_hz hz nn = hz_to_nn (hz + nn_to_hz nn)

modify_hz :: (Hz -> Hz) -> NoteNumber -> NoteNumber
modify_hz f = hz_to_nn . f . nn_to_hz

nn_to_hz :: NoteNumber -> Hz
nn_to_hz (NoteNumber nn) = exp (nn * _hz_scale + _hz_offset)

-- | Negative hz will result in NaN.  TODO take an abs or throw an error, or
-- let the NaN propagate?
hz_to_nn :: Hz -> NoteNumber
hz_to_nn hz = NoteNumber $ (log hz - _hz_offset) / _hz_scale

-- | Constants to calculate equal tempered conversions.
_hz_scale, _hz_offset :: Hz
_hz_scale = log 2 / 12
_hz_offset = log a_hz - (a_nn * _hz_scale)
    where
    -- NoteNumber is defined with these values.  Ultimately it's because midi
    -- synthesizers are by default defined with these values.
    a_hz = 440
    a_nn = 69

middle_c_hz :: Hz
middle_c_hz = nn_to_hz 60

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

instance Pretty.Pretty ScaleId where
    -- This is the pitch track syntax.
    pretty (ScaleId s) = '*' : untxt s

-- | Usually this means to use the scale currently in scope.
empty_scale :: ScaleId
empty_scale = ""

twelve :: ScaleId
twelve = "twelve"

-- | A generic transposition, for operations that can transpose diatonically,
-- chromatically, or by absolute NoteNumber.
data Transpose = Chromatic Double | Diatonic Double
    -- | Nn is scale-independent, so it's not suitable for symbolic
    -- transposition, but it's still useful for pitch transposition.
    | Nn Double
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Transpose where pretty = untxt . ShowVal.show_val
instance ShowVal.ShowVal Transpose where
    -- TODO convert to a Score.TypedVal and use its ShowVal
    show_val (Chromatic d) = ShowVal.show_val d <> "c"
    show_val (Diatonic d) = ShowVal.show_val d <> "d"
    show_val (Nn d) = ShowVal.show_val d <> "nn"

modify_transpose :: (Double -> Double) -> Transpose -> Transpose
modify_transpose f t = case t of
    Chromatic d -> Chromatic (f d)
    Diatonic d -> Diatonic (f d)
    Nn d -> Nn (f d)

-- | Diatonic transposition often requires a Key for context.
--
-- This is not very strongly typed, because it's intended to be scale
-- independent, and not every scale will have the same values for key and
-- mode.
newtype Key = Key Text deriving (Eq, Ord, Read, Show, Serialize.Serialize)

key_text :: Key -> Text
key_text (Key t) = t

instance Pretty.Pretty Key where
    format (Key s) = Pretty.text (untxt s)
