-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Representation for scales, pitches, and frequencies (note numbers).

    Pitches have a scale, and can be transposed via their scale.  However, they
    can't be interpolated since the scale may be irregular.

    To create smooth pitch interpolations and of course to perform the pitch,
    you need a NoteNumber, which is just a logarithmic representation of
    a frequency.  However, once the pitch becomes a NoteNumber it can no
    longer be diatonically transposed since the scale information is lost.
-}
module Perform.Pitch (
    Octave
    -- * Pitch
    , Note(..), note_text

    -- * Input
    , Input(..), KbdType(..), Frac
    , middle_octave, middle_c

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
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.ApproxEq as ApproxEq
import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import qualified Derive.Scale.Theory as Theory
import qualified Derive.ShowVal as ShowVal


-- There are many representations for pitch.  The types here are ordered
-- from abstract to concrete.  'NoteNumber' and 'Hz' can be relative
-- or absolute, but at the moment no distinctions are made at the type level.

-- | Just a way to label an octave, either relative or absolute.
type Octave = Int

-- * Note

-- | A Note belongs to a scale and describes a certain note in that scale.
newtype Note = Note Text deriving (Eq, Ord, Show)

note_text :: Note -> Text
note_text (Note s) = s

instance Pretty.Pretty Note where
    pretty (Note n) = untxt n

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
-- MIDI has the usual layout.  It's absolute, so that a relative scale can
-- start at keys other than C, if that would be convenient for the layout.
-- The octave is rounded up to the nearest multiple of 7, and the extra keys
-- are unused, so the octave always starts at C.
--
-- Continuum has no keys, just NNs.  So it gets the scale degree that's
-- closest to the given NN.  That's different from the MIDI kbd because the
-- MIDI kbd never wants a key to emit something between notes.  TODO not
-- supported yet
data Input = Input !KbdType !Theory.Pitch !Frac
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

-- | The middle octave.  The \"center\" of a scale should be oriented around
-- this.
middle_octave :: Octave
middle_octave = 4

middle_c :: Theory.Pitch
middle_c = Theory.Pitch middle_octave (Theory.Note 0 0)

-- * NoteNumber

-- | This is equal tempered scale notes with the same definition as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz, and is -1c.  Middle C (4c) is
-- NoteNumber 60.
--
-- 'PitchSignal's are converted into this before performance since performance
-- doesn't understand scales.
--
-- It would be less tempered-centric to use hz, but for the moment this seems
-- practical since note numbers are easier to read.
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
