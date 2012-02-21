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
    -- * Pitch
    Note(..), note_text

    -- * InputKey
    , InputKey(..), Octave, middle_c, middle_octave

    -- * NoteNumber
    , NoteNumber(..), nn

    -- * Hz
    , Hz, add_hz, nn_to_hz, hz_to_nn

    -- * Scale
    , ScaleId(..), empty_scale, twelve
    , Degree(..), Transpose(..)
    , Key(..)
) where
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

-- There are many representations for pitch.  The types here are ordered
-- from abstract to concrete.  'Degree', 'NoteNumber', and 'Hz' can be relative
-- or absolute, but at the moment no distinctions are made at the type level.

-- * Pitch

-- | A Note belongs to a scale and describes a certain note in that scale.
newtype Note = Note String deriving (Eq, Ord, Show)

note_text :: Note -> String
note_text (Note s) = s

-- * InputKey

-- | A physically played key that hasn't been mapped to a scale yet.
newtype InputKey = InputKey Double deriving (Num, Eq, Ord, Show)
type Octave = Int

-- | Useful to orient scales around a common center.
middle_c :: InputKey
middle_c = InputKey 60

-- | The middle octave.  The \"center\" of a scale should be oriented around
-- this.
middle_octave :: Octave
middle_octave = 5

-- * Degree

-- -- | For consistency, scales should roughly center themselves around this
-- -- degree.  This way you don't need to know the scale to know a good
-- -- representative note for it, for example for a default pitch.
-- middle_int_degree :: Integer
-- middle_int_degree = 60
-- middle_degree = Degree (fromIntegral middle_int_degree)

-- * NoteNumber

-- | This is equal tempered scale notes with the same definition as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz.  Middle C is NoteNumber 60.
--
-- 'PitchSignal's are converted into this before performance since performance
-- doesn't understand scales.
--
-- It would be less tempered-centric to use hz, but for the moment this seems
-- practical since note numbers are easier to read.
newtype NoteNumber = NoteNumber Double
    deriving (Eq, Ord, Show, Fractional, Num)

instance Pretty.Pretty NoteNumber where
    pretty (NoteNumber nn) = Pretty.show_float (Just 2) nn ++ "nn"

nn :: (Real a) => a -> NoteNumber
nn = NoteNumber . realToFrac

-- * Hz

-- | This is absolute non-logarithmic frequency.
type Hz = Double

add_hz :: Hz -> NoteNumber -> NoteNumber
add_hz 0 nn = nn -- hz_to_nn . nn_to_hz adds a tiny bit of inaccuracy
add_hz hz nn = hz_to_nn (hz + nn_to_hz nn)

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

newtype ScaleId = ScaleId String
    deriving (Eq, Ord, Read, Show, Serialize.Serialize)

instance Pretty.Pretty ScaleId where
    -- This mirrors TrackLang scale id syntax.
    pretty (ScaleId s) = '*' : s

-- | Usually this means to use the scale currently in scope.
empty_scale :: ScaleId
empty_scale = ScaleId ""

twelve :: ScaleId
twelve = ScaleId "twelve"

-- | Scale steps.  What this means is internal to each scale, but is intended
-- to correspond to chromatic steps in the scale.
newtype Degree = Degree Int
    deriving (Num, Integral, Real, Enum, Eq, Ord, Show)

-- | A generic transposition, for operations that can transpose either
-- diatonically or chromatically.
data Transpose = Chromatic Double | Diatonic Double
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Transpose where
    -- TODO these are tracklang vals, so they should use TrackLang.show_num,
    -- which I suppose should move to a lower level module
    pretty (Chromatic d) = Pretty.show_float (Just 2) d ++ "c"
    pretty (Diatonic d) = Pretty.show_float (Just 2) d ++ "d"

-- | Diatonic transposition often requires a Key for context.
--
-- This is not very strongly typed, because it's intended to be scale
-- independent, and not every scale will have the same values for key and
-- mode.
newtype Key = Key String deriving (Eq, Ord, Read, Show, Serialize.Serialize)
