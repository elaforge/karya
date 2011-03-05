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
    Pitch(..), Note(..), note_text

    -- * InputKey
    , InputKey(..), Octave, middle_c, middle_octave

    -- * Degree
    , Degree(..), middle_int_degree, middle_degree

    -- * NoteNumber
    , NoteNumber(..), nn

    -- * Hz
    , Hz, add_hz, nn_to_hz, hz_to_nn

    -- * Scale
    , ScaleId(..), default_scale_id, twelve, relative, is_relative
) where
import Perform.PitchSignal (ScaleId(..), relative_scale_id, is_relative,
    Degree(..))


-- There are many representations for pitch.  The types here are ordered
-- from abstract to concrete.  'Degree', 'NoteNumber', and 'Hz' can be relative
-- or absolute, but at the moment no distinctions are made at the type level.

-- * Pitch

-- | The main representation for a pitch.  Scale sharing is enforced by keeping
-- the scale as an ID, which must be looked up in a map.
data Pitch = Pitch {
    pitch_scale :: ScaleId
    , pitch_note :: Note
    } deriving (Eq, Ord, Show)

-- | A Note belongs to a scale and describes a certain note in that scale.
newtype Note = Note String deriving (Eq, Ord, Show)
note_text (Note s) = s

-- * InputKey

-- | A physically played key that hasn't been mapped to a scale yet.
newtype InputKey = InputKey Double deriving (Eq, Ord, Show)
type Octave = Integer

-- | Useful to orient scales around a common center.
middle_c :: InputKey
middle_c = InputKey 60

-- | The middle octave.  The \"center\" of a scale should be oriented around
-- this.
middle_octave :: Octave
middle_octave = 5

-- * Degree

-- | For consistency, scales should roughly center themselves around this
-- degree.  This way you don't need to know the scale to know a good
-- representative note for it, for example for a default pitch.
middle_int_degree :: Integer
middle_int_degree = 60
middle_degree = Degree (fromIntegral middle_int_degree)

-- * NoteNumber

-- | This is equal tempered scale notes with the same definition as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz.  Middle C is NoteNumber 60.
--
-- 'PitchSignal's are converted into this before performance since performance
-- doesn't understand scales.
--
-- It would be less tempered-centric to use hz, but for the moment this seems
-- practical since note numbers are easier to read.
newtype NoteNumber = NoteNumber Double deriving (Eq, Ord, Show, Fractional, Num)

nn :: (Real a) => a -> NoteNumber
nn = NoteNumber . realToFrac

-- * Hz

-- | This is absolute non-logarithmic frequency.  Used only for certain
-- functions.
type Hz = Double

add_hz :: Hz -> NoteNumber -> NoteNumber
add_hz 0 nn = nn -- hz_to_nn . nn_to_hz adds a tiny bit of inaccuracy TODO fix?
add_hz hz nn = hz_to_nn (hz + nn_to_hz nn)

nn_to_hz :: NoteNumber -> Hz
nn_to_hz (NoteNumber nn) = exp (nn * _equal1 + _equal2)

-- | Negative hz will result in NaN.  TODO take an abs or throw an error, or
-- let the NaN propagate?
hz_to_nn :: Hz -> NoteNumber
hz_to_nn hz = NoteNumber $ (log hz - _equal2) / _equal1

-- | Constants to calculate equal tempered conversions.
_equal1, _equal2 :: Hz
_equal1 = log 2 / 12
_equal2 = log a_hz - (a_nn * _equal1)
    where
    -- NoteNumber is defined with these values.  Ultimately it's because midi
    -- synthesizers are by default defined with these values.
    a_hz = 440
    a_nn = 69


-- * scale id

-- | An empty scale defaults to the scale in scope.
default_scale_id :: ScaleId
default_scale_id = ScaleId ""

relative :: ScaleId
relative = relative_scale_id

twelve :: ScaleId
twelve = ScaleId "twelve"
