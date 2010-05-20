{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Representation for scales, pitches, and frequencies (note numbers).

    Pitches have a scale, and can be transposed via their scale.  However, they
    can't be interpolated since the scale may be irregular.

    To create smooth pitch interpolations and of course to perform the pitch,
    you need a NoteNumber, which is just a logarithmic representation of
    a frequency.  However, once the pitch becomes a NoteNumber it can no
    longer be diatonically transposed since the scale information is lost.

    C1 = 36
-}
module Perform.Pitch where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


-- There are many representations for pitch.  The types here are ordered
-- from abstract to concrete.  'Degree', 'NoteNumber', and 'Hz' can be relative
-- or absolute, but at the moment no distinctions are made at the type level.

-- ** Pitch

-- | The main representation for a pitch.  Scale sharing is enforced by keeping
-- the scale as an ID, which must be looked up in a map.
data Pitch = Pitch {
    pitch_scale :: ScaleId
    , pitch_note :: Note
    } deriving (Eq, Ord, Show)

-- | Verify that the string is a valid note in the scale, and return the Pitch.
pitch :: Scale -> String -> Maybe Pitch
pitch scale note_s
    | note_in_scale scale note = Just (Pitch (scale_id scale) note)
    | otherwise = Nothing
    where note = Note note_s

-- | A Note belongs to a scale and describes a certain note in that scale.
newtype Note = Note String deriving (Eq, Ord, Show)
note_text (Note s) = s

-- ** InputKey

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

-- ** Degree

-- | This is a pitch in a certain scale, but the actual frequency can't be
-- known untill it's applied to a scale.
-- PitchSignals use this type.
newtype Degree = Degree Double deriving (Eq, Ord, Show, Num)

-- | For consistency, scales should roughly center themselves around this
-- degree.  This way you don't need to know the scale to know a good
-- representative note for it, for example for a default pitch.
middle_int_degree :: Int
middle_int_degree = 60
middle_degree = Degree (fromIntegral middle_int_degree)

-- ** NoteNumber

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

-- ** Hz

-- | This is absolute non-logarithmic frequency.  Used only for certain
-- functions.
type Hz = Double

add_hz :: Hz -> NoteNumber -> NoteNumber
add_hz 0 nn = nn -- hz_to_nn . nn_to_hz adds a tiny bit of inaccuracy TODO fix?
add_hz hz nn = hz_to_nn (hz + nn_to_hz nn)

nn_to_hz :: NoteNumber -> Hz
nn_to_hz (NoteNumber nn) = exp (nn * _equal1 + _equal2)

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


-- * scale

-- | Tie together Pitches and their Scales.
type ScaleMap = Map.Map ScaleId Scale

newtype ScaleId = ScaleId String deriving (Eq, Ord, Read, Show)

-- | An empty scale defaults to the scale in scope.
default_scale_id :: ScaleId
default_scale_id = ScaleId ""

data Scale = Scale {
    scale_id :: ScaleId
    -- | A pattern describing what the scale notes look like.  Used only for
    -- error msgs (i.e. parse errors) so it should be human readable and
    -- doesn't have to follow any particular syntax.  A regex is recommended
    -- though.
    , scale_pattern :: String

    -- | How many integral Degrees are there in an octave?  This is so
    -- that relative pitch notation, which includes an octave, can generate
    -- a PitchSignal.Relative, which doesn't.
    --
    -- This precludes fancy things like variably sized octaves, but that
    -- requires putting an octave in PitchSignal.Relative and letting the scale
    -- provide 'PitchSignal.sig_add'.
    --
    -- If this is zero, this scale has no concept of an octave.
    , scale_octave :: Octave

    -- | Used by derivation.
    , scale_note_to_degree :: Note -> Maybe Degree

    -- | Used by note input.
    , scale_input_to_note :: InputKey -> Maybe Note
    -- | Used by MIDI thru.  This is a shortcut for
    -- @degree_to_nn . note_to_degree . input_to_note@ but can be implemented
    -- more efficiently by the scale.
    , scale_input_to_nn :: InputKey -> Maybe NoteNumber

    -- | Used by conversion before performance.
    , scale_degree_to_nn :: Degree -> Maybe NoteNumber

    -- | A special hack for midi, since it needs additional pitch bend msgs
    -- to play a non-tempered scale (well, there is a midi tuning standard, but
    -- it's not widely supported).  If this is false, never emit pitch bends,
    -- which assumes the scale is tempered, but won't mess with an existing
    -- pitch bend setting.
    --
    -- TODO a better solution might be to always send pitch bend, but add it to
    -- the current pb state so I can keep using the wheel on a non-tempered
    -- scale.  This would also resolve the Midi.Play sproing problem, but is
    -- somewhat complicated since I have to remember pitch bend state.  I don't
    -- think I'll use the wheel much anyway.
    , scale_set_pitch_bend :: Bool
    }

-- | These instances are just so TrackLang.Val can have VScale.  You could
-- argue that it should store ScaleId instead of scale, but looking up the
-- scale all the time seems like a hassle.
instance Eq Scale where
    s0 == s1 = scale_id s0 == scale_id s1

instance Show Scale where
    show scale = "<" ++ show (scale_id scale) ++ ">"

note_in_scale :: Scale -> Note -> Bool
note_in_scale scale = Maybe.isJust . scale_note_to_degree scale
