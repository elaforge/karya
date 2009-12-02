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
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Util.Parse as Parse


-- | The main representation for a pitch.  Scale sharing is enforced by keeping
-- the scale as an ID, which must be looked up in a map.
data Pitch = Pitch {
    pitch_scale :: ScaleId
    , pitch_note :: Note
    } deriving (Eq, Ord, Show)

-- | Verify that the string is a valid note in the scale, and return the Pitch.
pitch :: Scale -> String -> Maybe Pitch
pitch scale note_str =
    fmap (const (Pitch (scale_id scale) note)) (scale_note_to_nn scale note)
    where note = Note note_str

-- * nn

-- | This is in equal tempered scale notes with the same definition as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz.  Middle C is NoteNumber 60.
--
-- It would be less tempered-centric to use hz, but for the moment this seems
-- practical since note numbers are easier to read.
newtype NoteNumber = NoteNumber Double deriving (Eq, Ord, Show, Fractional, Num)

nn :: (Real a) => a -> NoteNumber
nn = NoteNumber . realToFrac

-- * hz

type Hz = Double

add_hz :: Hz -> NoteNumber -> NoteNumber
add_hz 0 nn = nn -- hz_to_nn . nn_to_hz adds a tiny bit of inaccuracy TODO fix?
add_hz hz nn = hz_to_nn (hz + nn_to_hz nn)

nn_to_hz :: NoteNumber -> Hz
nn_to_hz (NoteNumber nn) = exp (nn * _equal1 + _equal2)

hz_to_nn :: Hz -> NoteNumber
hz_to_nn hz = NoteNumber $ (log hz - _equal2) / _equal1

-- | Constants to calculate equal tempered conversions.
_equal1 = log 2 / 12
_equal2 = log a_hz - (a_nn * _equal1)
    where
    -- NoteNumber is defined with these values.  Ultimately it's because midi
    -- synthesizers are by default defined with these values.
    a_hz = 440
    a_nn = 69

-- * InputKey

-- | A physically played key that hasn't been mapped to a scale yet.
newtype InputKey = InputKey Double deriving (Eq, Ord, Show)

-- | Useful to orient scales around a common center.
middle_c :: InputKey
middle_c = InputKey 60

-- | The middle octave.  The \"center\" of a scale should be oriented around
-- this.
middle_octave :: Octave
middle_octave = 5

-- * generic pitches

-- | A scale independent pitch.  The definitions of the octave and the degree
-- offset are up to the scale.  This doesn't know how many notes are in an
-- octave, so after transposition it may be denormalized.
data Generic = Generic Octave Double deriving (Eq, Ord, Show)

type Octave = Int

-- | Generic relative notes look like \"+4/3.2\" or \"+3.2\" (octave omitted)
-- or \"+3/\" (only octave).
from_relative :: Generic -> Note
from_relative (Generic oct nn)
    | oct == 0 = Note (sign ++ Parse.show_float (Just 2) (abs nn))
    | otherwise = Note (sign ++ show (abs oct) ++ degree)
    where
    sign = if (if oct /= 0 then fromIntegral oct else nn) >= 0 then "+" else "-"
    degree = (if oct == 0 then "" else "/")
        ++ (if nn == 0 then "" else Parse.show_float (Just 2) nn)

to_relative :: Note -> Maybe Generic
to_relative (Note note) = do
    unless (let c = take 1 note in c == "+" || c == "-") mzero
    oct <- if null oct_s then return 0 else Parse.int oct_s
    nn <- if null nn_s then return 0 else Parse.float nn_s
    return (Generic oct nn)
    where
    (pre, post) = break (=='/') note
    (oct_s, nn_s) = if null post then ("", pre) else (pre, drop 1 post)

-- Should this produce a separate Relative type?  Let's see if I need it first.
add_generic, sub_generic :: Generic -> Generic -> Generic
add_generic (Generic oct1 nn1) (Generic oct2 nn2) =
    Generic (oct1+oct2) (nn1+nn2)
sub_generic (Generic oct1 nn1) (Generic oct2 nn2) =
    Generic (oct1-oct2) (nn1-nn2)


-- * scale

-- | A Note belongs to a scale and describes a certain note in that scale.
newtype Note = Note String deriving (Eq, Ord, Show)
note_text (Note s) = s

-- | Tie together Pitches and their Scales.
type ScaleMap = Map.Map ScaleId Scale

newtype ScaleId = ScaleId String deriving (Eq, Ord, Read, Show)

data Scale = Scale {
    scale_id :: ScaleId
    -- | A pattern describing what the scale notes look like.  Used only for
    -- error msgs (i.e. parse errors) so it should be human readable and
    -- doesn't have to follow any particular syntax.  A regex is recommended
    -- though.
    , scale_pattern :: String

    -- | Convert the scale note to a pitch with frequency.  Returns Nothing
    -- if the note isn't part of the scale.
    , scale_note_to_nn :: Note -> Maybe NoteNumber
    -- | Convert scale note to generic pitch, or Nothing.
    , scale_note_to_generic :: Note -> Maybe Generic
    -- | Convert from an InputKey to a Note, or Nothing if the it's out of
    -- range for this scale.
    , scale_input_to_note :: InputKey -> Maybe Note

    -- | This could be implemented as @input_to_note >>= note_to_nn@ but
    -- it will be more efficient to bypass the Note.  Used by midi thru.
    , scale_input_to_nn :: InputKey -> Maybe NoteNumber

    -- | Transpose the given note by the given scale steps, or octaves.
    -- Return an error if it couldn't be transposed for some reason.
    --
    -- TODO once I add generic_to_note then these can go away.
    , scale_transpose :: Transposer
    , scale_transpose_octave :: Transposer

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

type Transposer = Double -> Note -> Either Error Note

note_in_scale :: Scale -> Note -> Bool
note_in_scale scale = Maybe.isJust . scale_note_to_nn scale

data Error = NotInScale | OutOfRange deriving (Eq, Read, Show)
instance Error.Error Error where
    strMsg = const NotInScale -- this is stupid but necessary
