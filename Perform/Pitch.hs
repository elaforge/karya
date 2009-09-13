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
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe


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

-- | This is in tempered scale notes with the same note range as MIDI, so
-- MIDI note 0 is NoteNumber 0, at 8.176 Hz.  Middle C is NoteNumber 60.
--
-- It would be less tempered-centric to use hz, but for the moment this seems
-- practical since note numbers are easier to read.
newtype NoteNumber = NoteNumber Double deriving (Eq, Ord, Show)
un_nn (NoteNumber nn) = nn

nn :: (Real a) => a -> NoteNumber
nn = NoteNumber . realToFrac

-- A scale independent pitch.  The definitions of the octave and the degree
-- offset are up to the scale.
newtype GenericPitch = GenericPitch (Octave, Int) deriving (Eq, Ord, Show)

-- | A physically played key, either on the midi keyboard or letter keyboard.
newtype InputKey = InputKey (Octave, Int) deriving (Eq, Ord, Show)


type Octave = Int


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
    -- | Convert from an InputKey to a Note, or Nothing if the it's out of
    -- range for this scale.
    , scale_input_to_note :: InputKey -> Maybe Note

    -- | Transpose the given note by the given scale steps, or octaves.
    -- Return an error if it couldn't be transposed for some reason.
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

type Transposer = Int -> Note -> Either Error Note

note_in_scale :: Scale -> Note -> Bool
note_in_scale scale = Maybe.isJust . scale_note_to_nn scale

data Error = NotInScale | OutOfRange deriving (Eq, Read, Show)
instance Error.Error Error where
    strMsg = const NotInScale -- this is stupid but necessary


-- * scale utils

-- | Construct data structures to implement a scale.
--
-- @input_to_generic@ is separate because multiple input keys may map to the
-- same note.  If it maps to generics that don't exist in @note_map@, those
-- inputs will be ignored.
make_scale_map :: [(GenericPitch, Note, NoteNumber)]
    -> [(InputKey, GenericPitch)]
    -> (Int -> GenericPitch -> GenericPitch)
    -> (Map.Map Note NoteNumber, Map.Map InputKey Note, Transposer)
make_scale_map note_map input_to_generic transpose_generic =
    (note_to_nn, input_to_note, transpose)
    where
    (generics, notes, note_numbers) = unzip3 note_map
    generic_to_note = Map.fromList $ zip generics notes
    note_to_generic = Map.fromList $ zip notes generics
    note_to_nn = Map.fromList (zip notes note_numbers)

    input_to_note = Map.fromList (concatMap get input_to_generic)
        where
        get (input, generic) = case Map.lookup generic generic_to_note of
            Nothing -> []
            Just note -> [(input, note)]

    transpose n note = do
        generic <- maybe (Left NotInScale) Right
            (Map.lookup note note_to_generic)
        maybe (Left OutOfRange) Right
            (Map.lookup (transpose_generic n generic) generic_to_note)
