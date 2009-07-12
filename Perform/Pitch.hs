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


-- | The main representation for a pitch.  Scale sharing is enforced by keeping
-- the scale as an ID, which must be looked up in a map.
data Pitch = Pitch {
    pitch_scale :: ScaleId
    , pitch_note :: Note
    } deriving (Eq, Ord, Show)

-- | Verify that the string is a valid note in the scale, and return the Pitch.
pitch :: Scale -> String -> Maybe Pitch
pitch scale note_str =
    fmap (const (Pitch (scale_id scale) note)) (scale_to_nn scale note)
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

-- | A physically played key, either on the midi keyboard or letter keyboard.
-- This isn't the same as the midi note number because the midi note number
-- represents a certain tempered pitch, while this is still user input.  It
-- has yet to be mapped to a pitch (and may not be, for unpitched sounds).
--
-- (octave, note)
type KeyNumber = (Int, Int)


-- * scale

-- | A Note belongs to a scale and describes a certain note in that scale.
newtype Note = Note String deriving (Eq, Ord, Show)
note_text (Note s) = s

-- | Tie together Pitches and their Scales.
type ScaleMap = Map.Map ScaleId Scale

type ScaleId = String

data Scale = Scale {
    scale_id :: ScaleId
    -- | A pattern describing what the scale notes look like.  Used only for
    -- error msgs (i.e. parse errors) so it should be human readable and
    -- doesn't have to follow any particular syntax.  A regex is recommended
    -- though.
    , scale_pattern :: String
    -- | Transpose the given note by the given scale steps, or octaves.
    -- Return an error if it couldn't be transposed for some reason.
    , scale_transpose :: Int -> Note -> Either String Note
    , scale_transpose_octave :: Int -> Note -> Either String Note

    -- | Convert the scale note to a pitch with frequency.  Returns Nothing
    -- if the note isn't part of the scale.
    , scale_to_nn :: Note -> Maybe NoteNumber
    -- | Convert back to a scale note from the pitch.  to_pitch -> from_pitch
    -- may lose information if the scale has enhormonics.  It will also round
    -- the pitch off if it doesn't exactly correspond to a scale note.
    , scale_from_nn :: NoteNumber -> Either String Note
    , scale_key_to_note :: KeyNumber -> Either String Note

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
