{- | Relative scale.
-}
module Derive.Scale.Relative where
import Util.Control
import qualified Util.Parse as Parse

import qualified Perform.Pitch as Pitch


-- | Create a relative scale adjusted to be relative for the enclosing scale.
adjust :: Pitch.Scale -> Pitch.Scale
adjust enclosing_scale = scale
    { Pitch.scale_octave = oct
    , Pitch.scale_id = Pitch.ScaleId ("relative " ++ show oct)
    }
    where oct = Pitch.scale_octave enclosing_scale

scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "float"
    , Pitch.scale_octave = 0
    , Pitch.scale_note_to_degree = note_to_degree
    , Pitch.scale_input_to_note = input_to_note
    , Pitch.scale_input_to_nn = input_to_nn
    , Pitch.scale_degree_to_nn = degree_to_nn
    , Pitch.scale_set_pitch_bend = False
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "relative"

-- | TODO parse octave signs
note_to_degree :: Pitch.Note -> Maybe Pitch.Degree
note_to_degree note = Pitch.Degree <$> Parse.float (Pitch.note_text note)

input_to_note :: Pitch.InputKey -> Maybe Pitch.Note
input_to_note (Pitch.InputKey key) =
    Just $ Pitch.Note $ Parse.show_float (Just 2) (key - middle)

-- Relative pitches map through with no change, just so you can hear something
-- when you play them.

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey key) = Just (Pitch.NoteNumber key)

degree_to_nn :: Pitch.Degree -> Maybe Pitch.NoteNumber
degree_to_nn (Pitch.Degree degree) = Just (Pitch.NoteNumber degree)

-- | Take input to a pitch relative to middle C.  This is kinda random, so I'm
-- not sure if it'll be useful.
middle :: Double
middle = case Pitch.middle_c of { Pitch.InputKey k -> k }
