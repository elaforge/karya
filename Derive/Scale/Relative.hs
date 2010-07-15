{- | Relative scale.
-}
module Derive.Scale.Relative where
import Util.Control
import qualified Util.Parse as Parse

import qualified Ui.Track as Track

import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- | Create a relative scale adjusted to be relative for the enclosing scale.
adjust :: Pitch.Scale -> Pitch.Scale
adjust enclosing_scale = scale
    { Pitch.scale_octave = oct
    -- would make Pitch.is_relative kinda icky
    -- , Pitch.scale_id = Pitch.ScaleId ("relative " ++ show oct)
    }
    where oct = Pitch.scale_octave enclosing_scale

scale :: Pitch.Scale
scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "float"
    , Pitch.scale_map = Track.make_scale_map
        [(note_of d, d) | d <- [-40..40]]
    , Pitch.scale_octave = 0
    , Pitch.scale_note_to_call = note_to_call
    , Pitch.scale_input_to_note = input_to_note
    , Pitch.scale_input_to_nn = input_to_nn
    , Pitch.scale_degree_to_nn = degree_to_nn
    , Pitch.scale_set_pitch_bend = False
    }
    where
    note_of :: Double -> String
    note_of = Pitch.note_text . degree_to_note . Pitch.Degree

scale_id :: Pitch.ScaleId
scale_id = Pitch.relative

-- | TODO parse octave signs
note_to_degree :: Pitch.Note -> Maybe Pitch.Degree
note_to_degree note = Pitch.Degree <$> Parse.float (Pitch.note_text note)

note_to_call :: Pitch.Note -> Maybe Derive.ValCall
note_to_call note = case TrackLang.parse_val (Pitch.note_text note) of
    Right (TrackLang.VNum d) -> Just (Call.Pitch.relative_call (Pitch.Degree d))
    _ -> Nothing

degree_to_note :: Pitch.Degree -> Pitch.Note
degree_to_note (Pitch.Degree d) = Pitch.Note (Parse.show_float (Just 2) d)

input_to_note :: Pitch.InputKey -> Maybe Pitch.Note
input_to_note (Pitch.InputKey key) =
    Just $ degree_to_note (Pitch.Degree (key - middle))

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
