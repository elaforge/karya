{- | Relative scale.
-}
module Derive.Scale.Relative where
import Util.Control
import qualified Util.ParseBs as Parse

import qualified Ui.Track as Track

import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.ParseBs as Derive.Parse
import qualified Derive.Scale as Scale
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


scale :: Scale.Scale
scale = Scale.Scale {
    Scale.scale_id = scale_id
    , Scale.scale_pattern = "float"
    , Scale.scale_map = Track.make_scale_map [(note_of d, d) | d <- [-40..40]]
    , Scale.scale_symbols = []
    , Scale.scale_octave = 0
    , Scale.scale_note_to_call = note_to_call
    , Scale.scale_input_to_note = input_to_note
    , Scale.scale_input_to_nn = input_to_nn
    , Scale.scale_degree_to_nn = degree_to_nn
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
note_to_call note = case Derive.Parse.parse_val (Pitch.note_text note) of
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
