-- | This \"scale\" generates pitches which are ratios of the pitches in
-- another pitch signal, @#ratio-source@.  The intent is to tune one instrument
-- relative another.
--
-- It's not a usual scale because there is no notion of scale degrees or
-- transposition or anything like that.  The degrees are ratios, e.g. @3/2@ or
-- @-9/8@.  A positive ratio will multiply with the source pitch, a negative
-- one will divide.  The source pitch is only sampled at the beginning of the
-- relative pitch, so if the source is moving the relative one won't move with
-- it.
module Derive.Scale.Ratio where
import qualified Text.ParserCombinators.Parsec as P
import qualified Util.Parse as Parse

import Ui
import qualified Ui.Track as Track

import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional)
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[+-]?\\d+/\\d+"
    -- no real sensible way to display this
    , Pitch.scale_map = Track.make_scale_map []
    , Pitch.scale_symbols = []
    , Pitch.scale_octave = 0
    , Pitch.scale_note_to_call = note_to_call
    -- | Since this isn't a proper scale, I can't think of any sensible way to
    -- input this with a music keyboard, so we'll have to use the computer
    -- keyboard.
    , Pitch.scale_input_to_note = const Nothing
    , Pitch.scale_input_to_nn = const Nothing
    , Pitch.scale_degree_to_nn = degree_to_nn
    , Pitch.scale_set_pitch_bend = True
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "ratio"

note_to_call :: Pitch.Note -> Maybe Derive.ValCall
note_to_call note = either (const Nothing) (Just . note_call)
    (Parse.parse_all p_note (Pitch.note_text note))

note_call :: (Double -> Double) -> Derive.ValCall
note_call ratio = Derive.ValCall "ratio" $ \args -> CallSig.call1 args
    (optional "hz" 0) $ \hz -> do
        nn <- get_nn_at source_name =<< Derive.now
        let out_hz = ratio (Pitch.nn_to_hz nn) + hz
            Pitch.NoteNumber out = Pitch.hz_to_nn out_hz
        return $ TrackLang.VDegree (Pitch.Degree out)
    where
    source_name = Score.Control "ratio-source"

get_nn_at :: Score.Control -> RealTime -> Derive.Deriver Pitch.NoteNumber
get_nn_at name pos = do
    sig <- Derive.require
        ("ratio scale requires a " ++ show name ++ " pitch signal")
        =<< Derive.get_named_pitch name
    scale <- Derive.get_scale "get_nn_at" (PitchSignal.sig_scale sig)
    let degree = PitchSignal.y_to_degree (PitchSignal.at pos sig)
    Derive.require (show degree ++ " not in " ++ show (Pitch.scale_id scale))
        (Pitch.scale_degree_to_nn scale degree)

-- | Ratios look like @2/5@, @-4/3@.
p_note :: P.Parser (Double -> Double)
p_note = do
    sign <- P.option '+' (P.oneOf "-+")
    num <- Parse.p_nat
    P.char '/'
    denom <- Parse.p_nat
    let ratio = fromIntegral num / fromIntegral denom
    return $ if sign == '-' then (/ ratio) else (* ratio)

degree_to_nn :: Pitch.Degree -> Maybe Pitch.NoteNumber
degree_to_nn (Pitch.Degree n) = Just (Pitch.NoteNumber n)
