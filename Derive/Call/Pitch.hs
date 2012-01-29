-- | Create val calls for scale degrees.
module Derive.Call.Pitch where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional, required)
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- | Create a note val call for the given scale degree.  This is intended to
-- be used by scales to generate their val calls, but of course each scale may
-- define degrees in its own way.
--
-- [frac /Num/ @0@] Add this many hundredths of a scale degree to the output.
-- Intended for fractional scale degrees.
--
-- [hz /Num/ @0@] Add an absolute hz value to the output.
note_call :: Pitch.Note -> Scale.GetNoteNumber -> Derive.ValCall
note_call note note_number =
    Derive.ValCall ("degree: " ++ Pitch.note_text note) $ \args ->
    CallSig.call2 args (optional "frac" 0, optional "hz" 0) $ \frac hz -> do
        key <- Util.lookup_key
        return $ TrackLang.VPitch $ PitchSignal.pitch (call frac hz key)
    where
    call frac hz key = \controls -> do
        let get c = maybe 0 Score.typed_val (Map.lookup c controls)
            chrom = get Score.c_chromatic + frac
            dia = get Score.c_diatonic
            hz_sig = get Score.c_hz
        either (Left . errmsg chrom dia key)
            (Right . Pitch.add_hz (hz + hz_sig)) (note_number chrom dia key)
    errmsg chrom dia key err = PitchSignal.PitchError $ case err of
        Scale.InvalidTransposition ->
            "note can't be transposed: " ++ show (chrom, dia)
        Scale.KeyNeeded ->
            "no key is set, but this transposition needs one"
        Scale.UnparseableKey ->
            "key unparseable by given scale: " ++ show key

-- | Convert a note and @frac@ arg into a tracklang expression representing
-- that note.
note_expr :: Pitch.Note -> Double -> String
note_expr (Pitch.Note note) frac
    | frac == 0 = note
    | otherwise = note ++ " " ++ Pretty.show_float (Just 2) (frac * 100)


-- * pitch

-- TODO pitch_interpolate, c_note_slide, and c_neighbor all share a certain
-- amount of code.  think of a way to factor that out.

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("=", Util.c_equal)
    , ("", c_note_set)
    , ("set", c_note_set)
    , ("i", c_note_linear)
    , ("e", c_note_exponential)
    , ("s", c_note_slide)

    , ("n", c_neighbor) -- this clearly needs a symbol
    , ("neighbor", c_neighbor)
    ]

c_note_set :: Derive.PitchCall
c_note_set = Derive.generator1 "note_set" $ \args -> CallSig.call1 args
    (required "pitch") $ \pitch -> do
        pos <- Args.real_start args
        Util.pitch_signal [(pos, pitch)]

c_note_linear :: Derive.PitchCall
c_note_linear = Derive.generator1 "note_linear" $ \args ->
    case Derive.passed_vals args of
        [] -> case Args.prev_val args of
            Nothing ->
                Derive.throw "can't set to previous val when there was none"
            Just (_, prev) -> do
                pos <- Args.real_start args
                Util.pitch_signal [(pos, prev)]
        _ -> CallSig.call1 args (required "pitch") $ \pitch ->
            pitch_interpolate id pitch args

c_note_exponential :: Derive.PitchCall
c_note_exponential = Derive.generator1 "note_exponential" $ \args ->
    CallSig.call2 args (required "pitch", optional "exp" 2) $ \pitch exp ->
        pitch_interpolate (Control.expon exp) pitch args

c_note_slide :: Derive.PitchCall
c_note_slide = Derive.generator1 "note_slide" $ \args ->CallSig.call2 args
    (required "pitch", optional "time" 0.1) $ \pitch time -> do
        start <- Args.real_start args
        end <- case Args.next_start args of
            Nothing -> return $ start + RealTime.seconds time
            Just n -> do
                next <- Derive.real n
                return $ min (start + RealTime.seconds time) next
        srate <- Util.get_srate
        case Args.prev_val args of
            Nothing -> Util.pitch_signal [(start, pitch)]
            Just (_, prev) -> interpolator srate id True start prev end pitch

-- | Emit a slide from a neighboring pitch in absolute time.
--
-- [pitch /Pitch/] Destination pitch.
--
-- [neighbor /Transpose/ @1@] Neighbor interval.
--
-- [time /ScoreOrReal/ @.3@] Time taken to get to the destination pitch.
c_neighbor :: Derive.PitchCall
c_neighbor = Derive.generator1 "neighbor" $ \args ->
    CallSig.call3 args (required "pitch",
        optional "neighbor" (Pitch.Chromatic 1),
        optional "time" (TrackLang.real 0.1)) $
    \pitch neighbor (TrackLang.DefaultReal time) -> do
        (start, end) <- Util.duration_from_start args time
        let pitch1 = Pitches.transpose neighbor pitch
        srate <- Util.get_srate
        interpolator srate id True start pitch1 end pitch

-- ** pitch util

pitch_interpolate :: (Double -> Double) -> PitchSignal.Pitch
    -> Derive.PassedArgs PitchSignal.Signal
    -> Derive.Deriver PitchSignal.Signal
pitch_interpolate f pitch args = do
    start <- Args.real_start args
    srate <- Util.get_srate
    case Args.prev_val args of
        Nothing -> Util.pitch_signal [(start, pitch)]
        Just (prev_t, prev) ->
            interpolator srate f False prev_t prev start pitch

interpolator :: RealTime -> (Double -> Double)
    -> Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> Derive.Deriver PitchSignal.Signal
interpolator srate f include_initial x1 note1 x2 note2 =
    Util.pitch_signal $ (if include_initial then id else drop 1) $
        [(x, pitch_of x) | x <- Seq.range_end x1 x2 srate]
    where
    pitch_of = Pitches.interpolated note1 note2
        . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds
