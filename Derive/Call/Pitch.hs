-- | Create val calls for scale degrees.
module Derive.Call.Pitch where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
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
            dia = get Score.c_diatonic
            chrom = get Score.c_chromatic + frac / 100
            hz_sig = get Score.c_hz
        either (Left . errmsg dia chrom (hz + hz_sig) key)
            (Right . Pitch.add_hz (hz + hz_sig)) (note_number chrom dia key)
    errmsg dia chrom hz key err = PitchSignal.PitchError $ case err of
        Scale.InvalidTransposition -> "note can't be transposed: "
            ++ unwords (filter (not . null)
                [show_val "d" dia, show_val "c" chrom, show_val "hz" hz])
        Scale.KeyNeeded ->
            "no key is set, but this transposition needs one"
        Scale.UnparseableKey ->
            "key unparseable by given scale: " ++ show key
        Scale.UnparseableNote ->
            "unparseable note (shouldn't happen)"
    show_val _ 0 = ""
    show_val code val = Pretty.pretty val ++ code

-- | Convert a note and @frac@ arg into a tracklang expression representing
-- that note.
note_expr :: Pitch.Note -> Double -> String
note_expr (Pitch.Note note) frac
    | frac == 0 = note
    | otherwise = note ++ " " ++ show (floor (frac * 100))


-- * pitch

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("=", Derive.transformer "equal" Util.equal_transformer)
    , ("", c_set)
    , ("set", c_set)
    , ("'", c_set_prev)
    , ("i", c_linear)
    , ("i>", c_linear_next)
    , ("e", c_exponential)
    , ("e>", c_exponential_next)
    , ("n", c_neighbor)
    , ("u", c_up)
    , ("d", c_down)
    ]

-- | This should contain the calls that require the previous value.  It's used
-- by a hack in 'Derive.Slice.slice'.
require_previous :: Set.Set String
require_previous = Set.fromList ["'", "u", "d"]

c_set :: Derive.PitchCall
c_set = Derive.generator1 "set" $ \args -> CallSig.call1 args
    (required "pitch") $ \pitch -> do
        pos <- Args.real_start args
        Util.pitch_signal [(pos, pitch)]

-- | Re-set the previous val.  This can be used to extend a breakpoint.
c_set_prev :: Derive.PitchCall
c_set_prev = Derive.generator "set-prev" $ \args -> CallSig.call0 args $
    case Args.prev_val args of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Args.real_start args
            if pos > prev_x then (:[]) <$> Util.pitch_signal [(pos, prev_y)]
                else return []

c_linear :: Derive.PitchCall
c_linear = Derive.generator1 "linear" $ \args ->
    CallSig.call1 args (required "pitch") (interpolate_prev id args)

-- | Linear interpolation from the previous value.  This is different than
-- 'c_linear' because it starts interpolating *after* the call and
-- continues for a given amount of time, or until the next event.
--
-- [val /Pitch/] Destination value.
--
-- [time /Maybe ScoreOrReal/ Nothing] Time taken to get there.  If not given,
-- slide until the next event.
c_linear_next :: Derive.PitchCall
c_linear_next = Derive.generator1 "linear-next" $ \args -> CallSig.call2 args
    (required "pitch", optional "time" Nothing) (interpolate_next id args)

c_exponential :: Derive.PitchCall
c_exponential = Derive.generator1 "exponential" $ \args ->
    CallSig.call2 args (required "pitch", optional "exp" 2) $ \pitch exp ->
        interpolate_prev (Control.expon exp) args pitch

c_exponential_next :: Derive.PitchCall
c_exponential_next = Derive.generator1 "exponential-next" $ \args ->
    CallSig.call3 args (required "pitch", optional "exp" 2,
            optional "time" Nothing) $ \pitch exp maybe_time ->
        interpolate_next (Control.expon exp) args pitch maybe_time

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
        make_interpolator id True start pitch1 end pitch

c_up :: Derive.PitchCall
c_up = Derive.generator1 "up" (slope 1)

c_down :: Derive.PitchCall
c_down = Derive.generator1 "down" (slope (-1))

slope :: Double -> Derive.PassedArgs PitchSignal.Signal
    -> Derive.Deriver PitchSignal.Signal
slope sign args = CallSig.call1 args (optional "speed" (Pitch.Chromatic 1)) $
    \speed -> case Args.prev_val args of
        Nothing -> Util.pitch_signal []
        Just (_, prev_y) -> do
            start <- Args.real_start args
            next <- Derive.real (Args.end args)
            let diff = RealTime.to_seconds (next - start) * speed_val * sign
                (speed_val, typ) = Util.split_transpose speed
                dest = Pitches.transpose (Util.join_transpose diff typ) prev_y
            make_interpolator id True start prev_y next dest

-- * util

type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -- ^ start -> starty -> end -> endy
    -> PitchSignal.Signal

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
interpolate_prev :: (Double -> Double) -> Derive.PassedArgs PitchSignal.Signal
    -> PitchSignal.Pitch -> Derive.Deriver PitchSignal.Signal
interpolate_prev f args pitch = do
    start <- Args.real_start args
    case Args.prev_val args of
        Nothing -> Util.pitch_signal [(start, pitch)]
        Just (prev_t, prev) ->
            make_interpolator f False prev_t prev start pitch

-- | Similar to 'interpolate_prev', except interpolate to the given val between
-- here and a time in the future, rather than between the previous event and
-- here.
interpolate_next :: (Double -> Double)
    -> Derive.PassedArgs PitchSignal.Signal -> PitchSignal.Pitch
    -> Maybe TrackLang.DefaultReal
    -- ^ if given, end at this time, if not end at the next event
    -> Derive.Deriver PitchSignal.Signal
interpolate_next f args pitch maybe_time = do
    (start, end) <- case maybe_time of
        Nothing -> (,) <$> Args.real_start args
            <*> Derive.real (Args.end args)
        Just (TrackLang.DefaultReal time) ->
            Util.duration_from_start args time
    case Args.prev_val args of
        Nothing -> Util.pitch_signal [(start, pitch)]
        Just (_, prev_y) ->
            make_interpolator f True start prev_y end pitch

make_interpolator :: (Double -> Double)
    -> Bool -- ^ include the initial sample or not
    -> RealTime -> PitchSignal.Pitch -> RealTime -> PitchSignal.Pitch
    -> Derive.Deriver PitchSignal.Signal
make_interpolator f include_initial x1 note1 x2 note2 = do
    scale <- Util.get_scale
    srate <- Util.get_srate
    return $ interpolator scale srate f include_initial x1 note1 x2 note2

interpolator :: Scale.Scale -> RealTime -> (Double -> Double)
    -> Interpolator
interpolator scale srate f include_initial x1 note1 x2 note2 =
    Util.signal scale $ (if include_initial then id else drop 1) $
        [(x, pitch_of x) | x <- Seq.range_end x1 x2 srate]
    where
    pitch_of = Pitches.interpolated note1 note2
        . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds
