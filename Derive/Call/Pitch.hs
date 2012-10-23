-- | Create val calls for scale degrees.
module Derive.Call.Pitch where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Control as Control
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (optional, required)
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- | Create a pitch val call for the given scale degree.  This is intended to
-- be used by scales to generate their calls, but of course each scale may
-- define calls in its own way.
scale_degree :: Scale.NoteCall -> Derive.ValCall
scale_degree get_note_number = Derive.val_call
    "pitch" "Emit the pitch of a scale degree." $ CallSig.call2g
    ( optional "frac" 0
        "Add this many hundredths of a scale degree to the output."
    , optional "hz" 0 "Add an absolute hz value to the output."
    ) $ \frac hz _ -> do
        environ <- Internal.get_dynamic Derive.state_environ
        return $ TrackLang.VPitch $ PitchSignal.pitch (call frac hz environ)
    where
    call frac hz environ controls =
        Pitch.add_hz (hz + hz_sig) <$> get_note_number environ
            (if frac == 0 then controls
                else Map.insertWith' (+) Score.c_chromatic (frac / 100)
                    controls)
        where hz_sig = Map.findWithDefault 0 Score.c_hz controls

-- | Convert a note and @frac@ arg into a tracklang expression representing
-- that note.
pitch_expr :: Pitch.Note -> Double -> String
pitch_expr (Pitch.Note note) frac
    | frac == 0 = note
    | otherwise = note ++ " " ++ show (floor (frac * 100))


-- * pitch

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("=", Util.c_equal)
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
c_set = Derive.generator1 "set" "Emit a pitch with no interpolation." $
    CallSig.call1g (required "pitch" "Destination pitch.") $ \pitch args -> do
        pos <- Args.real_start args
        Util.pitch_signal [(pos, pitch)]

-- | Re-set the previous val.  This can be used to extend a breakpoint.
c_set_prev :: Derive.PitchCall
c_set_prev = Derive.generator "set-prev"
    ("Re-set the previous pitch.  This can be used to extend a breakpoint."
    ) $ CallSig.call0g $ \args -> case Args.prev_val args of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Args.real_start args
            if pos > prev_x then (:[]) <$> Util.pitch_signal [(pos, prev_y)]
                else return []

c_linear :: Derive.PitchCall
c_linear = Derive.generator1 "linear"
    ("Interpolate from the previous pitch to the given one in a straight"
    <> " line, ending at the current time."
    ) $ CallSig.call1g (required "pitch" "Destination pitch.") $ \pitch args ->
        interpolate_prev id args pitch

c_linear_next :: Derive.PitchCall
c_linear_next = Derive.generator1 "linear-next"
    ("Interpolate from the previous pitch to the given one in a straight"
    <> " line, starting here and ending at some time in the future."
    ) $ CallSig.call2g
    ( required "pitch" "Destination pitch."
    , optional "time" Nothing "Time to get to destination pitch."
    ) $ \pitch maybe_time args -> interpolate_next id args pitch maybe_time

c_exponential :: Derive.PitchCall
c_exponential = Derive.generator1 "exponential"
    ("Interpolate from the previous pitch to the given one in a curve,"
    <> " ending at the current time."
    ) $ CallSig.call2g
    ( required "pitch" "Destination pitch."
    , optional "exp" 2 Control.exp_doc
    ) $ \pitch exp args -> interpolate_prev (Control.expon exp) args pitch

c_exponential_next :: Derive.PitchCall
c_exponential_next = Derive.generator1 "exponential-next"
    ("Interpolate from the previous pitch to the given one in a curve,"
    <> " starting here and ending at some time in the future."
    ) $ CallSig.call3g
    ( required "pitch" "Destination pitch."
    , optional "exp" 2 Control.exp_doc
    , optional "time" Nothing "Time to get to destination pitch."
    ) $ \pitch exp maybe_time args ->
        interpolate_next (Control.expon exp) args pitch maybe_time

-- | Emit a slide from a neighboring pitch in absolute time.
--
-- [pitch /Pitch/] Destination pitch.
--
-- [neighbor /Transpose/ @1@] Neighbor interval.
--
-- [time /ScoreOrReal/ @.3@] Time taken to get to the destination pitch.
c_neighbor :: Derive.PitchCall
c_neighbor = Derive.generator1 "neighbor"
    ("Emit a slide from a neighboring pitch to the given one."
    ) $ CallSig.call3g
    ( required "pitch" "Destination pitch."
    , optional "neighbor" (Pitch.Chromatic 1) "Neighobr interval."
    , optional "time" (TrackLang.real 0.1) "Time to get to destination pitch."
    ) $ \pitch neighbor (TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        let pitch1 = Pitches.transpose neighbor pitch
        make_interpolator id True start pitch1 end pitch

c_up :: Derive.PitchCall
c_up = Derive.generator1 "up"
    "Ascend at the given speed until the next event." $ slope "Ascend" 1

c_down :: Derive.PitchCall
c_down = Derive.generator1 "down"
    "Descend at the given speed until the next event." $ slope "Descend" (-1)

slope :: String -> Double -> Derive.WithArgDoc
    (Derive.PassedArgs PitchSignal.Signal -> Derive.Deriver PitchSignal.Signal)
slope word sign =
    CallSig.call1g (optional "speed" (Pitch.Chromatic 1)
        (word <> " this many steps per second.")) $
    \speed args -> case Args.prev_val args of
        Nothing -> Util.pitch_signal []
        Just (_, prev_y) -> do
            start <- Args.real_start args
            next <- Derive.real (Args.next args)
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
            <*> Derive.real (Args.next args)
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
    Util.signal scale $ (if include_initial then id else drop 1)
        [(x, pitch_of x) | x <- Seq.range_end x1 x2 srate]
    where
    pitch_of = Pitches.interpolated note1 note2
        . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds
