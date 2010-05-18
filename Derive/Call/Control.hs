-- | A collection of basic calls for control and pitch tracks.
module Derive.Call.Control where

import qualified Util.Num as Num

import Ui

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (required)

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


-- Warping:
-- Would it be faster to evaluate in a linear warp, and then warp the output
-- signal in one go?  This wouldn't give calls a chance to treat warp
-- specially.
--
-- Continuous warp:
-- happens if the interpolating functions look up every point

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("", c_set)
    , ("i", c_linear)
    ]

c_set :: Derive.ControlCall
c_set = Derive.generate_one $ \args _ _ _ -> TrackLang.call1 args
    (required "val") $ \val -> do
        pos <- Derive.score_to_real 0
        return $ Signal.signal [(pos, val)]

c_linear :: Derive.ControlCall
c_linear = Derive.generate_one $ \args _ _ _ ->
    case TrackLang.passed_vals args of
        [] -> case TrackLang.passed_prev_val args of
            Nothing -> return $
                Derive.throw "can't set to previous val when there was none"
            Just (_, prev_y) -> return $ do
                pos <- Derive.score_to_real 0
                return $ Signal.signal [(pos, prev_y)]
        _ -> control_interpolate id args

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
control_interpolate :: (Double -> Signal.Y) -> TrackLang.PassedArgs Signal.Y
    -> Either TrackLang.TypeError Derive.ControlDeriver
control_interpolate f args = TrackLang.call1 args (required "val") $ \val -> do
    cur <- Derive.score_to_real 0
    srate <- Derive.require_val TrackLang.v_srate
    case TrackLang.passed_prev_val args of
        Nothing -> do
            -- TODO warn
            return $ Signal.signal [(cur, val)]
        Just (prev, prev_val) -> return $ Signal.signal $
            interpolate (RealTime srate) Num.scale f prev prev_val cur val


-- ** note

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("", c_note_set)
    , ("i", c_note_linear)
    ]

c_note_set :: Derive.PitchCall
c_note_set = Derive.generate_one $ \args _ _ _ -> TrackLang.call1 args
    (required "val") $ \note -> do
        scale <- Derive.require_val TrackLang.v_scale
        pos <- Derive.score_to_real 0
        degree <- note_to_degree scale note
        return $ PitchSignal.signal (Pitch.scale_id scale)
            [(pos, PitchSignal.degree_to_y degree)]

note_to_degree :: Pitch.Scale -> Pitch.Note -> Derive.Deriver Pitch.Degree
note_to_degree scale note = case Pitch.scale_note_to_degree scale note of
    Nothing -> Derive.throw $
        show note ++ " not in " ++ show (Pitch.scale_id scale)
    Just degree -> return degree

c_note_linear :: Derive.PitchCall
c_note_linear = Derive.generate_one $ \args _ _ _ ->
    case TrackLang.passed_vals args of
        [] -> case TrackLang.passed_prev_val args of
            Nothing -> return $
                Derive.throw "can't set to previous val when there was none"
            Just (_, prev_y) -> return $ do
                pos <- Derive.score_to_real 0
                scale <- Derive.require_val TrackLang.v_scale
                return $ PitchSignal.signal (Pitch.scale_id scale)
                    [(pos, prev_y)]
        _ -> pitch_interpolate id args

pitch_interpolate :: (Double -> Double) -> TrackLang.PassedArgs PitchSignal.Y
    -> Either TrackLang.TypeError Derive.PitchDeriver
pitch_interpolate f args = TrackLang.call1 args (required "note") $ \note -> do
        cur <- Derive.score_to_real 0
        scale <- Derive.require_val TrackLang.v_scale
        srate <- Derive.require_val TrackLang.v_srate
        degree <- note_to_degree scale note
        let signal = PitchSignal.signal (Pitch.scale_id scale)
        case TrackLang.passed_prev_val args of
            Nothing -> do
                -- TODO warn
                return $ signal [(cur, PitchSignal.degree_to_y degree)]
            Just (prev, prev_y) -> return $ signal $
                interpolate (RealTime srate) pitch_scale f prev prev_y cur
                    (PitchSignal.degree_to_y degree)

pitch_scale :: PitchSignal.Y -> PitchSignal.Y -> Double -> PitchSignal.Y
pitch_scale y0 y1 n =
    (Num.double_to_float d0, Num.double_to_float d1, Num.double_to_float n)
    where
    Pitch.Degree d0 = PitchSignal.y_to_degree y0
    Pitch.Degree d1 = PitchSignal.y_to_degree y1

-- * util

interpolate :: RealTime -> (y -> y -> Double -> y) -> (Double -> Double)
    -> RealTime -> y -> RealTime -> y -> [(RealTime, y)]
interpolate srate scale f x0 y0 x1 y1 =
    zip xs (map (scale y0 y1 . f . Num.normalize x0 x1) xs)
    where xs = range_until x0 x1 srate

-- | Enumerate a half-open range, except this one omits the first value and
-- includes the final one.  Uses multiplication instead of successive addition
-- to avoid loss of precision.
range_until :: (Num a, Ord a) => a -> a -> a -> [a]
range_until start end step = go 1
    where
    go i
        | val >= end = [end]
        | otherwise = val : go (i+1)
        where val = start + (i*step)
