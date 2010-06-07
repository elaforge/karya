-- | Basic calls for control and pitch tracks.
module Derive.Call.Control where

import qualified Util.Num as Num

import Ui
import qualified Ui.Types as Types

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import Derive.TrackLang (required, optional)

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

-- * control

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("=", Call.c_equal Derive.no_control)
    , ("", c_set)
    , ("i", c_linear)
    , ("e", c_exponential)
    , ("s", c_slide)
    ]

c_set :: Derive.ControlCall
c_set = Derive.generate_one "set" $ \args _ _ _ -> TrackLang.call1 args
    (required "val") $ \val -> do
        pos <- Derive.now
        return $ Signal.signal [(pos, val)]

c_linear :: Derive.ControlCall
c_linear = Derive.generate_one "linear" $ \args _ _ _ ->
    case TrackLang.passed_vals args of
        [] -> case TrackLang.passed_prev_val args of
            Nothing -> return $ Derive.throw
                "can't set to previous val when there was none"
            Just (_, prev_y) -> return $ do
                pos <- Derive.now
                return $ Signal.signal [(pos, prev_y)]
        _ -> TrackLang.call1 args (required "val") $ \val ->
            control_interpolate id val args

c_exponential :: Derive.ControlCall
c_exponential = Derive.generate_one "exponential" $ \args _ _ _ ->
    TrackLang.call2 args (required "val", optional "exp" 2) $ \val exp ->
        control_interpolate (expon exp) val args

c_slide :: Derive.ControlCall
c_slide = Derive.generate_one "slide" $ \args _ _ next -> TrackLang.call2 args
    (required "val", optional "time" 0.1) $ \val time -> do
        start <- Derive.now
        end <- case Call.next_event_begin next of
            Nothing -> return $ start + RealTime time
            Just n -> do
                next <- Derive.score_to_real n
                return $ min (start + RealTime time) next
        srate <- Call.get_srate
        case TrackLang.passed_prev_val args of
                Nothing -> do
                    Derive.warn "no previous value to slide from"
                    return $ Signal.signal [(start, val)]
                Just (_, prev_y) -> return $ Signal.signal $
                    interpolate_control True srate id start prev_y end val


-- ** control util

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
control_interpolate :: (Double -> Signal.Y) -> Signal.Y
    -> TrackLang.PassedArgs Signal.Y -> Derive.ControlDeriver
control_interpolate f val args = do
    start <- Derive.now
    srate <- Call.get_srate
    case TrackLang.passed_prev_val args of
        Nothing -> do
            Derive.warn "no previous value to interpolate from"
            return $ Signal.signal [(start, val)]
        Just (prev, prev_val) -> return $ Signal.signal $
            interpolate_control False srate f prev prev_val start val

interpolate_control :: Bool -> RealTime -> (Double -> Double)
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> [(RealTime, Signal.Y)]
interpolate_control include_initial srate f x0 y0 x1 y1
    | include_initial = sig
    | otherwise = drop 1 sig
    where
    sig = [(x, y_of x) | x <- range x0 x1 srate]
    y_of = Num.scale y0 y1 . f . Types.real_to_double . Num.normalize x0 x1


-- * pitch

-- TODO pitch_interpolate, c_note_slide, and c_neighbor all share a certain
-- amount of code.  think of a way to factor that out.

pitch_calls :: Derive.PitchCallMap
pitch_calls = Derive.make_calls
    [ ("=", Call.c_equal Derive.no_pitch)
    , ("", c_note_set)
    , ("i", c_note_linear)
    , ("e", c_note_exponential)
    , ("s", c_note_slide)

    , ("n", c_neighbor) -- this clearly needs a symbol
    , ("neighbor", c_neighbor)
    ]

c_note_set :: Derive.PitchCall
c_note_set = Derive.generate_one "note_set" $ \args _ _ _ ->TrackLang.call1 args
    (required "val") $ \note -> do
        scale <- Derive.require_val TrackLang.v_scale
        pos <- Derive.now
        degree <- Call.lookup_note scale note
        return $ PitchSignal.signal (Pitch.scale_id scale)
            [(pos, PitchSignal.degree_to_y degree)]

c_note_linear :: Derive.PitchCall
c_note_linear = Derive.generate_one "note_linear" $ \args _ _ _ ->
    case TrackLang.passed_vals args of
        [] -> case TrackLang.passed_prev_val args of
            Nothing -> return $
                Derive.throw "can't set to previous val when there was none"
            Just (_, prev_y) -> return $ do
                pos <- Derive.now
                scale <- Derive.require_val TrackLang.v_scale
                return $ PitchSignal.signal (Pitch.scale_id scale)
                    [(pos, prev_y)]
        _ -> TrackLang.call1 args (required "note") $ \note ->
            pitch_interpolate id note args

c_note_exponential :: Derive.PitchCall
c_note_exponential = Derive.generate_one "note_exponential" $ \args _ _ _ ->
    TrackLang.call2 args (required "note", optional "exp" 2) $ \note exp ->
        pitch_interpolate (expon exp) note args

c_note_slide :: Derive.PitchCall
c_note_slide = Derive.generate_one "note_slide" $
    \args _ _ next -> TrackLang.call2 args
    (required "note", optional "time" 0.1) $ \note time -> do
        start <- Derive.now
        end <- case Call.next_event_begin next of
            Nothing -> return $ start + RealTime time
            Just n -> do
                next <- Derive.score_to_real n
                return $ min (start + RealTime time) next
        scale <- Derive.require_val TrackLang.v_scale
        srate <- Call.get_srate
        degree <- Call.lookup_note scale note
        let signal = PitchSignal.signal (Pitch.scale_id scale)
        case TrackLang.passed_prev_val args of
                Nothing -> do
                    Derive.warn "no previous value to slide from"
                    return $ signal [(start, PitchSignal.degree_to_y degree)]
                Just (_, prev_y) -> return $ signal $
                    interpolate_pitch True srate id
                        start (PitchSignal.y_to_degree prev_y) end degree

-- | Emit a quick slide from a neighboring pitch in absolute time.
--
-- [neighbor /Number/ @1@] Neighbor note, in scale degrees.
--
-- [time /Number/ @.3@] Duration of ornament, in seconds.
c_neighbor :: Derive.PitchCall
c_neighbor = Derive.generate_one "neighbor" $ \args _ _ _ ->
    TrackLang.call3 (Call.default_relative_note args)
    (required "note", optional "neighbor" 1, optional "time" 0.1) $
    \note neighbor time -> do
        start <- Derive.now
        let end = start + RealTime time
        scale <- Derive.require_val TrackLang.v_scale
        degree <- Call.lookup_note scale note
        srate <- Call.get_srate
        let signal = PitchSignal.signal (Pitch.scale_id scale)
        return $ signal $ interpolate_pitch True srate id
            start (Pitch.Degree neighbor + degree) end degree


-- ** pitch util

pitch_interpolate :: (Double -> Signal.Y) -> Pitch.Note
    -> TrackLang.PassedArgs PitchSignal.Y -> Derive.PitchDeriver
pitch_interpolate f note args = do
    start <- Derive.now
    scale <- Derive.require_val TrackLang.v_scale
    srate <- Call.get_srate
    degree <- Call.lookup_note scale note
    let signal = PitchSignal.signal (Pitch.scale_id scale)
    case TrackLang.passed_prev_val args of
        Nothing -> do
            Derive.warn $ "no previous val to interpolate from"
            return $ signal [(start, PitchSignal.degree_to_y degree)]
        Just (prev, prev_y) -> return $ signal $
            interpolate_pitch False srate f
                prev (PitchSignal.y_to_degree prev_y) start degree

interpolate_pitch :: Bool -> RealTime -> (Double -> Double)
    -> RealTime -> Pitch.Degree -> RealTime -> Pitch.Degree
    -> [(RealTime, PitchSignal.Y)]
interpolate_pitch include_initial srate f x0 y0 x1 y1
    | include_initial = sig
    | otherwise = drop 1 sig
    where
    sig = [(x, (fy0, fy1, y_of x)) | x <- range x0 x1 srate]
    y_of = Num.d2f . f . Types.real_to_double . Num.normalize x0 x1
    (fy0, fy1) = (to_f y0, to_f y1)
    to_f (Pitch.Degree d) = Num.d2f d

-- * util

-- | Negative exponents produce a curve that jumps from the "starting point"
-- which doesn't seem too useful, so so hijack the negatives as an easier way
-- to write 1/n.  That way n is smoothly departing, while -n is smoothly
-- approaching.
expon :: Double -> Double -> Double
expon n x = x**exp
    where exp = if n >= 0 then n else (1 / abs n)

-- | Enumerate an inclusive range.  Uses multiplication instead of successive
-- addition to avoid loss of precision.
range :: (Num a, Ord a) => a -> a -> a -> [a]
range start end step = go 0
    where
    go i
        | val >= end = [end]
        | otherwise = val : go (i+1)
        where val = start + (i*step)
