-- | Basic calls for control tracks.
module Derive.Call.Control where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (required, optional)
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- Warping:
-- Would it be faster to evaluate in a linear warp, and then warp the output
-- signal in one go?  This wouldn't give calls a chance to treat warp
-- specially.
--
-- Continuous warp:
-- happens if the interpolating functions look up every point

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("=", Util.c_equal)
    -- Fallback call will take val-call output.
    , ("", c_set)
    , ("set", c_set)
    , ("set-prev", c_set_prev)
    , ("i", c_linear)
    , ("e", c_exponential)
    , ("s", c_slide)
    , ("n", c_neighbor)

    -- not sure which one I'll like better
    , ("`ped`", c_pedal)
    , ("h", c_pedal)
    ]

c_set :: Derive.ControlCall
c_set = Derive.generator1 "set" $ \args -> CallSig.call1 args
    (required "val") $ \val -> do
        pos <- Derive.passed_real args
        return $ Signal.signal [(pos, val)]

c_set_prev :: Derive.ControlCall
c_set_prev = Derive.generator "set-prev" $ \args -> CallSig.call0 args $
    case Derive.passed_prev_val args of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Derive.passed_real args
            return $ if pos > prev_x
                then [Signal.signal [(pos, prev_y)]]
                else []

c_linear :: Derive.ControlCall
c_linear = Derive.generator1 "linear" $ \args ->
    case Derive.passed_vals args of
        [] -> case Derive.passed_prev_val args of
            Nothing -> Derive.throw
                "can't set to previous val when there was none"
            Just (_, prev_y) -> do
                pos <- Derive.passed_real args
                return $ Signal.signal [(pos, prev_y)]
        _ -> CallSig.call1 args (required "val") $ \val ->
            control_interpolate id val args

c_exponential :: Derive.ControlCall
c_exponential = Derive.generator1 "exponential" $ \args ->
    CallSig.call2 args (required "val", optional "exp" 2) $ \val exp ->
        control_interpolate (expon exp) val args

-- | Linear interpolation from the previous value.  This is different than
-- 'c_linear' because it starts interpolating *after* the call and continues
-- for a given amount of time.
--
-- [val /Number/] Destination value.
--
-- [time /ScoreOrReal/ 0.1] Time taken to get there.
c_slide :: Derive.ControlCall
c_slide = Derive.generator1 "slide" $ \args ->
    CallSig.call2 args (required "val", optional "time" (TrackLang.real 0.1)) $
    \val (TrackLang.DefaultReal time) -> do
        (start, end) <- Util.duration_from_start args time
        end <- case Derive.passed_next_begin args of
            Nothing -> return end
            Just n -> do
                next <- Derive.real n
                return $ min end next
        srate <- Util.get_srate
        return $ case Derive.passed_prev_val args of
            Nothing -> Signal.signal [(start, val)]
            Just (_, prev_y) -> interpolator srate id True start prev_y end val

-- | Emit a slide from a value to 0 in absolute time.  This is the control
-- version of the neighbor pitch call.
--
-- [neighbor /Number/ @1@] Neighbor value.
--
-- [time /ScoreOrReal/ @.3@] Time taken to get to 0.
c_neighbor :: Derive.ControlCall
c_neighbor = Derive.generator1 "neighbor" $ \args ->
    CallSig.call2 args (optional "neighbor" 1,
        optional "time" (TrackLang.real 0.1)) $
    \neighbor (TrackLang.DefaultReal time) -> do
        (start, end) <- Util.duration_from_start args time
        srate <- Util.get_srate
        return $ interpolator srate id True start neighbor end 0

-- | Unlike most control events, this uses a duration.  Set the control to the
-- given value for the event's duration, and reset to the old value
-- afterwards.
--
-- Mostly makes sense with pedal type controls.
c_pedal :: Derive.ControlCall
c_pedal = Derive.generator1 "pedal" $ \args -> CallSig.call1 args
    (optional "val" 1) $ \val -> do
        (start, end) <- Derive.passed_real_range args
        let prev = maybe 0 snd (Derive.passed_prev_val args)
        return $ Signal.signal [(start, val), (end, prev)]

-- * control util

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
control_interpolate :: (Double -> Signal.Y) -> Signal.Y
    -- -> Derive.PassedArgs Signal.Control -> Derive.ControlDeriver
    -> Derive.PassedArgs Signal.Control -> Derive.Deriver Signal.Control
control_interpolate f val args = do
    start <- Derive.passed_real args
    srate <- Util.get_srate
    return $ case Derive.passed_prev_val args of
        -- This can happen a lot when the control track is sliced, and is
        -- nothing to worry about.
        Nothing -> Signal.signal [(start, val)]
        Just (prev, prev_val) ->
            interpolator srate f False prev prev_val start val

-- | TODO more efficient version without the intermediate list
interpolator :: RealTime -> (Double -> Double)
    -> Bool -- ^ include the initial sample or not
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> Signal.Control
interpolator srate f include_initial x0 y0 x1 y1
    | include_initial = Signal.signal sig
    | otherwise = Signal.signal (drop 1 sig)
    where
    sig = [(x, y_of x) | x <- Seq.range_end x0 x1 srate]
    y_of = Num.scale y0 y1 . f . Num.normalize (secs x0) (secs x1) . secs
    secs = RealTime.to_seconds

-- * util

-- | Negative exponents produce a curve that jumps from the "starting point"
-- which doesn't seem too useful, so so hijack the negatives as an easier way
-- to write 1/n.  That way n is smoothly departing, while -n is smoothly
-- approaching.
expon :: Double -> Double -> Double
expon n x = x**exp
    where exp = if n >= 0 then n else 1 / abs n
