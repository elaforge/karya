-- | Basic calls for control tracks.
module Derive.Call.Control where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (required, optional)
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang

import Util.Control
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * call map

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("=", Derive.transformer "equal" Util.equal_transformer)
    -- Fallback call will take val-call output.
    , ("", c_set)
    , ("set", c_set)
    , ("set-prev", c_set_prev)
    , ("'", c_set_prev)
    , ("i", c_linear)
    , ("i>", c_linear_next)
    , ("e", c_exponential)
    , ("e>", c_exponential_next)
    , ("n", c_neighbor)
    , ("d", c_down)
    , ("u", c_up)

    -- not sure which one I'll like better
    , ("`ped`", c_pedal)
    , ("h", c_pedal)
    ]

c_set :: Derive.ControlCall
c_set = Derive.generator1 "set" $ \args -> CallSig.call1 args
    (required "val") $ \val -> do
        pos <- Args.real_start args
        return $ Signal.signal [(pos, val)]

-- | Re-set the previous val.  This can be used to extend a breakpoint, and is
-- also automatically set by the control track deriver for the hack described
-- in 'Perform.Signal.integrate'.
c_set_prev :: Derive.ControlCall
c_set_prev = Derive.generator "set-prev" $ \args -> CallSig.call0 args $
    case Args.prev_val args of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Args.real_start args
            return $ if pos > prev_x
                then [Signal.signal [(pos, prev_y)]]
                else []

c_linear :: Derive.ControlCall
c_linear = Derive.generator1 "linear" $ \args -> CallSig.call1 args
    (required "val") $ interpolate_prev id args

-- | Linear interpolation to the given value.  This is different than
-- 'c_linear' because it interpolates from the given value to the next value,
-- instead of from the previous value to the given one.
--
-- [val /Number/] Destination value.
--
-- [time /Maybe ScoreOrReal/ Nothing] Time taken to get there.  If not given,
-- slide until the next event.
c_linear_next :: Derive.ControlCall
c_linear_next = Derive.generator1 "linear-next" $ \args ->
    CallSig.call2 args (required "val", optional "time" Nothing) $
        interpolate_next id args

c_exponential :: Derive.ControlCall
c_exponential = Derive.generator1 "exponential" $ \args ->
    CallSig.call2 args (required "val", optional "exp" 2) $ \val exp ->
        interpolate_prev (expon exp) args val

c_exponential_next :: Derive.ControlCall
c_exponential_next = Derive.generator1 "exponential-next" $ \args ->
    CallSig.call3 args (required "val", optional "exp" 2,
        optional "time" Nothing) $ \val exp maybe_time ->
    interpolate_next (expon exp) args val maybe_time

-- | Emit a slide from a value to 0 in absolute time.  This is the control
-- version of the neighbor pitch call.
--
-- [neighbor /Number/ @1@] Neighbor value.
--
-- [time /ScoreOrReal/ @.3@] Time taken to get to 0.
c_neighbor :: Derive.ControlCall
c_neighbor = Derive.generator1 "neighbor" $ \args -> CallSig.call2 args
    (optional "neighbor" 1, optional "time" (TrackLang.real 0.1)) $
    \neighbor (TrackLang.DefaultReal time) -> do
        (start, end) <- Util.duration_from_start args time
        srate <- Util.get_srate
        return $ interpolator srate id True start neighbor end 0

-- | Descend with a given slope until the value reaches 0 or the next event.
c_down :: Derive.ControlCall
c_down = Derive.generator1 "down" $ \args -> slope args $
    \start next prev_y speed ->
        let diff = RealTime.to_seconds (next - start) * speed
            end = min next
                (start + RealTime.seconds prev_y / RealTime.seconds speed)
        in (end, max 0 (prev_y - diff))

-- | Ascend with a given slope until the value reaches 1 or the next event.
c_up :: Derive.ControlCall
c_up = Derive.generator1 "up" $ \args -> slope args $
    \start next prev_y speed ->
        let diff = RealTime.to_seconds (next - start) * speed
            end = min next
                (start + RealTime.seconds (1-prev_y) / RealTime.seconds speed)
        in (end, min 1 (prev_y + diff))

slope :: Derive.PassedArgs Signal.Control
    -> (RealTime -> RealTime -> Signal.Y -> Double -> (RealTime, Signal.Y))
    -> Derive.Deriver Signal.Control
slope args f = CallSig.call1 args (optional "speed" 1) $ \speed ->
    case Args.prev_val args of
        Nothing -> return Signal.empty
        Just (_, prev_y) -> do
            start <- Args.real_start args
            next <- Derive.real (Args.end args)
            srate <- Util.get_srate
            let (end, dest) = f start next prev_y speed
            return $ interpolator srate id True start prev_y end dest

-- | Unlike most control events, this uses a duration.  Set the control to the
-- given value for the event's duration, and reset to the old value
-- afterwards.
--
-- Mostly makes sense with pedal type controls.
c_pedal :: Derive.ControlCall
c_pedal = Derive.generator1 "pedal" $ \args -> CallSig.call1 args
    (optional "val" 1) $ \val -> do
        (start, end) <- Args.real_range args
        let prev = maybe 0 snd (Args.prev_val args)
        return $ Signal.signal [(start, val), (end, prev)]

-- * control util

type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -- ^ start -> starty -> end -> endy
    -> Signal.Control

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
interpolate_prev :: (Double -> Signal.Y) -> Derive.PassedArgs Signal.Control
    -> Signal.Y -> Derive.Deriver Signal.Control
interpolate_prev f args val = do
    start <- Args.real_start args
    srate <- Util.get_srate
    return $ case Args.prev_val args of
        -- This can happen a lot when the control track is sliced, and is
        -- nothing to worry about.
        Nothing -> Signal.signal [(start, val)]
        Just (prev, prev_val) ->
            interpolator srate f False prev prev_val start val

-- | Similar to 'interpolate_prev', except interpolate to the given val between
-- here and a time in the future, rather than between the previous event and
-- here.
interpolate_next :: (Double -> Signal.Y) -> Derive.PassedArgs Signal.Control
    -> Signal.Y -> Maybe TrackLang.DefaultReal
    -- ^ if given, end after this duration, if not end at the next event
    -> Derive.Deriver Signal.Control
interpolate_next f args val maybe_time = do
    (start, end) <- case maybe_time of
        Nothing -> (,) <$> Args.real_start args
            <*> Derive.real (Args.end args)
        Just (TrackLang.DefaultReal time) ->
            Util.duration_from_start args time
    srate <- Util.get_srate
    return $ case Args.prev_val args of
        Nothing -> Signal.signal [(start, val)]
        Just (_, prev_y) -> interpolator srate f True start prev_y end val

interpolator :: RealTime -> (Double -> Double) -> Interpolator
interpolator srate f include_initial x0 y0 x1 y1 =
    Signal.signal $ (if include_initial then id else drop 1) sig
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
