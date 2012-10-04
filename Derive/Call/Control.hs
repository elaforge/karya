-- | Basic calls for control tracks.
module Derive.Call.Control where
import qualified Data.Set as Set

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import Derive.CallSig (required, optional)
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * call map

control_calls :: Derive.ControlCallMap
control_calls = Derive.make_calls
    [ ("=", Util.c_equal)
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

-- | This should contain the calls that require the previous value.  It's used
-- by a hack in 'Derive.Slice.slice'.
require_previous :: Set.Set String
require_previous = Set.fromList ["'", "u", "d"]

c_set :: Derive.ControlCall
c_set = Derive.generator1 "set" "Emit a sample with no interpolation." $
    CallSig.call1g (required "val" "Destination value.") $ \val args -> do
        pos <- Args.real_start args
        return $ Signal.signal [(pos, val)]

-- | Re-set the previous val.  This can be used to extend a breakpoint, and is
-- also automatically set by the control track deriver for the hack described
-- in 'Perform.Signal.integrate'.
c_set_prev :: Derive.ControlCall
c_set_prev = Derive.generator "set-prev"
    ("Re-set the previous value.  This can be used to extend a breakpoint,"
    <> " and is also automatically set by the control track deriver for"
    <> " the hack described in 'Perform.Signal.integrate'."
    ) $ CallSig.call0g $ \args -> case Args.prev_val args of
        Nothing -> return []
        Just (prev_x, prev_y) -> do
            pos <- Args.real_start args
            return $ if pos > prev_x
                then [Signal.signal [(pos, prev_y)]]
                else []

c_linear :: Derive.ControlCall
c_linear = Derive.generator1 "linear"
    ("Interpolate from the previous sample to the given value in a straight"
    <> " line, ending at the current time."
    ) $ CallSig.call1g (required "val" "Destination value.") $ \val args ->
        interpolate_prev id args val

c_linear_next :: Derive.ControlCall
c_linear_next = Derive.generator1 "linear-next"
    ("Interpolate from the previous sample to the given value in a straight"
    <> " line, starting here and ending at some time in the future."
    ) $ CallSig.call2g
    ( required "val" "Destination value."
    , optional "time" Nothing $ "Time to reach destination.  If not given, it "
        <> "is the start of the next event."
    ) $ \val maybe_time args -> interpolate_next id args val maybe_time

c_exponential :: Derive.ControlCall
c_exponential = Derive.generator1 "exponential"
    ( "Interpolate from the previous sample to the given value in a curve,"
    <> " ending at the current time."
    ) $ CallSig.call2g
    (required "val" "Destination value."
    , optional "exp" 2 exp_doc
    ) $ \val exp args -> interpolate_prev (expon exp) args val

c_exponential_next :: Derive.ControlCall
c_exponential_next = Derive.generator1 "exponential-next"
    ("Interpolate from the previous sample to the given value in a curve,"
    <> " starting here and ending at some time in the future."
    ) $ CallSig.call3g
    ( required "val" "Destination value."
    , optional "exp" 2 exp_doc
    , optional "time" Nothing $ "Time to reach destination.  If not given, it "
        <> "is the start of the next event."
    ) $ \val exp maybe_time args ->
        interpolate_next (expon exp) args val maybe_time

exp_doc :: String
exp_doc = "Slope of an exponential curve. Positive `n` is taken as `x^n` "
    <> "and will generate a slowly departing and rapidly approaching "
    <> "curve. Negative `-n` is taken as `x^1/n`, which will generate a "
    <> "rapidly departing and slowly approaching curve."

c_neighbor :: Derive.ControlCall
c_neighbor = Derive.generator1 "neighbor"
    ("Emit a slide from a value to 0 in absolute time. This is the control"
    <> " equivalent of the neighbor pitch call."
    ) $ CallSig.call2g
    ( optional "neighbor" 1 "Start at this value."
    , optional "time" (TrackLang.real 0.1) "Time taken to get to 0."
    ) $ \neighbor (TrackLang.DefaultReal time) args -> do
        (start, end) <- Util.duration_from_start args time
        srate <- Util.get_srate
        return $ interpolator srate id True start neighbor end 0

c_down :: Derive.ControlCall
c_down = Derive.generator1 "down"
    ("Descend at the given speed until the value reaches 0 or the next event."
    ) $
    CallSig.call1g (optional "speed" 1 "Descend this amount per second.") $
    \speed args -> slope args $ \start next prev_y ->
        let diff = RealTime.to_seconds (next - start) * speed
            end = min next
                (start + RealTime.seconds prev_y / RealTime.seconds speed)
        in (end, max 0 (prev_y - diff))

c_up :: Derive.ControlCall
c_up = Derive.generator1 "up"
    ("Ascend at the given speed until the value reaches 1 or the next event."
    ) $
    CallSig.call1g (optional "speed" 1 "Ascend this amount per second.") $
    \speed args -> slope args $ \start next prev_y ->
        let diff = RealTime.to_seconds (next - start) * speed
            end = min next
                (start + RealTime.seconds (1-prev_y) / RealTime.seconds speed)
        in (end, min 1 (prev_y + diff))

slope :: Derive.PassedArgs Signal.Control
    -> (RealTime -> RealTime -> Signal.Y -> (RealTime, Signal.Y))
    -> Derive.Deriver Signal.Control
slope args f = case Args.prev_val args of
    Nothing -> return Signal.empty
    Just (_, prev_y) -> do
        start <- Args.real_start args
        next <- Derive.real (Args.next args)
        srate <- Util.get_srate
        let (end, dest) = f start next prev_y
        return $ interpolator srate id True start prev_y end dest

c_pedal :: Derive.ControlCall
c_pedal = Derive.generator1 "pedal"
    ("Unlike most control events, this uses a duration. Set the control to"
    <> " the given value for the event's duration, and reset to the old"
    <> " value afterwards."
    ) $ CallSig.call1g (optional "val" 1 "Set to this value.") $
    \val args -> do
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
            <*> Derive.real (Args.next args)
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
