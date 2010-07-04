-- | Basic calls for control tracks.
module Derive.Call.Control where

import qualified Util.Num as Num

import Ui
import qualified Ui.Types as Types

import qualified Derive.Call as Call
import Derive.CallSig (required, optional)
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive

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
    [ ("=", Call.c_equal Derive.no_control)
    -- Fallback call will take val-call output.
    , ("", c_set)
    , ("set", c_set)
    , ("i", c_linear)
    , ("e", c_exponential)
    , ("s", c_slide)
    ]

c_set :: Derive.ControlCall
c_set = Derive.generate_one "set" $ \args -> CallSig.call1 args
    (required "val") $ \val -> do
        pos <- Derive.now
        return $ Signal.signal [(pos, val)]

c_linear :: Derive.ControlCall
c_linear = Derive.generate_one "linear" $ \args ->
    case Derive.passed_vals args of
        [] -> case Derive.passed_prev_val args of
            Nothing -> return $ Derive.throw
                "can't set to previous val when there was none"
            Just (_, prev_y) -> return $ do
                pos <- Derive.now
                return $ Signal.signal [(pos, prev_y)]
        _ -> CallSig.call1 args (required "val") $ \val ->
            control_interpolate id val args

c_exponential :: Derive.ControlCall
c_exponential = Derive.generate_one "exponential" $ \args ->
    CallSig.call2 args (required "val", optional "exp" 2) $ \val exp ->
        control_interpolate (expon exp) val args

c_slide :: Derive.ControlCall
c_slide = Derive.generate_one "slide" $ \args -> CallSig.call2 args
    (required "val", optional "time" 0.1) $ \val time -> do
        start <- Derive.now
        end <- case Derive.passed_next_begin args of
            Nothing -> return $ start + RealTime time
            Just n -> do
                next <- Derive.score_to_real n
                return $ min (start + RealTime time) next
        srate <- Call.get_srate
        case Derive.passed_prev_val args of
                Nothing -> do
                    Derive.warn "no previous value to slide from"
                    return $ Signal.signal [(start, val)]
                Just (_, prev_y) -> return $ Signal.signal $
                    interpolate_control True srate id start prev_y end val


-- * control util

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
control_interpolate :: (Double -> Signal.Y) -> Signal.Y
    -> Derive.PassedArgs Derive.Control -> Derive.ControlDeriver
control_interpolate f val args = do
    start <- Derive.now
    srate <- Call.get_srate
    case Derive.passed_prev_val args of
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
