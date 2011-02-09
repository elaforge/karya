-- | Basic calls for control tracks.
module Derive.Call.Control where
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq

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
    [ ("=", Call.c_equal)
    -- Fallback call will take val-call output.
    , ("", c_set)
    , ("set", c_set)
    , ("i", c_linear)
    , ("e", c_exponential)
    , ("s", c_slide)
    ]

c_set :: Derive.ControlCall
c_set = Derive.generator1 "set" $ \args -> CallSig.call1 args
    (required "val") $ \val -> do
        pos <- Derive.now
        return $ Signal.signal [(pos, val)]

c_linear :: Derive.ControlCall
c_linear = Derive.generator1 "linear" $ \args ->
    case Derive.passed_vals args of
        [] -> case Derive.passed_prev_val args of
            Nothing -> Derive.throw
                "can't set to previous val when there was none"
            Just (_, prev_y) -> do
                pos <- Derive.now
                return $ Signal.signal [(pos, prev_y)]
        _ -> CallSig.call1 args (required "val") $ \val ->
            control_interpolate id val args

c_exponential :: Derive.ControlCall
c_exponential = Derive.generator1 "exponential" $ \args ->
    CallSig.call2 args (required "val", optional "exp" 2) $ \val exp ->
        control_interpolate (expon exp) val args

c_slide :: Derive.ControlCall
c_slide = Derive.generator1 "slide" $ \args -> CallSig.call2 args
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
                    Log.warn "no previous value to slide from"
                    return $ Signal.signal [(start, val)]
                Just (_, prev_y) -> return $
                    interpolator srate id True start prev_y end val


-- * control util

-- | Create samples according to an interpolator function.  The function is
-- passed values from 0--1 representing position in time and is expected to
-- return values from 0--1 representing the Y position at that time.  So linear
-- interpolation is simply @id@.
control_interpolate :: (Double -> Signal.Y) -> Signal.Y
    -- -> Derive.PassedArgs Signal.Control -> Derive.ControlDeriver
    -> Derive.PassedArgs Signal.Control -> Derive.Deriver Signal.Control
control_interpolate f val args = do
    start <- Derive.now
    srate <- Call.get_srate
    case Derive.passed_prev_val args of
        Nothing -> do
            Log.warn "no previous value to interpolate from"
            return $ Signal.signal [(start, val)]
        Just (prev, prev_val) -> return $
            interpolator srate f False prev prev_val start val

-- | TODO more efficient version without the intermediate list
interpolator :: RealTime -> (Double -> Double) -> Call.ControlInterpolator
interpolator srate f include_initial x0 y0 x1 y1
    | include_initial = Signal.signal sig
    | otherwise = Signal.signal (drop 1 sig)
    where
    sig = [(x, y_of x) | x <- Seq.range x0 x1 srate]
    y_of = Num.scale y0 y1 . f . Types.real_to_double . Num.normalize x0 x1

-- * util

-- | Negative exponents produce a curve that jumps from the "starting point"
-- which doesn't seem too useful, so so hijack the negatives as an easier way
-- to write 1/n.  That way n is smoothly departing, while -n is smoothly
-- approaching.
expon :: Double -> Double -> Double
expon n x = x**exp
    where exp = if n >= 0 then n else (1 / abs n)
