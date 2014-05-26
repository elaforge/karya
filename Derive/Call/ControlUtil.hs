-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities that emit 'Signal.Control's and 'Derive.ControlMod's.
module Derive.Call.ControlUtil where
import qualified Data.Monoid as Monoid

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.Call.Util as Util
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


type Interpolator = Bool -- ^ include the initial sample or not
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y
    -- ^ start -> starty -> end -> endy
    -> Signal.Control

-- | Sampling rate.
type SRate = RealTime

-- | Interpolation function.  This maps a straight line to the desirned curve.
type Function = Double -> Double

-- | Create an interpolating call, from a certain duration (positive or
-- negative) from the event start to the event start.
interpolate :: Function -> Derive.ControlArgs
    -> Signal.Y -> TrackLang.Duration -> Derive.Deriver Signal.Control
interpolate f args val dur = do
    (start, end) <- Util.duration_from_start args dur
    srate <- Util.get_srate
    return $ case Args.prev_control args of
        Nothing -> Signal.signal [(start, val)]
        -- I always set include_initial.  It might be redundant, but if the
        -- previous call was sliced off, it won't be.
        Just (_, prev_val) -> interpolator srate f True
            (min start end) prev_val (max start end) val

interpolator :: SRate -> Function -> Interpolator
interpolator srate f include_initial x1 y1 x2 y2 =
    (if include_initial then id else Signal.drop 1)
        (interpolate_segment True srate f x1 y1 x2 y2)

-- | Interpolate between the given points.
interpolate_segment :: Bool -> SRate -> Function
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y -> Signal.Control
interpolate_segment include_end srate f x1 y1 x2 y2 =
    Signal.unfoldr go (Seq.range_ x1 srate)
    where
    go [] = Nothing
    go (x:xs)
        | x >= x2 = if include_end then Just ((x2, y2), []) else Nothing
        | otherwise = Just ((x, y_of x), xs)
    y_of = Num.scale y1 y2 . f . Num.normalize (secs x1) (secs x2) . secs
    secs = RealTime.to_seconds

exp_doc :: Text
exp_doc = "Slope of an exponential curve. Positive `n` is taken as `x^n`\
    \ and will generate a slowly departing and rapidly approaching\
    \ curve. Negative `-n` is taken as `x^1/n`, which will generate a\
    \ rapidly departing and slowly approaching curve."

-- | Negative exponents produce a curve that jumps from the \"starting point\"
-- which doesn't seem too useful, so so hijack the negatives as an easier way
-- to write 1/n.  That way n is smoothly departing, while -n is smoothly
-- approaching.
expon :: Double -> Double -> Double
expon n x = x**exp
    where exp = if n >= 0 then n else 1 / abs n

-- | I could probably make a nicer curve of this general shape if I knew more
-- math.
expon2 :: Double -> Double -> Double -> Double
expon2 a b x
    | x >= 1 = 1
    | x < 0.5 = expon a (x * 2) / 2
    | otherwise = expon (-b) ((x-0.5) * 2) / 2 + 0.5

-- * breakpoints

breakpoints :: SRate -> Function -> RealTime -> RealTime -> [Signal.Y]
    -> Signal.Control
breakpoints srate f =
    signal_breakpoints Signal.signal (interpolate_segment False srate f)

signal_breakpoints :: Monoid.Monoid sig => ([(RealTime, y)] -> sig)
    -> (RealTime -> y -> RealTime -> y -> sig) -> RealTime -> RealTime
    -> [y] -> sig
signal_breakpoints make_signal segment start end =
    mconcatMap line . Seq.zip_next . make_breakpoints start end
    where
    line ((x1, y1), Just (x2, y2)) = segment x1 y1 x2 y2
    line ((x1, y2), Nothing) = make_signal [(x1, y2)]

make_breakpoints :: RealTime -> RealTime -> [a] -> [(RealTime, a)]
make_breakpoints start end vals = case vals of
    [] -> []
    [x] -> [(start, x)]
    _ -> [(Num.scale start end (n / (len - 1)), x)
        | (n, x) <- zip (Seq.range_ 0 1) vals]
    where len = fromIntegral (length vals)

-- * control mod

multiply_dyn :: RealTime -> Signal.Control -> Derive.Deriver ()
multiply_dyn = multiply_signal Controls.dynamic

-- | Emit a multiplying modify control.
multiply_signal :: Score.Control -> RealTime
    -- ^ End time, after which the signal becomes 1.  This should be set to the
    -- next event, otherwise, all subsequent events will be zeroed.
    -> Signal.Control -> Derive.Deriver ()
multiply_signal control end sig = do
    -- Since signals are implicitly 0 before the first sample, the modification
    -- will zero out the control before 'x1'.  That's usually not what I want,
    -- so assume it's 'y1' before that.
    Derive.modify_control (Derive.Merge Derive.op_mul) control $
        initial <> sig <> Signal.signal [(end, 1)]
    where
    initial = case Signal.head sig of
        Nothing -> mempty
        Just (_, y) -> Signal.signal [(0, y)]

add_control :: Score.Control -> (Double -> Double)
    -> RealTime -> Signal.Y -> RealTime -> Signal.Y -> Derive.Deriver ()
add_control control f x1 y1 x2 y2 = do
    sig <- make_signal f x1 y1 x2 y2
    Derive.modify_control (Derive.Merge Derive.op_add) control sig

make_signal :: (Double -> Double) -> RealTime -> Signal.Y -> RealTime
    -> Signal.Y -> Derive.Deriver Signal.Control
make_signal f x1 y1 x2 y2 = do
    srate <- Util.get_srate
    return $ interpolator srate f True x1 y1 x2 y2
