-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for the 'Warp'.
module Derive.Warp (
    Warp, Linear(..), is_linear, is_identity, warp, unwarp
    , identity, from_signal, compose
    , shift, stretch
    -- * utils
    , unwarp_signal
    -- * compose_hybrid
    , compose_hybrid
) where
import qualified Control.DeepSeq as DeepSeq

import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.RealTime as RealTime
import Perform.RealTime (to_score)
import qualified Perform.Signal2 as Signal
import Global
import Types


{- | The 'Warp' keeps track of the ScoreTime -> RealTime function, as well
    as its inverse.

    This treats linear warps specially, since they're common and allow some
    optimizations.

    The main transformation is 'compose', but 'shift' and 'stretch' are
    shortcuts for composing with @f(x) = y + shift@ or @f(x) = y * stretch@,
    respectively.

    The confusing thing is that shift and stretch compose with the *input* of
    the function, so @shift n@ is @(`compose` shift n identity)@.  This means
    that @shift 1 . stretch 2@ is actually @(*2) . (+1)@, which makes
    compositions look backwards.  It turns out this is more convenient since
    Deriver level shift and stretch go in left-to-right order since monadic
    effects go left to right:
    reason that

    > Reader.runReader (Reader.local (+1) . Reader.local (*2) $ Reader.ask) 0

    is 2, not 1.

    There is probably some kind of theory to do with positive position or
    negative position or some such thing, but my tiny brain can't quite get a
    handle on it, so all I can say is that this is the way that makes things
    work out right.
-}
data Warp = WarpFunction !Function | WarpLinear !Linear

data Function = Function {
    _warp :: !(ScoreTime -> RealTime)
    , _unwarp :: !(RealTime -> ScoreTime)
    -- | For debugging.  TODO keep all of them?  Or is it a memory leak?
    -- , _signal :: !Signal.Warp
    }

data Linear = Linear { _shift :: !RealTime, _stretch :: !RealTime }
    deriving (Show)

instance Show Warp where show = prettys
instance Pretty Warp where
    pretty (WarpFunction f) = "((Warp " <> pretty (_warp f 0) <> "--"
        <> pretty (_warp f 1) <> "))"
    pretty (WarpLinear (Linear shift stretch)) =
        "((Warp *" <> pretty stretch <> "+" <> pretty shift <> "))"

-- | I can't really rnf functions, but maybe there will be data in here someday.
instance DeepSeq.NFData Warp where rnf _ = ()

is_linear :: Warp -> Maybe Linear
is_linear (WarpLinear linear) = Just linear
is_linear _ = Nothing

is_identity :: Warp -> Bool
is_identity (WarpLinear (Linear shift stretch)) = shift == 0 && stretch == 1
is_identity _ = False

warp :: Warp -> ScoreTime -> RealTime
warp (WarpFunction f) t = _warp f t
warp (WarpLinear w) t = to_real t * _stretch w + _shift w

-- | The inverse of 'warp'.  I originally would fail when the RealTime
-- doesn't occur in the Warp, but now I extend it in the same way as
-- 'warp'.  Failing caused awkwardness with events at the end of the score.
unwarp :: Warp -> RealTime -> ScoreTime
unwarp (WarpFunction f) = _unwarp f
unwarp (WarpLinear w) = to_score . (/ _stretch w) . subtract (_shift w)

-- | 1:1 identity warp.  Previously I could detect this to optimize it away,
-- but since 'compose' is cheap now, I might not need that anymore.
identity :: Warp
identity = WarpLinear (Linear 0 1)

signal_identity :: Warp
signal_identity = from_signal $ Signal.from_pairs
    [(0, 0), (RealTime.large, RealTime.to_seconds RealTime.large)]

-- | Create a Warp from a signal and its inverse.  This assumes the signal
-- is monotonically increasing.
-- TODO error on empty signal?
from_signal :: Signal.Warp -> Warp
from_signal signal = WarpFunction $ Function
    { _warp = RealTime.seconds . flip Signal.at signal . to_real
    , _unwarp = ScoreTime.double . flip Signal.at inverted
    }
    where inverted = Signal.invert signal

compose :: Warp -> Warp -> Warp
compose (WarpLinear (Linear shift1 stretch1))
        (WarpLinear (Linear shift2 stretch2)) =
    WarpLinear $ Linear (shift1 + shift2 * stretch1) (stretch1 * stretch2)
compose (WarpFunction (Function warp1 unwarp1))
        (WarpFunction (Function warp2 unwarp2)) =
    WarpFunction $ Function
        { _warp = warp1 . to_score . warp2
        , _unwarp = unwarp2 . to_real . unwarp1
        }
compose (WarpLinear linear) (WarpFunction f) =
    compose (to_function linear) (WarpFunction f)
compose (WarpFunction f) (WarpLinear linear) =
    compose (WarpFunction f) (to_function linear)

to_function :: Linear -> Warp
to_function w = stretch (to_score (_stretch w)) $
    shift (to_score (_shift w)) signal_identity

-- | "See 'Warp'.
shift :: ScoreTime -> Warp -> Warp
shift 0 w = w
shift x (WarpFunction (Function warp unwarp)) = WarpFunction $ Function
    { _warp = warp . (+x)
    , _unwarp = subtract x . unwarp
    }
shift x (WarpLinear linear) = WarpLinear $ Linear
    { _shift = _shift linear + _stretch linear * to_real x
    , _stretch = _stretch linear
    }

-- | See 'Warp'.
--
-- Previously, this would disallow <=0 stretch, but it turns out to be useful
-- to stretch events to 0, and to negative durations.
stretch :: ScoreTime -> Warp -> Warp
stretch 1 w = w
stretch factor (WarpFunction (Function warp unwarp)) = WarpFunction $ Function
    { _warp = warp . (*factor)
    , _unwarp = (/factor) . unwarp
    }
stretch factor (WarpLinear linear) = WarpLinear $ Linear
    { _shift = _shift linear
    , _stretch = to_real factor * _stretch linear
    }

-- * utils

unwarp_signal :: Warp -> Signal.Control -> Signal.Display
unwarp_signal w = Signal.coerce . Signal.map_x (warp w . to_score)


-- * compose_hybrid

-- TODO I'll need a different approach.  Can't I modify the signal and then
-- compose normally?
compose_hybrid :: Warp -> Warp -> Warp
compose_hybrid _ _ = identity

to_real :: ScoreTime -> RealTime
to_real = RealTime.score

{-
-- | This is like 'compose', but implements a kind of \"semi-absolute\"
-- composition.  The idea is that it's normal composition until the second
-- signal has a slope of zero.  Normally this would be a discontinuity, but
-- is special cased to force the output to a 1\/1 line.  In effect, it's as
-- if the flat segment were whatever slope is necessary to to generate a slope
-- of 1 when composed with the first signal.
compose_hybrid :: Warp -> Warp -> Warp
compose_hybrid f g = Signal $ run initial $ Vector.generateM (length g) gen
    where
    -- If 'g' starts with a flat segment, I need to start the linear bit in the
    -- right place.
    initial = (at_linear (y_to_x y) f, 0)
        where y = maybe 0 snd (head g)
    run state m = Identity.runIdentity $ Monad.State.evalStateT m state
    -- Where h = f•g:
    -- If g(x_t) == g(x_t-1), then this is a flat segment.
    -- h(x) is simply h(x_t-1) + (x_t - x_t-1), but I have to store an
    -- offset that represents where the signal would be were just right to
    -- produce a slope of 1, so I work backwards:
    -- offset = f-1(h(x)) - g(x_t-1)
    --
    -- If g(x_t) > g(x_t-1), then this is a normal positive slope, and I
    -- have to add the offset: h(x) = f(g(x + offset)).
    --
    -- So the state is (h(x_t-1), offset).
    gen i
        | gy0 == gy = gen_flat gx gx0 gy0
        | otherwise = gen_normal gx gy
        where
        Sample gx gy = Vector.unsafeIndex (sig_vec g) i
        Sample gx0 gy0
            | i == 0 = Sample 0 0
            | otherwise = Vector.unsafeIndex (sig_vec g) (i-1)
    gen_flat gx gx0 gy0 = do
        (y0, _) <- Monad.State.get
        let y = y0 + x_to_y (gx - gx0)
            offset = inverse_at_extend y f - y_to_x gy0
        Monad.State.put (y, offset)
        return $ Sample gx y
    gen_normal gx gy = do
        (_, offset) <- Monad.State.get
        let y = at_linear (y_to_x gy + offset) f
        Monad.State.put (y, offset)
        return $ Sample gx y

-- | This is like 'inverse_at', except that if the Y value is past the end
-- of the signal, it extends the signal as far as necessary.  When used for
-- warp composition or unwarping, this means that the parent warp is too small
-- for the child.  Normally this shouldn't happen, but if it does it's
-- sometimes better to make something up than crash.
--
-- The rules for extension are the same as 'at_linear_extend', and this
-- function should be the inverse of that one.  This ensures that if you warp
-- and then unwarp a time, you get your original time back.
inverse_at_extend :: Y -> Warp -> X
inverse_at_extend y (Signal vec)
    | TimeVector.null vec = y_to_x y
    -- Nothing means the line is flat and will never reach Y.  I pick a big
    -- X instead of crashing.
    | otherwise = fromMaybe RealTime.large $ TimeVector.x_at x0 y0 x1 y1 y
    where
    -- Has to be the highest index, or it gets hung up on a flat segment.
    i = index_above_y y vec
    (Sample x0 y0, Sample x1 y1)
        | len == 1 =
            let at0@(Sample x0 y0) = index 0
            in (at0, Sample (x0+1) (y0+1))
        | i >= TimeVector.length vec = (index (i-2), index (i-1))
        | i == 0 = (index 0, index 1)
        | otherwise = (index (i-1), index i)
        where len = TimeVector.length vec
    index = TimeVector.index vec

index_above_y :: Y -> TimeVector.Unboxed -> Int
index_above_y y vec = go 0 (TimeVector.length vec)
    where
    go low high
        | low == high = low
        | y >= sy (TimeVector.unsafeIndex vec mid) = go (mid+1) high
        | otherwise = go low mid
        where mid = (low + high) `div` 2

-}
