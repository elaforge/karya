-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions for the 'Warp'.
module Derive.Warp (
    Warp, warp, unwarp
    , identity, from_signal, compose
    , shift, stretch, place
) where
import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal2 as Signal
import Types


-- | The 'Warp' keeps track of the ScoreTime -> RealTime function, as well
-- as its inverse.
data Warp = Warp {
    _warp :: !(ScoreTime -> RealTime)
    , _unwarp :: !(RealTime -> ScoreTime)
    -- | For debugging.  TODO keep all of them?  Or is it a memory leak?
    -- , _signal :: !Signal.Warp
    }

instance Show Warp where show w = "((Warp " ++ show (_warp w 0) ++ "))"

warp :: Warp -> ScoreTime -> RealTime
warp = _warp

-- | The inverse of 'warp'.  I originally would fail when the RealTime
-- doesn't occur in the Warp, but now I extend it in the same way as
-- 'warp'.  Failing caused awkwardness with events at the end of the score.
unwarp :: Warp -> RealTime -> ScoreTime
unwarp = _unwarp

-- | 1:1 identity warp.  Previously I could detect this to optimize it away,
-- but since 'compose' is cheap now, I might not need that anymore.
identity :: Warp
identity = Warp
    { _warp = RealTime.score
    , _unwarp = RealTime.to_score
    -- , _signal = mempty
    }

-- | Create a Warp from a signal and its inverse.  This assumes the signal
-- is monotonically increasing.
from_signal :: Signal.Warp -> Warp
from_signal signal = Warp
    { _warp = RealTime.seconds . flip Signal.at signal . RealTime.score
    , _unwarp = ScoreTime.double . flip Signal.at inverted
    -- , _signal = signal
    }
    where
    inverted = Signal.invert signal

compose :: Warp -> Warp -> Warp
compose (Warp warp1 unwarp1) (Warp warp2 unwarp2) = Warp
    { _warp = warp1 . RealTime.to_score . warp2
    , _unwarp = unwarp1 . RealTime.score . unwarp2
    }

shift :: ScoreTime -> Warp -> Warp
shift x warp = Warp
    -- { _warp = \val -> _warp warp . Debug.trace_ret ("+"<>pretty x) val . (+x) $ val
    { _warp = _warp warp . (+x)
    , _unwarp = subtract x . _unwarp warp
    }

stretch :: ScoreTime -> Warp -> Warp
stretch factor warp = Warp
    -- { _warp = \val -> _warp warp . Debug.trace_ret ("*" <> pretty factor) val . (*factor) $ val
    { _warp = _warp warp . (*factor)
    , _unwarp = (/factor) . _unwarp warp
    }

-- | 'shift' and 'stretch' in one.  It might be a bit more efficient than using
-- them separately, but at least is shorter to write.  The order is stretch,
-- then shift.
place :: ScoreTime -- ^ shift
    -> ScoreTime -- ^ stretch
    -> Warp -> Warp
place x factor warp = warp
    { _warp = _warp warp . (+x) . (*factor)
    , _unwarp = (/factor) . subtract x . _unwarp warp
    }

-- TODO I'll need a different approach.  Can't I modify the signal and then
-- compose normally?
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
