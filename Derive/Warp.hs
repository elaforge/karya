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
