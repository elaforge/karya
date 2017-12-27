-- | A signal modeled as linear segments.
module Util.Linear where
import qualified Data.Vector.Generic as V
import qualified Util.TimeVector as TimeVector
import Util.TimeVector (X, Sample(..), to_pair)


type Y = TimeVector.UnboxedY

{-

The in-memory representation should be like TimeVector: (x, y), where two
concurrent xs denote a discontinuity.

But the high-level API shouldn't give access to samples, and I should be able
to slice without being error prone.

Linear transformations keep segments.
Integration should keep them too if possible.

Can composition keep them?


Problems I'm trying to solve:

- Need to remember to use Signal.set, and provide previous y.  Signals should
be implicitly flat in both directions, and merging them should respect that.

- Transpose signal resampling should "just work", without needing special
'at_before' stuff.  Why does it require that now?

- Awkward rules where the sample at 0 sets values before.  Or at least the part
where it leads to errors when asking for the value at <0.

- Include shift so I can get rid of Score.Event 'untransformed' nonsense.
I think the problem before is having to use the shift on all signal operations
even when it's 0 for everyone except Event seemed over complex.  Also that I
just want a per-event offset, not one for each signal inside.

Though if I extend this to stretch as well, then I don't need a separate Warp
type.  I don't think anyone except warp would use it though.

How much would I solve just be changing (<>)?

-}

newtype Signal = Signal TimeVector.Unboxed
    deriving (Eq, Show)

data Segment y = Segment {
    x1 :: !X, y1 :: !y
    , x2 :: !X, y2 :: !y
    } deriving (Eq, Show)

signal :: [Segment Y] -> Signal
signal = Signal . V.fromList . strip . toList
    where
    toList (Segment x1 y1 x2 y2 : segments) =
        Sample x1 y1 : Sample x2 y2 : toList segments
    toList [] = []
    -- Two segments on the same line.  I could try to project lines at any
    -- angle but let's not bother right now.
    strip (Sample x1 _ : sn@(Sample x2 _ : _)) | x2 < x1 = strip sn
    strip (s1@(Sample _ y1) : Sample _ y2 : sn@(Sample _ y3 : _))
        | y1 == y2 && y2 == y3 = strip (s1 : sn)
    -- Abbreviate concident samples.
    strip (Sample x1 y1 : samples@(Sample x2 y2 : _))
        | x1 == x2 && y1 == y2 = strip samples
    strip (s1:sn) = s1 : strip sn
    strip [] = []

unsignal :: Signal -> [Segment Y]
unsignal (Signal v) = go $ V.toList v
    where
    go [] = []
    go [_] = []
    go (Sample x1 y1 : xs@(Sample x2 y2 : _))
        | x1 == x2 = go xs
        | otherwise = Segment x1 y1 x2 y2 : go xs

at :: Signal -> X -> Y
at (Signal v) x = interpolate v (TimeVector.highest_index x v)
    where
    interpolate vec i
        | TimeVector.null vec = 0
        | i + 1 >= TimeVector.length vec = y0
        | i < 0 = y0
        | otherwise = TimeVector.y_at x0 y0 x1 y1 x
        where
        Sample x0 y0 = V.unsafeIndex v i
        Sample x1 y1 = V.unsafeIndex v (i+1)

segment_at :: Signal -> X -> Maybe (Segment Y)
segment_at (Signal v) x
    | i < 0 = Nothing
    | i + 1 >= TimeVector.length v = Nothing
    | otherwise =
        let Sample x1 y1 = V.unsafeIndex v i
            Sample x2 y2 = V.unsafeIndex v (i+1)
        in Just $ Segment x1 y1 x2 y2
    where
    i = TimeVector.highest_index x v
