{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- instances for RealTime
module Perform.RealTime where
import Prelude hiding (div, max)
import qualified Prelude
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Int as Int
#ifdef TESTING
import qualified Data.Ratio as Ratio
#endif
import qualified Foreign as Foreign

import qualified Util.Pretty as Pretty

import qualified Ui.Types as Types


-- | A concrete unit of time.
--
-- This must have negative values because it's used for signals, which are
-- used for the warp map, which is oriented with zero at the note start.  If
-- a note wants to get the real time before it, it must look up a negative
-- RealTime.
newtype RealTime = RealTime Int.Int64
    deriving (DeepSeq.NFData, Foreign.Storable, Eq, Ord)

-- | This loses precision so show /= read, but no one should be relying on that
-- anyway.
instance Show RealTime where
    show t = show (to_seconds t) ++ "s"
instance Pretty.Pretty RealTime where
    pretty t = Pretty.show_float (Just 2) (to_seconds t) ++ "s"

-- | The unintuitive definitions make me a little nervous but it's not like
-- it's any worse than the Word instances.
instance Num RealTime where
    RealTime a + RealTime b = RealTime (a + b)
    RealTime a - RealTime b = RealTime (a - b)
    -- This makes me nervous because time * time = time handles units
    -- incorrectly.  But I can't not have a (*) operator without getting
    -- runtime errors, and so I might as well have one the acts intuitively.
    a * b = mul a (to_seconds b)
    negate (RealTime a) = RealTime (negate a)
    abs (RealTime a) = RealTime (abs a)
    signum (RealTime a) = RealTime (signum a)
    fromInteger = RealTime . (*time_factor) . fromIntegral

#ifdef TESTING
-- This instance is incorrect because you can't divide time by time and get
-- time.  However, it's needed for floating point literal syntax which is
-- extremely useful when writing tests.  It's dangerous for real code though,
-- because floating point is not exact.
instance Fractional RealTime where
    fromRational ratio = seconds $
        fromIntegral (Ratio.numerator ratio)
            / fromIntegral (Ratio.denominator ratio)
    a / b = error $ show a ++ " / " ++ show b
        ++ ": Fractional instance is only for tests"
#endif

div :: RealTime -> Double -> RealTime
div a b = seconds (to_seconds a / b)
infixl 7 `div`

mul :: RealTime -> Double -> RealTime
mul a b = seconds (to_seconds a * b)
infixl 7 `mul`

time_factor :: Int.Int64
time_factor = 1000000

-- | A large RealTime that is also not the max bound so it won't overflow
-- too easily, and will also fit in a Signal.Y.
large :: RealTime
large = RealTime (2^32)

-- * convert to

seconds :: Double -> RealTime
seconds s = RealTime (round (s * fromIntegral time_factor))

milliseconds :: Int -> RealTime
milliseconds = microseconds . (*1000) . fromIntegral

microseconds :: Int.Int64 -> RealTime
microseconds = RealTime

score :: Types.ScoreTime -> RealTime
score = seconds . Types.score_to_double

-- * convert from

to_seconds :: RealTime -> Double
to_seconds (RealTime us) = fromIntegral us / fromIntegral time_factor

to_milliseconds :: RealTime -> Integer
to_milliseconds = fromIntegral . (`Prelude.div` 1000) . to_microseconds

to_microseconds :: RealTime -> Int.Int64
to_microseconds (RealTime us) = us

to_score :: RealTime -> Types.ScoreTime
to_score = Types.double_to_score . to_seconds

-- | May overflow!
-- TODO should I error on overflow?
to_int :: RealTime -> Int
to_int (RealTime us) = fromIntegral (us `Prelude.div` fromIntegral time_factor)
