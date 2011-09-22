{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ui.ScoreTime (ScoreTime, to_double, double) where
import qualified Control.DeepSeq as DeepSeq
import qualified Foreign
import qualified Text.Read as Read

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize


-- | Score time is the abstract unit of time, and its mapping to real time
-- is dependent on the score context.  ScoreTime units can be negative, but
-- blocks only display events at >=0 ScoreTime.
newtype ScoreTime = ScoreTime Double
    deriving (DeepSeq.NFData, Foreign.Storable, Num, Fractional, Real, Eq, Ord)

instance Serialize.Serialize ScoreTime where
    put (ScoreTime a) = Serialize.put a
    get = fmap ScoreTime Serialize.get

-- t is for time, since RealTime uses s for seconds
instance Show ScoreTime where
    show (ScoreTime n) = show n ++ "t"

instance Read.Read ScoreTime where
    readPrec = do
        n <- Read.readPrec
        't' <- Read.get
        return (ScoreTime n)

instance Pretty.Pretty ScoreTime where
    pretty (ScoreTime p) = Pretty.show_float (Just 3) p ++ "t"

to_double :: ScoreTime -> Double
to_double (ScoreTime p) = p

double :: Double -> ScoreTime
double = ScoreTime
