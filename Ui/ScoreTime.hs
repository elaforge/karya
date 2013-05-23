{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ui.ScoreTime (
    ScoreTime, TrackTime, to_double, to_cdouble, double, suffix, eta, eq
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Digest.CRC32 as CRC32
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.ApproxEq as ApproxEq
import Util.Crc32Instances ()
import Util.Control
import qualified Util.ForeignC as C
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import qualified Ui.Util as Util


-- | Score time is the abstract unit of time, and its mapping to real time
-- is dependent on the score context.  ScoreTime units can be negative, but
-- blocks only display events at >=0 ScoreTime.
newtype ScoreTime = ScoreTime Double deriving
    ( DeepSeq.NFData, Num, Fractional, Real
    , RealFrac, Eq, Ord, Serialize.Serialize, CRC32.CRC32
    , ApproxEq.ApproxEq
    )

-- | This is also ScoreTime, but it's relative to the beginning of the track.
-- I.e., UI events are all in track time, but when they get shifted and
-- stretched as by note slicing they're no longer in TrackTime, but not yet in
-- RealTime.
--
-- I'd like to make a type-level distinction because it's easy to get confused
-- about whether a time has or hasn't been transformed, but when I tried it
-- seemed like a big hassle since I'd really like for TrackTime to be a subtype
-- of ScoreTime.  I could do it with a phantom type, but it would change about
-- a million type declarations.  And since Events start in TrackTime but are
-- then ScoreTime if transformed, they would also need a type parameter, along
-- with probably a few other basic data types.
--
-- Unless I work up the courage to do that someday, the least I can do is
-- document the difference with a type synonym.
type TrackTime = ScoreTime

-- I could derive Storable, but technically speaking Double is not necessarily
-- the same as CDouble.
instance C.CStorable ScoreTime where
    sizeOf _ = C.sizeOf (undefined :: C.CDouble)
    alignment _ = C.alignment (undefined :: C.CDouble)
    poke p (ScoreTime d) = C.poke (C.castPtr p) (Util.c_double d)
    peek p = ScoreTime . Util.hs_double <$> C.peek (C.castPtr p)

instance Show ScoreTime where
    show (ScoreTime n) = show n ++ [suffix]

instance Read.Read ScoreTime where
    readPrec = do
        n <- Read.readPrec
        Read.lift ReadP.skipSpaces
        't' <- Read.get
        return (ScoreTime n)

instance Pretty.Pretty ScoreTime where
    pretty (ScoreTime p) = Pretty.show_float 3 p ++ [suffix]

to_double :: ScoreTime -> Double
to_double (ScoreTime p) = p

to_cdouble :: ScoreTime -> C.CDouble
to_cdouble = C.CDouble . to_double

double :: Double -> ScoreTime
double = ScoreTime

-- | t is for time, since RealTime uses s for seconds
suffix :: Char
suffix = 't'

-- | Eta for comparison.  ScoreTimes are all relative, but there's no reason to
-- use such tiny ones.
eta :: ScoreTime
eta = 0.00000000000004

-- | ScoreTimes are imprecise, so compare them with this instead of (==).
eq :: ScoreTime -> ScoreTime -> Bool
eq = ApproxEq.approx_eq (to_double eta)
