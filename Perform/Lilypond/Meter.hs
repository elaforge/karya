module Perform.Lilypond.Meter where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed ((!))

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Cmd.Meter as Meter
import Cmd.Meter (AbstractMeter(..))
import qualified Perform.Lilypond.Types as Types
import Perform.Lilypond.Types (Duration(..))


data TimeSignature = TimeSignature {
    -- | NonEmpty list of numerators.  E.g. [2, 3] indicates 2+3.
    time_nums :: ![Int]
    , time_denom :: !Duration
    , time_meter :: !Meter
    } deriving (Eq, Show)

time_num :: TimeSignature -> Int
time_num = sum . time_nums

rank_at :: TimeSignature -> Types.Time -> Int
rank_at sig t = v ! (fromIntegral t `mod` Vector.length v)
    where v = time_meter sig

-- | Find the time of the rank <= the given one.  Rank 0 can never be spanned,
-- so always stop at a rank 0.
find_rank :: Types.Time -> Rank -> TimeSignature -> Maybe Types.Time
find_rank start rank = fmap ((+ (start + 1)) . Types.Time)
    . Vector.findIndex (<= max 0 rank)
    . Vector.drop (fromIntegral start + 1) . time_meter

instance Pretty.Pretty TimeSignature where pretty = Types.to_lily
instance Types.ToLily TimeSignature where
    to_lily (TimeSignature nums denom _) =
        show (sum nums) ++ "/" ++ Types.to_lily denom

-- | Duration of a measure, in Time.
measure_time :: TimeSignature -> Types.Time
measure_time sig =
    Types.Time (time_num sig) * Types.dur_to_time (time_denom sig)

unparse_signature :: TimeSignature -> String
unparse_signature sig = Seq.join "+" (map show (time_nums sig))
    ++ "/" ++ Types.to_lily (time_denom sig)

parse_signature :: String -> Either String TimeSignature
parse_signature s = case Map.lookup s time_signatures of
    Nothing -> Left $ "can't parse " ++ show s ++ ", should be in "
        ++ Seq.join ", " (Map.keys time_signatures)
    Just sig -> Right sig

default_signature :: TimeSignature
Right default_signature = parse_signature "4/4"

type Rank = Int
type Meter = Vector.Vector Rank

-- D1 | D2 | D4 | D8 | D16 | D32 | D64 | D128
-- 0    1    2    3    4     5     6     7

time_signatures :: Map.Map String TimeSignature
time_signatures = Map.fromList $ Seq.key_on unparse_signature $ map make
    [ ([4], D4, [t])
    , ([2], D4, [t])
    , ([3], D4, [D [t, t, t]])
    , ([3, 2], D4, [D [t, t, t], D [t, t]])
    , ([2, 3], D4, [D [t, t], D [t, t, t]])

    , ([3, 3], D8, [D [D [t, t, t], D [t, t, t]]])
    , ([2, 2, 2], D8, [D [D [t, t], D [t, t], D [t, t]]])
    ]
    where
    make (nums, denom, meters) = time_signature nums denom meters
    t = T 1

time_signature :: [Int] -> Duration -> [AbstractMeter] -> TimeSignature
time_signature nums denom meters = TimeSignature nums denom vector
    where
    vector
        | frac /= 0 = error $ "can't fit " ++ show ranks ++ " into "
            ++ show expected ++ " by doubling"
        | otherwise = to_vector $ subdivides (replicate exp 2) meters
    (exp, frac) = properFraction $
        logBase 2 (fromIntegral expected / fromIntegral ranks)
    expected = sum nums * fromIntegral (Types.dur_to_time denom)
    ranks = sum $ map abstract_length meters
    to_vector = Vector.fromList . map fst . Meter.make_meter 1

subdivides :: [Int] -> [AbstractMeter] -> [AbstractMeter]
subdivides divs meter = foldr subdivide meter (reverse divs)

subdivide :: Int -> [AbstractMeter] -> [AbstractMeter]
subdivide n = map (Meter.subdivide n)

abstract_length :: AbstractMeter -> Int
abstract_length (D ds) = sum $ map abstract_length ds
abstract_length (T _) = 1
