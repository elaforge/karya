module Perform.Signal where
import qualified Data.Map as Map

import qualified Util.Data

import qualified Perform.Timestamp as Timestamp


import Debug.Trace


-- * construction

-- TODO this will probably become TrackPos since derivation also works with
-- signals

data Signal = Signal {
    signal_map :: Map.Map Timestamp.Timestamp (Method, Val)
    } deriving (Show, Eq)

signal :: [(Timestamp.Timestamp, Method, Val)] -> Signal
signal = Signal . Map.fromList . map (\(ts, meth, val) -> (ts, (meth, val)))

type Val = Double

data Method =
    Set
    -- | Given ((meth, val1), val2), approach val1 with the given method and
    -- then jump to val2.
    -- TODO this is kind of ugly and not too well tested.  Remove it later if
    -- it turns out to not be useful.
    | Jump (Method, Val)
    | Linear
    -- | Approach the point with an exponential curve.  If the exponent is
    -- positive, the value will be pos**n.  If it's negative, it will be
    -- pos**(1/n).
    | Exp Double
    deriving (Show, Eq)


-- * sampling

-- | Generate samples between the given timestamps.  A sample at the starting
-- timestamp is always given, and Set values are always generated at their set
-- timestamp.  Interpolated samples (Linear, Exp) are generated according to
-- 'srate'.
-- TODO: supply srate as a signal
sample :: Signal -> Timestamp.Timestamp -> Timestamp.Timestamp
    -> [(Timestamp.Timestamp, Val)]
sample sig start_ts end_ts = _sample srate previous (Map.assocs post) end_ts
    where
    (pre, post) = Util.Data.split_map start_ts (signal_map sig)
    previous = if Map.null pre
        then (Timestamp.Timestamp 0, 0)
        else (\(ts, (meth, val)) -> (ts, val)) (Map.findMax pre)
    -- TODO hardcoded for the moment
    srate = Timestamp.seconds 0.1

-- TODO use DList instead of ++
_sample _ _ [] _ = []
_sample srate (prev_ts, prev_val) ((ts, (meth, val)):rest) end_ts
    | ts > end_ts = []
    -- Discontinuities in the input can make interpolate produce NaNs, and
    -- will produce extra samples, so skip them.
    | prev_ts == ts = _sample srate (ts, val) rest end_ts
    | otherwise = case meth of
        Set -> (ts, val) : _sample srate (ts, val) rest end_ts
        Jump (jmeth, jval) ->
            _sample srate (prev_ts, prev_val) [(ts, (jmeth, jval))] ts
            ++ ( _sample srate (ts, val) rest end_ts)
        _ -> let smps = sample_from srate prev_ts ts
            in zip smps
                (map (interpolate meth (prev_ts, prev_val) (ts, val)) smps)
            ++ _sample srate (ts, val) rest end_ts

sample_from srate start_ts end_ts
    | start_ts > end_ts = []
    | otherwise = start_ts : sample_from srate (start_ts + srate) end_ts

-- | Get the value of a signal at the given timestamp.
at :: Signal -> Timestamp.Timestamp -> Val
at sig ts = interpolate next_meth (prev_ts, prev_val) (next_ts, next_val) ts
    where
    (pre_, at, post) = Map.splitLookup ts (signal_map sig)
    -- Since 'interpolate' interpolates from the previous value, put an exact
    -- match in pre to ensure that I return its value.
    pre = case at of
        Nothing -> pre_
        Just v -> Map.insert ts v pre_

    (prev_ts, (_, prev_val)) = if Map.null pre
        then ((Timestamp.Timestamp 0), (Set, 0))
        else Map.findMax pre
    (next_ts, (next_meth, next_val)) = if Map.null post
        then (ts, (Set, 0))
        else Map.findMin post

-- interpolate meth prev next ts =
--     trace ("\n*->" ++ show (meth, prev, next, ts) ++ "\n")
--     (interpolate' meth prev next ts)

interpolate meth (prev_ts, prev_val) (next_ts, next_val) ts
    | prev_ts == next_ts = next_val
    | otherwise = prev_val + ival
    where
    ival = case meth of
        Set -> 0
        Jump _ -> 0
        Linear -> amount * (next_val - prev_val)
        Exp n -> let exp = if n >= 0 then n else (1 / abs n)
            in amount**exp * (next_val - prev_val)
    amount = Timestamp.to_seconds (ts - prev_ts)
        / Timestamp.to_seconds (next_ts - prev_ts)
