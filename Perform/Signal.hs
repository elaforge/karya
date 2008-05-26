module Perform.Signal where
import qualified Control.Arrow as Arrow
import qualified Data.Map as Map

import qualified Util.Data

import Ui.Types
import qualified Perform.Timestamp as Timestamp

-- import Debug.Trace


-- * construction

-- TODO this will probably become TrackPos since derivation also works with
-- signals

data Signal = Signal {
    signal_map :: Map.Map TrackPos (Method, Val)
    } deriving (Show, Eq)

signal :: [(TrackPos, Method, Val)] -> Signal
signal = Signal . Map.fromList . map (\(pos, meth, val) -> (pos, (meth, val)))

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
-- TODO filter dups
sample :: Signal -> TrackPos -> TrackPos -> [(TrackPos, Val)]
sample sig start_pos end_pos = _sample srate previous (Map.assocs post) end_pos
    where
    (pre, post) = Util.Data.split_map start_pos (signal_map sig)
    previous = if Map.null pre
        then (TrackPos 0, 0)
        else (\(pos, (_meth, val)) -> (pos, val)) (Map.findMax pre)
    -- TODO hardcoded for the moment, this should be 0.1 second at the
    -- hardcoded pos->ts conversion.
    srate = TrackPos 100

sample_timestamp :: Signal -> Timestamp.Timestamp -> Timestamp.Timestamp
    -> [(Timestamp.Timestamp, Val)]
sample_timestamp sig start_ts end_ts =
    map (Arrow.first Timestamp.from_track_pos) $
        sample sig (Timestamp.to_track_pos start_ts)
            (Timestamp.to_track_pos end_ts)

-- TODO use DList instead of ++
_sample _ _ [] _ = []
_sample srate (prev_pos, prev_val) ((pos, (meth, val)):rest) end_pos
    | pos > end_pos = []
    -- Discontinuities in the input can make interpolate produce NaNs, and
    -- will produce extra samples, so skip them.
    | prev_pos == pos = _sample srate (pos, val) rest end_pos
    | otherwise = case meth of
        Set -> (pos, val) : _sample srate (pos, val) rest end_pos
        Jump (jmeth, jval) ->
            _sample srate (prev_pos, prev_val) [(pos, (jmeth, jval))] pos
            ++ ( _sample srate (pos, val) rest end_pos)
        _ -> let smps = sample_from srate prev_pos pos
            in zip smps
                (map (interpolate meth (prev_pos, prev_val) (pos, val)) smps)
            ++ _sample srate (pos, val) rest end_pos

sample_from srate start_pos end_pos
    | start_pos > end_pos = []
    | otherwise = start_pos : sample_from srate (start_pos + srate) end_pos

-- | Get the value of a signal at the given timestamp.
at :: Signal -> TrackPos -> Val
at sig pos = interpolate next_meth (prev_pos, prev_val) (next_pos, next_val) pos
    where
    (pre_, at, post) = Map.splitLookup pos (signal_map sig)
    -- Since 'interpolate' interpolates from the previous value, put an exact
    -- match in pre to ensure that I return its value.
    pre = case at of
        Nothing -> pre_
        Just v -> Map.insert pos v pre_

    (prev_pos, (_, prev_val)) = if Map.null pre
        then ((TrackPos 0), (Set, 0))
        else Map.findMax pre
    (next_pos, (next_meth, next_val)) = if Map.null post
        then (pos, (Set, 0))
        else Map.findMin post

timestamp_at :: Signal -> Timestamp.Timestamp -> Val
timestamp_at sig ts = at sig (Timestamp.to_track_pos ts)

-- interpolate meth prev next pos =
--     trace ("\n*->" ++ show (meth, prev, next, pos) ++ "\n")
--     (interpolate' meth prev next pos)

interpolate :: Method -> (TrackPos, Val) -> (TrackPos, Val) -> TrackPos -> Val
interpolate meth (prev_pos, prev_val) (next_pos, next_val) pos
    | prev_pos == next_pos = next_val
    | otherwise = prev_val + ival
    where
    ival = case meth of
        Set -> 0
        Jump _ -> 0
        Linear -> amount * (next_val - prev_val)
        Exp n -> let exp = if n >= 0 then n else (1 / abs n)
            in amount**exp * (next_val - prev_val)
    amount = fromIntegral (pos - prev_pos) / fromIntegral (next_pos - prev_pos)
