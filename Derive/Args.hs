-- | Extract things from the PassedArgs data structure.
module Derive.Args where
import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Derive.Derive as Derive
import Derive.Derive (PassedArgs, CallInfo)
import Types


info :: PassedArgs d -> CallInfo d
info = Derive.passed_info

event :: PassedArgs derived -> Event.Event
event = Derive.info_event . info

-- | Get the previous derived val.  This is used by control derivers so they
-- can interpolate from the previous sample.
prev_val :: PassedArgs d -> Maybe (RealTime, Derive.Elem d)
prev_val = Derive.info_prev_val . info


-- * event timing

-- | The start of the next event, or the end of the block if there is no next
-- event.
end :: PassedArgs d -> ScoreTime
end = Derive.info_event_end . info

prev_start :: PassedArgs d -> Maybe ScoreTime
prev_start = fmap Event.start . Seq.head . Derive.info_prev_events . info

start :: PassedArgs d -> ScoreTime
start = Event.start . event

real_start :: PassedArgs d -> Derive.Deriver RealTime
real_start = Derive.real . start

-- | Get the start of the next event, if there is one.  Used by calls to
-- determine their extent, especially control calls, which have no explicit
-- duration.
--
-- If there is a value it should be the same as 'end', so this is for calls
-- that care if there really is a next event.
next_start :: PassedArgs d -> Maybe ScoreTime
next_start = fmap Event.start . Seq.head . Derive.info_next_events . info

prev_events, next_events :: PassedArgs d -> [Event.Event]
next_events = Derive.info_next_events . info
prev_events = Derive.info_prev_events . info

-- ** range

-- | Range of the called event.  Note that range is the minimum to maximum,
-- which is not the same as the start and end if the event has negative
-- duration.
range :: PassedArgs d -> (ScoreTime, ScoreTime)
range = Event.range . event

-- | Like 'range', but (start, duration) instead of (start, end).
extent :: PassedArgs d -> (ScoreTime, ScoreTime)
extent = (\e -> (Event.start e, Event.duration e)) . event

real_range :: PassedArgs d -> Derive.Deriver (RealTime, RealTime)
real_range args = (,) <$> Derive.real start <*> Derive.real end
    where (start, end) = range args

-- | Like 'range', except the range is to the beginning of the next
-- event.  Suitable for control calls, which tend to have 0 duration.
range_to_next :: PassedArgs d -> (ScoreTime, ScoreTime)
range_to_next args = (start args, end args)

-- | Event range as it appears on the track, regardless of slicing.
range_on_track :: PassedArgs d -> (ScoreTime, ScoreTime)
range_on_track args = (track_start + start, track_start + end)
    where
    (start, end) = range args
    track_start = fst (Derive.info_track_range (info args))
