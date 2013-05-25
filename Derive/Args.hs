-- | Extract things from the PassedArgs data structure.
module Derive.Args where
import Util.Control
import qualified Util.Seq as Seq
import qualified Ui.Event as Event
import qualified Derive.Derive as Derive
import Derive.Derive (PassedArgs, CallInfo)
import Types


info :: PassedArgs a -> CallInfo a
info = Derive.passed_info

event :: PassedArgs a -> Event.Event
event = Derive.info_event . info

-- | Get the previous derived val.  This is used by control derivers so they
-- can interpolate from the previous sample.
prev_val :: PassedArgs a -> Maybe (RealTime, a)
prev_val = Derive.info_prev_val . info

is_title_call :: PassedArgs a -> Bool
is_title_call args = range args == Derive.info_track_range (info args)

-- * event timing

start :: PassedArgs a -> ScoreTime
start = Event.start . event

duration :: PassedArgs a -> ScoreTime
duration = Event.duration . event

real_start :: PassedArgs a -> Derive.Deriver RealTime
real_start = Derive.real . start

end :: PassedArgs a -> ScoreTime
end = Event.end . event

real_end :: PassedArgs a -> Derive.Deriver RealTime
real_end = Derive.real . end

-- | The start of the next event, or the end of the block if there is no next
-- event.
--
-- Used by calls to determine their extent, especially control calls, which
-- have no explicit duration.
next :: PassedArgs a -> ScoreTime
next = Derive.info_event_end . info

-- | End of the next event, or the end of the block if there is no next event.
next_end :: PassedArgs a -> ScoreTime
next_end args = maybe (next args) Event.end (Seq.head (next_events args))

-- | Get the start of the next event, if there is one.
--
-- This is similar to 'next', except that it will be Nothing at the end of
-- the block.
next_start :: PassedArgs a -> Maybe ScoreTime
next_start = fmap Event.start . Seq.head . next_events

prev_start :: PassedArgs a -> Maybe ScoreTime
prev_start = fmap Event.start . Seq.head . prev_events

prev_events, next_events :: PassedArgs a -> [Event.Event]
next_events = Derive.info_next_events . info
prev_events = Derive.info_prev_events . info

-- ** range

-- | Range of the called event.  Note that range is the minimum to maximum,
-- which is not the same as the start and end if the event has negative
-- duration.
range :: PassedArgs a -> (ScoreTime, ScoreTime)
range = Event.range . event

-- | Like 'range', but if the duration is 0, then the end is 'next' event.
range_or_next :: PassedArgs a -> (ScoreTime, ScoreTime)
range_or_next args
    | start == end = (start, next args)
    | otherwise = (start, end)
    where (start, end) = range args

real_range :: PassedArgs a -> Derive.Deriver (RealTime, RealTime)
real_range args = (,) <$> Derive.real start <*> Derive.real end
    where (start, end) = range args

-- | Start and duration of the event.  This is probably the right thing for
-- calls that generate a note since it will give a negative duration when
-- appropriate.
extent :: PassedArgs a -> (ScoreTime, ScoreTime)
extent = (\e -> (Event.start e, Event.duration e)) . event

real_extent :: PassedArgs a -> Derive.Deriver (RealTime, RealTime)
real_extent args = do
    let e = event args
    start <- Derive.real (Event.start e)
    end <- Derive.real (Event.end e)
    return (start, end - start)

-- | Event range as it appears on the track, regardless of slicing.
range_on_track :: PassedArgs a -> (ScoreTime, ScoreTime)
range_on_track args = (track_start + start, track_start + end)
    where
    (start, end) = range args
    track_start = fst (Derive.info_track_range (info args))

-- | This normalizes a deriver to start at 0 and have a duration of 1, provided
-- that the deriver is placed at the start and dur of the given args.  This is
-- the case if the deriver is a transformer arg, so this is useful for
-- a transformer to manipulate its argument.
normalized :: PassedArgs a -> Derive.Deriver b -> Derive.Deriver b
normalized args = Derive.d_place (- (start / dur)) (1 / dur)
    where
    (start, dur_) = extent args
    dur = if dur_ == 0 then 1 else dur_
