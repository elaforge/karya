module Derive.Call.Post.Reverse where
import Util.Control
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("reverse", c_reverse)
    ]

-- * reverse

c_reverse :: Derive.NoteCall
c_reverse = Derive.transformer "reverse" "Reverse the events." $
    CallSig.call0t $ \args deriver -> do
        start <- Args.real_start args
        (events, logs) <- LEvent.partition <$> deriver
        return $ map LEvent.Log logs ++ map LEvent.Event
            (reverse_tracks start events)

reverse_tracks :: RealTime -> [Score.Event] -> [Score.Event]
reverse_tracks start events = Seq.merge_lists Score.event_start
    [ reverse_events (start + (end - Score.event_end last)) events
    | events@(last : _) <- by_track
    ]
    where
    by_track = map (reverse . snd) $ partition_tracks events
    -- This will be the new 0.
    end = fromMaybe 0 $ Seq.maximum
        [Score.event_end last | last : _ <- by_track]

-- | Partition up events by the track they're in.
partition_tracks :: [Score.Event] -> [(TrackId, [Score.Event])]
partition_tracks = strip . Seq.keyed_group_on track_of
    where strip xs = [(track_id, events) | (Just track_id, events) <- xs]

track_of :: Score.Event -> Maybe TrackId
track_of = Seq.head . mapMaybe Stack.track_of . Stack.innermost
    . Score.event_stack

reverse_events :: RealTime -> [Score.Event] -> [Score.Event]
reverse_events start = rev start
    where
    rev _ [] = []
    rev at [cur] = [move at cur]
    rev at (cur : rest@(next : _)) = move at cur
        : rev (at + Score.event_duration cur
            + (Score.event_start cur - Score.event_end next)) rest
    move at = Score.move (const at)
