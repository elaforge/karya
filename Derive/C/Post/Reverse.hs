-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Reverse where
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("reverse", c_reverse)
    ]

-- * reverse

c_reverse :: Derive.Transformer Derive.Note
c_reverse = Derive.transformer Module.prelude "reverse" Tags.postproc
    "Reverse the events." $
    Sig.call0t $ \args deriver -> do
        start <- Args.real_start args
        (events, logs) <- Stream.partition <$> deriver
        return $ Stream.merge_logs logs $
            Stream.from_sorted_events (reverse_tracks start events)

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
partition_tracks = strip . Seq.keyed_group_sort track_of
    where strip xs = [(track_id, events) | (Just track_id, events) <- xs]

track_of :: Score.Event -> Maybe TrackId
track_of = Seq.head . mapMaybe Stack.track_of . Stack.innermost
    . Score.event_stack

reverse_events :: RealTime -> [Score.Event] -> [Score.Event]
reverse_events at events = case events of
    [] -> []
    [cur] -> [move at cur]
    cur : rest@(next : _) -> move at cur
        : reverse_events (at + Score.event_duration cur
            + (Score.event_start cur - Score.event_end next)) rest
    where
    move at = Score.move (const at)
