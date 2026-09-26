-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Reverse (library) where
import qualified Util.Lists as Lists
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("reverse", cReverse)
    ]

-- * reverse

cReverse :: Derive.Transformer Derive.Note
cReverse = Derive.transformer Module.prelude "reverse" Tags.postproc
    "Reverse the events." $
    Sig.call0t $ \args deriver -> do
        start <- Args.realStart args
        (events, logs) <- Stream.partition <$> deriver
        pure $ Stream.mergeLogs logs $
            Stream.fromSortedEvents (reverseTracks start events)

reverseTracks :: RealTime -> [Score.Event] -> [Score.Event]
reverseTracks start events = Lists.mergeLists Score.eventStart
    [ reverseEvents (start + (end - Score.eventEnd last)) events
    | events@(last : _) <- byTrack
    ]
    where
    byTrack = map (reverse . snd) $ partitionTracks events
    -- This will be the new 0.
    end = fromMaybe 0 $ Lists.maximum
        [Score.eventEnd last | last : _ <- byTrack]

-- | Partition up events by the track they're in.
partitionTracks :: [Score.Event] -> [(TrackId, [Score.Event])]
partitionTracks = strip . Lists.keyedGroupSort trackOf
    where strip xs = [(trackId, events) | (Just trackId, events) <- xs]

trackOf :: Score.Event -> Maybe TrackId
trackOf = Lists.head . mapMaybe Stack.trackOf . Stack.innermost
    . Score.eventStack

reverseEvents :: RealTime -> [Score.Event] -> [Score.Event]
reverseEvents at events = case events of
    [] -> []
    [cur] -> [move at cur]
    cur : rest@(next : _) -> move at cur
        : reverseEvents (at + Score.eventDuration cur
            + (Score.eventStart cur - Score.eventEnd next)) rest
    where
    move at = Score.move (const at)
