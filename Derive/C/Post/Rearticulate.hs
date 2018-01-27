-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-proc calls that impose a new kind of articulation.
module Derive.C.Post.Rearticulate (library) where
import qualified Util.Map
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


library :: Library.Library
library = Library.transformers
    [ ("slur-n", c_slur_n)
    , ("slur-dur", c_slur_dur)
    ]

c_slur_n :: Derive.Transformer Derive.Note
c_slur_n = Derive.transformer Module.prelude "slur-n" Tags.postproc
    "Merge groups of notes into one note, where the pitch is taken from each\
    \ merged note. The groups are of a fixed size."
    $ Sig.callt (Sig.required "group" "How many notes in a group.")
    $ \group _args deriver -> slur_n group <$> deriver

slur_n :: Int -> Stream.Stream Score.Event -> Stream.Stream Score.Event
slur_n group = Stream.from_sorted_list . go . Stream.to_list
    where
    go events = case first LEvent.partition $ split_events group events of
        (([], logs), _) -> map LEvent.Log logs
        ((e : es, logs), post) -> map LEvent.Log logs
            ++ LEvent.Event (slur e es) : go post

-- | Merge pitch and controls from the given events.
--
-- Previously I used a curve and time to merge more gradually.  I can add it
-- back if it's useful.
slur :: Score.Event -> [Score.Event] -> Score.Event
slur event events = event
    { Score.event_duration = dur
    , Score.event_pitch = merge_pitch (Score.event_pitch event)
        [(Score.event_start e, Score.event_pitch e) | e <- events]
    , Score.event_controls = merge_controls event events
    }
    where
    dur = Score.event_end (fromMaybe event (Seq.last events))
        - Score.event_start event

merge_pitch :: PSignal.PSignal -> [(RealTime, PSignal.PSignal)]
    -> PSignal.PSignal
merge_pitch sig sigs =
    mconcat $ sig : [PSignal.clip_before start sig | (start, sig) <- sigs]

merge_controls :: Score.Event -> [Score.Event] -> Score.ControlMap
merge_controls event events = Util.Map.mconcat $ clip event : map clip events
    where
    clip event = fmap (Signal.clip_before (Score.event_start event)) <$>
        Score.event_controls event

-- | 'splitAt' for LEvents.
split_events :: Int -> [LEvent.LEvent a]
    -> ([LEvent.LEvent a], [LEvent.LEvent a])
split_events _ [] = ([], [])
split_events n (e:es)
    | n <= 0 = ([], e : es)
    | otherwise = (e : pre, post)
    where (pre, post) = split_events (if LEvent.is_event e then n - 1 else n) es

c_slur_dur :: Derive.Transformer Derive.Note
c_slur_dur = Derive.transformer Module.prelude "slur-dur" Tags.postproc
    "Merge groups of notes into one note, where the pitch is taken from each\
    \ merged note. The groups are by duration."
    $ Sig.callt ((,)
    <$> (Typecheck._score <$> Sig.required "dur" "How long each group is.")
    <*> (Typecheck._score <$> Sig.defaulted "offset" (Typecheck.score 0)
        "Groups start at this time.")
    ) $ \(dur, offset) args deriver -> do
        start <- Args.real_start args
        dur <- Call.real_duration start dur
        offset <- Call.real_duration start offset
        slur_dur dur offset <$> deriver

slur_dur :: RealTime -> RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
slur_dur dur offset stream =
    Stream.merge_logs logs $ Stream.from_sorted_events $
        map apply (group_dur dur offset events)
    where
    apply (e :| es) = slur e es
    (events, logs) = Stream.partition stream

group_dur :: RealTime -> RealTime -> [Score.Event] -> [NonEmpty Score.Event]
group_dur dur offset = Seq.group_stable group_of
    where
    group_of = floor . RealTime.to_seconds . (/dur) . subtract offset
        . Score.event_start
