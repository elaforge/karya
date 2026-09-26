-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-proc calls that impose a new kind of articulation.
module Derive.C.Post.Rearticulate (library) where
import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.LEvent as LEvent
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("slur-n", cSlurN)
    , ("slur-dur", cSlurDur)
    ]

cSlurN :: Derive.Transformer Derive.Note
cSlurN = Derive.transformer Module.prelude "slur-n" Tags.postproc
    "Merge groups of notes into one note, where the pitch is taken from each\
    \ merged note. The groups are of a fixed size."
    $ Sig.callt (Sig.required "group" "How many notes in a group.")
    $ \group _args deriver -> slurN group <$> deriver

slurN :: Int -> Stream.Stream Score.Event -> Stream.Stream Score.Event
slurN group = Stream.fromSortedList . go . Stream.toList
    where
    go events = case first LEvent.partition $ splitEvents group events of
        (([], logs), _) -> map LEvent.Log logs
        ((e : es, logs), post) -> map LEvent.Log logs
            ++ LEvent.Event (slur e es) : go post

-- | Merge pitch and controls from the given events.
--
-- Previously I used a curve and time to merge more gradually.  I can add it
-- back if it's useful.
slur :: Score.Event -> [Score.Event] -> Score.Event
slur event events = event
    { Score.eventDuration = dur
    , Score.eventPitch = mergePitch (Score.eventPitch event)
        [(Score.eventStart e, Score.eventPitch e) | e <- events]
    , Score.eventEnviron = Env.fromControls (mergeControls event events)
        <> Score.eventEnviron event
    }
    where
    dur = Score.eventEnd (fromMaybe event (Lists.last events))
        - Score.eventStart event

mergePitch :: PSignal.PSignal -> [(RealTime, PSignal.PSignal)]
    -> PSignal.PSignal
mergePitch sig sigs =
    mconcat $ sig : [PSignal.clipBefore start sig | (start, sig) <- sigs]

mergeControls :: Score.Event -> [Score.Event] -> ScoreT.ControlMap
mergeControls event events = Maps.mconcat $ clip event : map clip events
    where
    clip event = fmap (Signal.clipBefore (Score.eventStart event)) <$>
        Score.eventControls event

-- | 'splitAt' for LEvents.
splitEvents :: Int -> [LEvent.LEvent a]
    -> ([LEvent.LEvent a], [LEvent.LEvent a])
splitEvents _ [] = ([], [])
splitEvents n (e:es)
    | n <= 0 = ([], e : es)
    | otherwise = (e : pre, post)
    where (pre, post) = splitEvents (if LEvent.isEvent e then n - 1 else n) es

cSlurDur :: Derive.Transformer Derive.Note
cSlurDur = Derive.transformer Module.prelude "slur-dur" Tags.postproc
    "Merge groups of notes into one note, where the pitch is taken from each\
    \ merged note. The groups are by duration."
    $ Sig.callt ((,)
    <$> (Typecheck._score <$> Sig.required "dur" "How long each group is.")
    <*> (Typecheck._score <$> Sig.defaulted "offset" (Typecheck.score 0)
        "Groups start at this time.")
    ) $ \(dur, offset) args deriver -> do
        start <- Args.realStart args
        dur <- Call.realDuration start dur
        offset <- Call.realDuration start offset
        slurDur dur offset <$> deriver

slurDur :: RealTime -> RealTime -> Stream.Stream Score.Event
    -> Stream.Stream Score.Event
slurDur dur offset stream =
    Stream.mergeLogs logs $ Stream.fromSortedEvents $
        map apply (groupDur dur offset events)
    where
    apply (e :| es) = slur e es
    (events, logs) = Stream.partition stream

groupDur :: RealTime -> RealTime -> [Score.Event] -> [NonEmpty Score.Event]
groupDur dur offset = Lists.groupStable groupOf
    where
    groupOf = floor . RealTime.toSeconds . (/dur) . subtract offset
        . Score.eventStart
