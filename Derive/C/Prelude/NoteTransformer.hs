-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on @Derive.Generator Derive.Note@.
module Derive.C.Prelude.NoteTransformer (library) where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Lists as Lists

import qualified Derive.Args as Args
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Eval as Eval
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime

import           Global
import           Types


library :: Library.Library
library = mconcat
    [ Library.generators
        [ ("sequence", cSequence)
        , ("sequence-rt", cSequenceRealtime)
        , ("parallel", cParallel)
        ]
    , Library.transformers
        [ ("clip", cClip)
        , ("Clip", cClipStart)
        , ("debug", cDebug)
        , ("loop", cLoop)
        , ("multiple", cMultiple)
        , ("tile", cTile)
        , ("repeat", cRepeat)
        ]
    ]

-- * generators

-- This isn't a NoteTransformer, but it seems like it belongs here.  What is
-- a better name for the module?
cSequence :: Derive.Generator Derive.Note
cSequence = Derive.withScoreDuration scoreDuration $
    Derive.generator Module.prelude "sequence" mempty
    "Run the given calls in sequence. If they each have have an intrinsic\
    \ CallDuration (usually this means block calls), they will get that amount\
    \ of time, at least proportial to the duration of the event. Otherwise,\
    \ if none of them do, they are given equal duration. If some do and some\
    \ don't, you probably get confusing results."
    $ Sig.call callsArg $ \calls args -> do
        let derivers = callsToDerivers args calls
        durs <- mapM getScoreDuration derivers
        sequenceDerivers (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    callsArg = Sig.many1 "call" "Generator calls."
    scoreDuration args = do
        calls <- Sig.parseOrThrow callsArg args
        durs <- mapM getScoreDuration (callsToDerivers args calls)
        pure $ Derive.CallDuration (Num.sum durs)

sequenceDerivers :: ScoreTime -> ScoreTime -> [Derive.NoteDeriver]
    -> [ScoreTime] -> Derive.NoteDeriver
sequenceDerivers start eventDur derivers unstretchedDurs = mconcat
    [ Derive.place start dur d
    | (start, dur, d) <- zip3 (scanl (+) start durs) durs derivers
    ]
    where
    durs = map (*stretch) unstretchedDurs
    stretch = if totalDur == 0 then 1 else eventDur / totalDur
        where totalDur = Num.sum unstretchedDurs

cSequenceRealtime :: Derive.Generator Derive.Note
cSequenceRealtime = Derive.withScoreDuration scoreDuration $
    Derive.generator Module.prelude "sequence-rt" mempty
    "Run the given block calls in sequence. Each call gets its natural\
    \ real time duration. Unlike `sequence`, each block gets its natural\
    \ RealTime duration, rather than being normalized to 1 and then expanded\
    \ to its ScoreTime duration. TODO I can't get the RealTime duration without\
    \ deriving, at which point it's too late to stretch, so the event duration\
    \ has no effect." -- TODO that last sentence is now out of date since
    -- I have Derive.get_real_duration, right?
    $ Sig.call callsArg $ \calls args -> do
        let derivers = callsToDerivers args calls
        durs <- mapM getRealDuration derivers
        sequenceDeriversRealtime (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    callsArg = Sig.many1 "call" "Generator calls."
    scoreDuration args = do
        calls <- Sig.parseOrThrow callsArg args
        durs <- mapM getRealDuration (callsToDerivers args calls)
        end <- Call.scoreDuration (Args.start args) (Num.sum durs)
        pure $ Derive.CallDuration $ end - Args.start args

sequenceDeriversRealtime :: ScoreTime -> ScoreTime -> [Derive.NoteDeriver]
    -> [RealTime] -> Derive.NoteDeriver
sequenceDeriversRealtime start eventDur derivers rDurs = do
    rStart <- Derive.real start
    starts <- mapM Derive.score $ scanl (+) rStart rDurs
    let unstretchedDurs = zipWith (-) (drop 1 starts) starts
    let totalDur = Num.sum unstretchedDurs
        stretch = if totalDur == 0 then 1 else eventDur / totalDur
    let durs = map (*stretch) unstretchedDurs
    mconcat
        [ Derive.place start dur d
        | (start, dur, d) <- zip3 (scanl (+) start durs) durs derivers
        ]

cParallel :: Derive.Generator Derive.Note
cParallel = Derive.withScoreDuration scoreDuration $ Derive.generator
    Module.prelude "parallel" mempty "Run the given calls in parallel."
    $ Sig.call callsArg $ \calls args -> do
        let derivers = callsToDerivers args calls
        durs <- mapM getScoreDuration derivers
        parallelDerivers (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    callsArg = Sig.many1 "call" "Generator calls."
    scoreDuration args = do
        calls <- Sig.parseOrThrow callsArg args
        durs <- mapM getScoreDuration (callsToDerivers args calls)
        pure $ Derive.CallDuration $ fromMaybe 0 (Lists.maximum durs)

parallelDerivers :: ScoreTime -> ScoreTime -> [Derive.NoteDeriver]
    -> [ScoreTime] -> Derive.NoteDeriver
parallelDerivers start eventDur derivers durs =
    Derive.stretch stretch $ mconcat
        [ Derive.place start dur d
        | (dur, d) <- zip durs derivers
        ]
    where
    stretch = if callDur == 0 then 1 else eventDur / callDur
    callDur = fromMaybe 0 (Lists.maximum durs)

callsToDerivers :: Derive.CallableExpr d => Derive.PassedArgs d
    -> NonEmpty DeriveT.Quoted
    -> [(DeriveT.Quoted, Derive.Deriver (Stream.Stream d))]
callsToDerivers args calls = zip (NonEmpty.toList calls)
    (map (Eval.evalQuotedNormalized (Args.context args))
        (NonEmpty.toList calls))

getScoreDuration :: (DeriveT.Quoted, Derive.Deriver a)
    -> Derive.Deriver ScoreTime
getScoreDuration (quoted, d) = Derive.getScoreDuration d >>= \case
    Left err -> Derive.throw $ "get score dur: " <> pretty err
    Right Derive.Unknown -> Derive.throw $ "unknown score duration for "
        <> ShowVal.showVal quoted
    Right (Derive.CallDuration dur) -> pure dur

getRealDuration :: (DeriveT.Quoted, Derive.Deriver a)
    -> Derive.Deriver RealTime
getRealDuration (quoted, d) = Derive.getRealDuration d >>= \case
    Left err -> Derive.throw $ "get real dur: " <> pretty err
    Right Derive.Unknown -> Derive.throw $ "unknown real duration for "
        <> ShowVal.showVal quoted
    Right (Derive.CallDuration dur) -> pure dur


-- * transformers

cMultiple :: Derive.Transformer Derive.Note
cMultiple = Derive.transformer Module.prelude "multiple" mempty
    "Derive the transformed score under different transformers."
    $ Sig.callt (Sig.many1 "transformer" "Derive under each transformer.")
    $ \transformers args deriver ->
        mconcatMap (apply (Args.context args) deriver)
            (NonEmpty.toList transformers)
    where
    apply ctx deriver trans = Eval.evalQuotedTransformers ctx trans deriver

cDebug :: Derive.Transformer Derive.Note
cDebug = Derive.transformer Module.prelude "debug" mempty
    "Save the events at this point in a special log msg. This is useful to\
    \ inspect events at a certain point in a pipeline. You can extract them\
    \ later by looking at the `Log.msg_data` of a log msg with the given tag."
    $ Sig.callt (Sig.required "tag" "Log msg has this text.")
    $ \tag _ deriver -> do
        events <- deriver
        Log.write $ Log.withDyn tag (Stream.eventsOf events) $
            Log.msg Log.Debug Nothing "debug call"
        pure events

-- ** clip

cClip :: Derive.Transformer Derive.Note
cClip = Derive.transformer Module.prelude "clip" mempty
    "Unstretch the deriver to its natural duration, and clip events that lie\
    \ beyond the end of the event. Notes that overlap the end of the event will\
    \ be cut short.\
    \\nThis is only useful with calls that have a natural duration apart from\
    \ whatever their calling event's duration, e.g. block calls."
    $ Sig.call0t $ \args -> unstretchArgs args $ \_dur deriver -> do
        end <- Derive.real $ snd (Args.range args)
        trimEvents Nothing (Just end) $
            Derive.at (Args.start args) deriver

cClipStart :: Derive.Transformer Derive.Note
cClipStart = Derive.transformer Module.prelude "Clip" mempty
    "Like `clip`, but align the named block to the end of the event instead\
    \ of the beginning. Events that then lie before the start are clipped."
    $ Sig.call0t $ \args -> unstretchArgs args $ \dur deriver -> do
        start <- Derive.real $ fst (Args.range args)
        trimEvents (Just start) Nothing $
            Derive.at (Args.end args - dur) deriver

-- ** loop

cLoop :: Derive.Transformer Derive.Note
cLoop = Derive.transformer Module.prelude "loop" mempty
    "This is similar to `clip`, but when the called note runs out, it is\
    \ repeated."
    $ Sig.call0t $ \args -> unstretchArgs args $ \dur deriver -> do
        let (start, end) = Args.range args
        let repeats = ceiling $ (end - start) / dur
            starts = take repeats $ Lists.range_ start dur
        realEnd <- Derive.real end
        trimEvents Nothing (Just realEnd) $ repeatAt starts 1 deriver

cTile :: Derive.Transformer Derive.Note
cTile = Derive.transformer Module.prelude "tile" mempty
    "This is like `loop`, but it can start the looped call in its middle\
    \ instead of starting from 0. The effect is as if the loop is tiled from\
    \ the beginning of the called block, and is only \"let through\" during\
    \ the `tile` call. This is useful for patterns that are tied to the meter,\
    \ but may be interrupted at arbitrary times, e.g. sarvalaghu patterns."
    $ Sig.call0t $ \args -> unstretchArgs args $ \dur deriver -> do
        let (start, end) = Args.range args
        let subStart = fromIntegral (floor (start / dur)) * dur
        let repeats = ceiling $ (end - subStart) / dur
            starts = take repeats $ Lists.range_ subStart dur
        (realStart, realEnd) <- Args.realRange args
        trimEvents (Just realStart) (Just realEnd) $
            repeatAt starts 1 deriver

cRepeat :: Derive.Transformer Derive.Note
cRepeat = Derive.transformer Module.prelude "repeat" mempty
    "Repeat the score multiple times, fitted within the note duration."
    $ Sig.callt (Sig.required "times" "Repeat this many times.")
    $ \(Typecheck.Positive times) args deriver -> do
        let dur = Args.duration args / fromIntegral times
        let deriver0 = Derive.at (- Args.start args) deriver
        let starts = take times (Lists.range_ (Args.start args) dur)
        repeatAt starts (1 / fromIntegral times) deriver0

-- | Repeat the deriver at the given start times.
repeatAt :: [ScoreTime] -> ScoreTime -> Derive.NoteDeriver
    -> Derive.NoteDeriver
repeatAt starts dur deriver =
    mconcat [Derive.place start dur deriver | start <- starts]

trimEvents :: Maybe RealTime -> Maybe RealTime
    -> Derive.NoteDeriver -> Derive.NoteDeriver
trimEvents start end deriver = do
    events <- Internal.trimTrackWarps start end deriver
    pure $ maybe id trimEnd end $
        maybe id (Stream.dropWhile . eventBefore) start events
    where
    trimEnd e = fmap (clip e) . Stream.takeWhile (eventBefore e)
    clip end event
        | Score.eventEnd event <= end = event
        | otherwise =
            Score.setDuration (max 0 (end - Score.eventStart event)) event

-- * util

unstretchArgs :: Derive.PassedArgs x
    -> (ScoreTime -> Derive.Deriver a -> Derive.Deriver a)
    -> Derive.Deriver a -> Derive.Deriver a
unstretchArgs args = unstretch (Args.start args) (Args.duration args)

-- | Put the deriver at 0t and in its \"natural\" time.  This is only different
-- from its event's time if the deriver has its own duration as per
-- 'Derive.get_score_duration'.
--
-- The generator will do @Derive.place start (event_dur/dur)@, so I have to
-- undo that.
unstretch :: ScoreTime -> ScoreTime
    -> (ScoreTime -> Derive.Deriver a -> Derive.Deriver a)
    -> Derive.Deriver a -> Derive.Deriver a
unstretch start eventDur process deriver = do
    dur <- Derive.getScoreDuration deriver >>= \case
        Left _ -> pure eventDur
        Right Derive.Unknown -> pure eventDur
        Right (Derive.CallDuration dur) -> pure dur
    process dur $ flatten dur deriver
    where
    flatten dur = Derive.stretch (1 / (eventDur/dur)) . Derive.at (-start)

-- | Consistent with half-open ranges, block calls try to include events lining
-- up with the start, and exclude ones lining up with the end.  Except negative
-- notes, which is usual, the same thing but backwards.
eventBefore :: RealTime -> Score.Event -> Bool
eventBefore t e
    | Score.eventDuration e >= 0 = Score.eventStart e < t - RealTime.eta
    | otherwise = Score.eventStart e <= t + RealTime.eta
