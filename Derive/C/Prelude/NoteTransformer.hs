-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on @Derive.Generator Derive.Note@.
module Derive.C.Prelude.NoteTransformer (library) where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Args as Args
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
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
        [ ("sequence", c_sequence)
        , ("sequence-rt", c_sequence_realtime)
        , ("parallel", c_parallel)
        ]
    , Library.transformers
        [ ("clip", c_clip)
        , ("Clip", c_clip_start)
        , ("debug", c_debug)
        , ("loop", c_loop)
        , ("multiple", c_multiple)
        , ("tile", c_tile)
        , ("repeat", c_repeat)
        ]
    ]

-- * generators

-- This isn't a NoteTransformer, but it seems like it belongs here.  What is
-- a better name for the module?
c_sequence :: Derive.Generator Derive.Note
c_sequence = Derive.with_score_duration score_duration $
    Derive.generator Module.prelude "sequence" mempty
    "Run the given calls in sequence. If they each have have an intrinsic\
    \ CallDuration (usually this means block calls), they will get that amount\
    \ of time, at least proportial to the duration of the event. Otherwise,\
    \ if none of them do, they are given equal duration. If some do and some\
    \ don't, you probably get confusing results."
    $ Sig.call calls_arg $ \calls args -> do
        let derivers = calls_to_derivers args calls
        durs <- mapM get_score_duration derivers
        sequence_derivers (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    calls_arg = Sig.many1 "call" "Generator calls."
    score_duration args = do
        calls <- Sig.parse_or_throw calls_arg args
        durs <- mapM get_score_duration (calls_to_derivers args calls)
        return $ Derive.CallDuration (Num.sum durs)

sequence_derivers :: ScoreTime -> ScoreTime -> [Derive.NoteDeriver]
    -> [ScoreTime] -> Derive.NoteDeriver
sequence_derivers start event_dur derivers unstretched_durs = mconcat
    [ Derive.place start dur d
    | (start, dur, d) <- zip3 (scanl (+) start durs) durs derivers
    ]
    where
    durs = map (*stretch) unstretched_durs
    stretch = if total_dur == 0 then 1 else event_dur / total_dur
        where total_dur = Num.sum unstretched_durs

c_sequence_realtime :: Derive.Generator Derive.Note
c_sequence_realtime = Derive.with_score_duration score_duration $
    Derive.generator Module.prelude "sequence-rt" mempty
    "Run the given block calls in sequence. Each call gets its natural\
    \ real time duration. Unlike `sequence`, each block gets its natural\
    \ RealTime duration, rather than being normalized to 1 and then expanded\
    \ to its ScoreTime duration. TODO I can't get the RealTime duration without\
    \ deriving, at which point it's too late to stretch, so the event duration\
    \ has no effect." -- TODO that last sentence is now out of date since
    -- I have Derive.get_real_duration, right?
    $ Sig.call calls_arg $ \calls args -> do
        let derivers = calls_to_derivers args calls
        durs <- mapM get_real_duration derivers
        sequence_derivers_realtime (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    calls_arg = Sig.many1 "call" "Generator calls."
    score_duration args = do
        calls <- Sig.parse_or_throw calls_arg args
        durs <- mapM get_real_duration (calls_to_derivers args calls)
        end <- Call.score_duration (Args.start args) (Num.sum durs)
        return $ Derive.CallDuration $ end - Args.start args

sequence_derivers_realtime :: ScoreTime -> ScoreTime -> [Derive.NoteDeriver]
    -> [RealTime] -> Derive.NoteDeriver
sequence_derivers_realtime start event_dur derivers r_durs = do
    r_start <- Derive.real start
    starts <- mapM Derive.score $ scanl (+) r_start r_durs
    let unstretched_durs = zipWith (-) (drop 1 starts) starts
    let total_dur = Num.sum unstretched_durs
        stretch = if total_dur == 0 then 1 else event_dur / total_dur
    let durs = map (*stretch) unstretched_durs
    mconcat
        [ Derive.place start dur d
        | (start, dur, d) <- zip3 (scanl (+) start durs) durs derivers
        ]

c_parallel :: Derive.Generator Derive.Note
c_parallel = Derive.with_score_duration score_duration $ Derive.generator
    Module.prelude "parallel" mempty "Run the given calls in parallel."
    $ Sig.call calls_arg $ \calls args -> do
        let derivers = calls_to_derivers args calls
        durs <- mapM get_score_duration derivers
        parallel_derivers (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    calls_arg = Sig.many1 "call" "Generator calls."
    score_duration args = do
        calls <- Sig.parse_or_throw calls_arg args
        durs <- mapM get_score_duration (calls_to_derivers args calls)
        return $ Derive.CallDuration $ fromMaybe 0 (Seq.maximum durs)

parallel_derivers :: ScoreTime -> ScoreTime -> [Derive.NoteDeriver]
    -> [ScoreTime] -> Derive.NoteDeriver
parallel_derivers start event_dur derivers durs =
    Derive.stretch stretch $ mconcat
        [ Derive.place start dur d
        | (dur, d) <- zip durs derivers
        ]
    where
    stretch = if call_dur == 0 then 1 else event_dur / call_dur
    call_dur = fromMaybe 0 (Seq.maximum durs)

calls_to_derivers :: Derive.CallableExpr d => Derive.PassedArgs d
    -> NonEmpty BaseTypes.Quoted
    -> [(BaseTypes.Quoted, Derive.Deriver (Stream.Stream d))]
calls_to_derivers args calls = zip (NonEmpty.toList calls)
    (map (Eval.eval_quoted_normalized (Args.context args))
        (NonEmpty.toList calls))

get_score_duration :: (BaseTypes.Quoted, Derive.Deriver a)
    -> Derive.Deriver ScoreTime
get_score_duration (quoted, d) = Derive.get_score_duration d >>= \case
    Left err -> Derive.throw $ "get score dur: " <> pretty err
    Right Derive.Unknown -> Derive.throw $ "unknown score duration for "
        <> ShowVal.show_val quoted
    Right (Derive.CallDuration dur) -> return dur

get_real_duration :: (BaseTypes.Quoted, Derive.Deriver a)
    -> Derive.Deriver RealTime
get_real_duration (quoted, d) = Derive.get_real_duration d >>= \case
    Left err -> Derive.throw $ "get real dur: " <> pretty err
    Right Derive.Unknown -> Derive.throw $ "unknown real duration for "
        <> ShowVal.show_val quoted
    Right (Derive.CallDuration dur) -> return dur


-- * transformers

c_multiple :: Derive.Transformer Derive.Note
c_multiple = Derive.transformer Module.prelude "multiple" mempty
    "Derive the transformed score under different transformers."
    $ Sig.callt (Sig.many1 "transformer" "Derive under each transformer.")
    $ \transformers args deriver ->
        mconcat $ map (apply (Args.context args) deriver)
            (NonEmpty.toList transformers)
    where
    apply ctx deriver trans =
        Eval.eval_quoted_transformers ctx trans deriver

c_debug :: Derive.Transformer Derive.Note
c_debug = Derive.transformer Module.prelude "debug" mempty
    "Save the events at this point in a special log msg. This is useful to\
    \ inspect events at a certain point in a pipeline. You can extract them\
    \ later by looking at the `Log.msg_data` of a log msg with the given tag."
    $ Sig.callt (Sig.required "tag" "Log msg has this text.")
    $ \tag _ deriver -> do
        events <- deriver
        Log.write $ Log.with_dyn tag (Stream.events_of events) $
            Log.msg Log.Debug Nothing "debug call"
        return events

-- ** clip

c_clip :: Derive.Transformer Derive.Note
c_clip = Derive.transformer Module.prelude "clip" mempty
    "Unstretch the deriver to its natural duration, and clip events that lie\
    \ beyond the end of the event. Notes that overlap the end of the event will\
    \ be cut short.\
    \\nThis is only useful with calls that have a natural duration apart from\
    \ whatever their calling event's duration, e.g. block calls."
    $ Sig.call0t $ \args -> unstretch_args args $ \_dur deriver -> do
        end <- Derive.real $ snd (Args.range args)
        trim_events Nothing (Just end) $
            Derive.at (Args.start args) deriver

c_clip_start :: Derive.Transformer Derive.Note
c_clip_start = Derive.transformer Module.prelude "Clip" mempty
    "Like `clip`, but align the named block to the end of the event instead\
    \ of the beginning. Events that then lie before the start are clipped."
    $ Sig.call0t $ \args -> unstretch_args args $ \dur deriver -> do
        start <- Derive.real $ fst (Args.range args)
        trim_events (Just start) Nothing $
            Derive.at (Args.end args - dur) deriver

-- ** loop

c_loop :: Derive.Transformer Derive.Note
c_loop = Derive.transformer Module.prelude "loop" mempty
    "This is similar to `clip`, but when the called note runs out, it is\
    \ repeated."
    $ Sig.call0t $ \args -> unstretch_args args $ \dur deriver -> do
        let (start, end) = Args.range args
        let repeats = ceiling $ (end - start) / dur
            starts = take repeats $ Seq.range_ start dur
        real_end <- Derive.real end
        trim_events Nothing (Just real_end) $ repeat_at starts 1 deriver

c_tile :: Derive.Transformer Derive.Note
c_tile = Derive.transformer Module.prelude "tile" mempty
    "This is like `loop`, but it can start the looped call in its middle\
    \ instead of starting from 0. The effect is as if the loop is tiled from\
    \ the beginning of the called block, and is only \"let through\" during\
    \ the `tile` call. This is useful for patterns that are tied to the meter,\
    \ but may be interrupted at arbitrary times, e.g. sarvalaghu patterns."
    $ Sig.call0t $ \args -> unstretch_args args $ \dur deriver -> do
        let (start, end) = Args.range args
        let sub_start = fromIntegral (floor (start / dur)) * dur
        let repeats = ceiling $ (end - sub_start) / dur
            starts = take repeats $ Seq.range_ sub_start dur
        (real_start, real_end) <- Args.real_range args
        trim_events (Just real_start) (Just real_end) $
            repeat_at starts 1 deriver

c_repeat :: Derive.Transformer Derive.Note
c_repeat = Derive.transformer Module.prelude "repeat" mempty
    "Repeat the score multiple times, fitted within the note duration."
    $ Sig.callt (Sig.required "times" "Repeat this many times.")
    $ \(Typecheck.Positive times) args deriver -> do
        let dur = Args.duration args / fromIntegral times
        let deriver0 = Derive.at (- Args.start args) deriver
        let starts = take times (Seq.range_ (Args.start args) dur)
        repeat_at starts (1 / fromIntegral times) deriver0

-- | Repeat the deriver at the given start times.
repeat_at :: [ScoreTime] -> ScoreTime -> Derive.NoteDeriver
    -> Derive.NoteDeriver
repeat_at starts dur deriver =
    mconcat [Derive.place start dur deriver | start <- starts]

trim_events :: Maybe RealTime -> Maybe RealTime
    -> Derive.NoteDeriver -> Derive.NoteDeriver
trim_events start end deriver = do
    events <- Internal.trim_track_warps start end deriver
    return $ maybe id trim_end end $
        maybe id (Stream.drop_while . event_before) start events
    where
    trim_end e = fmap (clip e) . Stream.take_while (event_before e)
    clip end event
        | Score.event_end event <= end = event
        | otherwise =
            Score.set_duration (max 0 (end - Score.event_start event)) event

-- * util

unstretch_args :: Derive.PassedArgs x
    -> (ScoreTime -> Derive.Deriver a -> Derive.Deriver a)
    -> Derive.Deriver a -> Derive.Deriver a
unstretch_args args = unstretch (Args.start args) (Args.duration args)

-- | Put the deriver at 0t and in its \"natural\" time.  This is only different
-- from its event's time if the deriver has its own duration as per
-- 'Derive.get_score_duration'.
--
-- The generator will do @Derive.place start (event_dur/dur)@, so I have to
-- undo that.
unstretch :: ScoreTime -> ScoreTime
    -> (ScoreTime -> Derive.Deriver a -> Derive.Deriver a)
    -> Derive.Deriver a -> Derive.Deriver a
unstretch start event_dur process deriver = do
    dur <- Derive.get_score_duration deriver >>= \case
        Left _ -> return event_dur
        Right Derive.Unknown -> return event_dur
        Right (Derive.CallDuration dur) -> return dur
    process dur $ flatten dur deriver
    where
    flatten dur = Derive.stretch (1 / (event_dur/dur)) . Derive.at (-start)

-- | Consistent with half-open ranges, block calls try to include events lining
-- up with the start, and exclude ones lining up with the end.
event_before :: RealTime -> Score.Event -> Bool
event_before t = (< t - RealTime.eta) . Score.event_start
