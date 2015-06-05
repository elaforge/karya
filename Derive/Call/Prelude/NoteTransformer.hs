-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on @Derive.Generator Derive.Note@.
module Derive.Call.Prelude.NoteTransformer (note_calls) where
import qualified Data.List.NonEmpty as NonEmpty

import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps
    [ ("sequence", c_sequence)
    , ("parallel", c_parallel)
    ]
    [ ("clip", c_clip)
    , ("Clip", c_clip_start)
    , ("loop", c_loop)
    , ("multiple", c_multiple)
    , ("tile", c_tile)
    ]


-- * generators

-- This isn't a NoteTransformer, but it seems like it belongs here.  What is
-- a better name for the module?
c_sequence :: Derive.Generator Derive.Note
c_sequence = Derive.generator_with_duration get_call_duration Module.prelude
    "sequence" mempty
    "Run the given calls in sequence. If they each have have an intrinsic\
    \ CallDuration (usually this means block calls), they will get that amount\
    \ of time, at least proportial to the duration of the event. Otherwise,\
    \ if none of them do, they are given equal duration. If some do and some\
    \ don't, you probably get confusing results."
    $ Sig.call calls_arg $ \calls args -> do
        let derivers = calls_to_derivers args calls
        durs <- get_durations derivers
        sequence_derivers (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    calls_arg = Sig.many1 "call" "Generator calls."
    get_call_duration args = do
        calls <- Sig.run_or_throw calls_arg args
        durs <- get_durations (calls_to_derivers args calls)
        return $ Derive.Duration (sum durs)

sequence_derivers :: ScoreTime -> ScoreTime -> [Derive.NoteDeriver]
    -> [ScoreTime] -> Derive.NoteDeriver
sequence_derivers start event_dur derivers unstretched_durs = mconcat
    [ Derive.place start dur d
    | (start, dur, d) <- zip3 (scanl (+) start durs) durs derivers
    ]
    where
    durs = map (*stretch) unstretched_durs
    stretch = if call_dur == 0 then 1 else event_dur / call_dur
        where call_dur = sum unstretched_durs

c_parallel :: Derive.Generator Derive.Note
c_parallel = Derive.generator_with_duration get_call_duration Module.prelude
    "parallel" mempty
    "Run the given calls in parallel."
    $ Sig.call calls_arg $ \calls args -> do
        let derivers = calls_to_derivers args calls
        durs <- get_durations derivers
        parallel_derivers (Args.start args) (Args.duration args)
            (map snd derivers) durs
    where
    calls_arg = Sig.many1 "call" "Generator calls."
    get_call_duration args = do
        calls <- Sig.run_or_throw calls_arg args
        durs <- get_durations (calls_to_derivers args calls)
        return $ Derive.Duration $ fromMaybe 0 (Seq.maximum durs)

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

calls_to_derivers :: Derive.Callable d => Derive.PassedArgs d
    -> NonEmpty TrackLang.Quoted -> [(TrackLang.Quoted, Derive.LogsDeriver d)]
calls_to_derivers args calls = zip (NonEmpty.toList calls)
    (map (Eval.eval_quoted_normalized (Args.info args))
        (NonEmpty.toList calls))

get_durations :: [(TrackLang.Quoted, Derive.Deriver a)]
    -> Derive.Deriver [ScoreTime]
get_durations = mapM $ \(sym, d) ->
    Derive.get_call_duration d >>= \dur -> case dur of
        Derive.Unknown ->
            Derive.throw $ "unknown CallDuration for " <> ShowVal.show_val sym
        Derive.Duration dur -> return dur


-- * transformers

c_multiple :: Derive.Transformer Derive.Note
c_multiple = Derive.transformer Module.prelude "multiple" mempty
    "Derive the transformed score under different transformers."
    $ Sig.callt (Sig.many1 "transformer" "Derive under each transformer.")
    $ \transformers args deriver ->
        mconcat $ map (apply (Args.info args) deriver)
            (NonEmpty.toList transformers)
    where
    apply cinfo deriver trans =
        Eval.eval_transformers cinfo (to_transformer trans) deriver

to_transformer :: Either TrackLang.Quoted Score.Instrument -> [TrackLang.Call]
to_transformer val = case val of
    Left (TrackLang.Quoted expr) -> NonEmpty.toList expr
    Right inst -> [TrackLang.literal_call ParseTitle.note_track_symbol
        [TrackLang.to_val inst]]

-- ** clip

c_clip :: Derive.Transformer Derive.Note
c_clip = Derive.transformer Module.prelude "clip" mempty
    "Unstretch the deriver to its natural duration, and clip events that lie\
    \ beyond the end of the event. Notes that overlap the end of the event will\
    \ be cut short.\
    \\nThis is only useful with calls that have a natural duration apart from\
    \ whatever their calling event's duration, e.g. block calls."
    $ Sig.call0t $ \args -> unstretch args $ \_dur deriver -> do
        end <- Derive.real $ snd (Args.range args)
        map (fmap (clip end)) . takeWhile (event_before end) <$>
            Derive.at (Args.start args) deriver
    where
    clip end event = Score.duration (min (end - Score.event_start event)) event

c_clip_start :: Derive.Transformer Derive.Note
c_clip_start = Derive.transformer Module.prelude "Clip" mempty
    "Like `clip`, but align the named block to the end of the event instead\
    \ of the beginning. Events that then lie before the start are clipped."
    $ Sig.call0t $ \args -> unstretch args $ \dur deriver -> do
        start <- Derive.real $ fst (Args.range args)
        dropWhile (event_before start) <$>
            Derive.at (Args.end args - dur) deriver

-- ** loop

c_loop :: Derive.Transformer Derive.Note
c_loop = Derive.transformer Module.prelude "loop" mempty
    "This is similar to `clip`, but when the called note runs out, it is\
    \ repeated."
    $ Sig.call0t $ \args -> unstretch args $ \dur deriver -> do
        let (start, end) = Args.range args
        let repeats = ceiling $ (end - start) / dur
            starts = take repeats $ Seq.range_ start dur
        real_end <- Derive.real end
        takeWhile (event_before real_end) <$>
            mconcat [Derive.at s deriver | s <- starts]

c_tile :: Derive.Transformer Derive.Note
c_tile = Derive.transformer Module.prelude "tile" mempty
    "This is like `loop`, but it can start the looped call in its middle\
    \ instead of starting from 0. The effect is as if the loop is tiled from\
    \ the beginning of the called block, and is only \"let through\" during\
    \ the `tile` call. This is useful for patterns that are tied to the meter,\
    \ but may be interrupted at arbitrary times, e.g. sarvalaghu patterns."
    $ Sig.call0t $ \args -> unstretch args $ \dur deriver -> do
        let (start, end) = Args.range args
        let sub_start = fromIntegral (floor (start / dur)) * dur
        let repeats = ceiling $ (end - sub_start) / dur
            starts = take repeats $ Seq.range_ sub_start dur
        (real_start, real_end) <- Args.real_range args
        dropWhile (event_before real_start) . takeWhile (event_before real_end)
            <$> mconcat [Derive.at s deriver | s <- starts]

-- * util

-- | Put the deriver at 0t and in its \"natural\" time.  This is only different
-- from its event's time if the deriver has its own duration as per
-- 'Derive.get_call_duration'.
--
-- The generator will do @Derive.place start (event_dur/dur)@, so I have to
-- undo that.
unstretch :: Derive.PassedArgs x
    -> (ScoreTime -> Derive.Deriver a -> Derive.Deriver a)
    -> Derive.Deriver a -> Derive.Deriver a
unstretch args process deriver = do
    dur <- Derive.get_call_duration deriver
    case dur of
        Derive.Duration dur | dur /= event_dur ->
            process dur $ flatten dur deriver
        _ -> deriver
    where
    flatten dur = Derive.stretch (1 / (event_dur/dur)) . Derive.at (-start)
    start = Args.start args
    event_dur = Args.duration args

-- | Consistent with half-open ranges, block calls try to include events lining
-- up with the start, and exclude ones lining up with the end.
event_before :: RealTime -> LEvent.LEvent Score.Event -> Bool
event_before t =
    LEvent.either ((< t - RealTime.eta) . Score.event_start) (const True)
