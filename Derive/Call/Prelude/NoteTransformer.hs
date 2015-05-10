-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Transformers on @Derive.Generator Derive.Note@.
module Derive.Call.Prelude.NoteTransformer where
import qualified Util.Seq as Seq
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig

import qualified Perform.RealTime as RealTime
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.transformer_call_map
    [ ("clip", c_clip)
    , ("Clip", c_clip_start)
    , ("loop", c_loop)
    , ("tile", c_tile)
    ]

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
