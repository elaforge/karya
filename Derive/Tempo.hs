-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Functions to handle tempo tracks.
module Derive.Tempo (
    extend_signal
    , with_tempo, with_absolute, with_hybrid
#ifdef TESTING
    , tempo_to_warp
#endif
) where
import qualified Data.Vector.Storable as Vector

import qualified Util.TimeVector as TimeVector
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


-- | Extend the last sample of a tempo track signal to the end of the track.
-- This is required because 'Signal.integrate' needs to generate samples to the
-- end of the track, ultimately because 'Signal.compose' doesn't resample its
-- arguments.
extend_signal :: RealTime -> Signal.Tempo -> Signal.Tempo
extend_signal track_end sig = sig <> case Signal.last sig of
    Just (x, y) | x < track_end -> Signal.signal [(track_end, y)]
    _ -> mempty

-- * normal

-- | Warp a deriver with a tempo signal.
--
-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo
-- implies, which is the integral of (1/tempo).
with_tempo :: Monoid a => Bool -- ^ If this tempo track is the toplevel track,
    -- i.e. controls all other tracks in this block, then I noramlize to the
    -- block duration.  See comment below.
    -> Maybe (ScoreTime, ScoreTime)
    -- ^ block start and end, used to normalize block duration to 0--1.  If
    -- Nothing, don't normalize.
    -> Maybe TrackId
    -- ^ Needed to record this track in TrackWarps.  It's optional because if
    -- there's no explicit tempo track there's an implicit tempo around the
    -- whole block, but the implicit one doesn't have a track of course.
    -> Signal.Tempo -> Derive.Deriver a -> Derive.Deriver a
with_tempo toplevel range maybe_track_id signal deriver = do
    let warp = tempo_to_warp signal
    stretch_to_1 <- get_stretch_to_1 range $ \(start, end) -> do
        let real_start = Score.warp_pos warp start
            real_end = Score.warp_pos warp end
            real_dur = real_end - real_start
        -- This is tricky.  I want nested tempo tracks to be equivalent to
        -- nested blocks, so it would be as if the score starting at the tempo
        -- track below were in its own block.  However, in that case there
        -- would have to be an intervening block call event, and its duration
        -- would be the duration of the current block, which would insert an
        -- extra stretch by block duration.  So I simulate that by inserting
        -- the stretch here if this isn't the toplevel tempo track.
        --
        -- Otherwise, I would have to get the really real_dur by looking it up
        -- in the warp of the parent tempo track, which is not the same as the
        -- global warp, so I'd have to store that somewhere, and it seems a lot
        -- more complicated.
        let stretch = 1 / RealTime.to_score real_dur
                * if toplevel then 1 else end - start
        place <- require_nonzero (end - start) real_dur $
            Derive.stretch stretch . Derive.at (- RealTime.to_score real_start)
        return (place, real_dur)
    stretch_to_1 $ Internal.warp warp $ do
        Internal.add_new_track_warp maybe_track_id
        deriver

tempo_to_warp :: Signal.Tempo -> Score.Warp
tempo_to_warp sig
    -- Optimize for a constant (or missing) tempo.
    | Just y <- Signal.constant_val sig =
        Score.Warp Score.id_warp_signal 0 $
            RealTime.seconds (1 / max min_tempo y)
    | otherwise = Score.Warp warp_sig 0 1
    where
    warp_sig = Signal.integrate Signal.tempo_srate $ Signal.map_y (1/) $
         Signal.scalar_min min_tempo sig

min_tempo :: Signal.Y
min_tempo = 0.001

require_nonzero :: ScoreTime -> RealTime -> (a -> a) -> Derive.Deriver (a -> a)
require_nonzero block_dur real_dur ok
    | block_dur == 0 = return id
    | real_dur == 0 = Derive.throw $
        "real time of block with dur " <> showt block_dur <> " was zero"
    | otherwise = return ok

{- | If a tempo track is the block's topmost, it needs to do some special
    stretching to normalize to a length of 1, regardless of the tempo.  This is
    so that when the calling block stretches it to the duration of the event it
    winds up being the right length.  This is skipped for the top level block
    or all pieces would last exactly 1 second.  This is another reason every
    block must have a 'with_tempo' at the top.

    The normalization feature relies on every block having a tempo track as its
    top-level track.  'Derive.Call.BlockUtil.note_deriver' establishes this
    invariant.

    This only needs to apply to the top tempo track, but actually applies to
    all of them.  The subsequent applications should have no effect because
    the duration is already normalized to 1.  This is just because it seems a
    little complicated to get a flag in here about whether the track is the
    top one.

    'Derive.Call.Block.d_block' might seem like a better place to normalize the
    duration, but it doesn't have the local warp yet.

    TODO relying on the stack seems a little implicit, would it be better to
    have an explicit flag?
-}
get_stretch_to_1 :: Monoid a => Maybe (ScoreTime, ScoreTime)
    -> ((ScoreTime, ScoreTime)
        -> Derive.Deriver (Derive.Deriver a -> Derive.Deriver a, RealTime))
    -- ^ Take the block range, and return a transformer to properly place the
    -- block, and the RealTime duration of the block.
    -> Derive.Deriver (Derive.Deriver a -> Derive.Deriver a)
get_stretch_to_1 Nothing _ = return id
get_stretch_to_1 (Just range) compute =
    ifM Internal.is_root_block (return id) $ do
        (transform, dur) <- compute range
        Derive.get_mode >>= \mode -> case mode of
            Derive.RealDurationQuery -> do
                set_real_duration dur
                return $ const $ return mempty
            _ -> return transform

set_real_duration :: RealTime -> Derive.Deriver ()
set_real_duration dur = Internal.modify_collect $ \collect ->
    collect { Derive.collect_real_duration = Derive.CallDuration dur }


-- * absolute

-- | Warp the deriver to have the given tempo like 'with_tempo', but override
-- the existing warp instead of composing with it.
--
-- This can be used to isolate the tempo from any tempo effects that may be
-- going on.
with_absolute :: Monoid a => Bool -> Maybe (ScoreTime, ScoreTime)
    -> Maybe TrackId -> Signal.Tempo -> Derive.Deriver a -> Derive.Deriver a
with_absolute toplevel range maybe_track_id signal deriver = do
    unless toplevel $ Derive.throw
        "nested absolute tracks not supported yet, use 'with_tempo' as a model"
    let warp = tempo_to_warp signal
    place <- get_stretch_to_1 range $ \(block_start, block_end) -> do
        start <- RealTime.to_score <$> Derive.real (0 :: ScoreTime)
        end <- RealTime.to_score <$> Derive.real (1 :: ScoreTime)
        let real_end = Score.warp_pos warp block_end
        let real_dur = real_end - Score.warp_pos warp block_start
        place <- require_nonzero (block_end - block_start) real_dur $
            Internal.place start ((end - start) / RealTime.to_score real_dur)
        return (place, real_dur)
    Internal.in_real_time $ place $ Internal.warp warp $ do
        Internal.add_new_track_warp maybe_track_id
        deriver

-- * hybrid

-- | This is like 'with_tempo', but zero tempo segments are played in absolute
-- time.  That is, they won't stretch along with the non-zero segments.  This
-- means the output will always be at least as long as the absolute sections,
-- so a block call may extend past the end of its event.
with_hybrid :: Monoid a => Bool -> Maybe (ScoreTime, ScoreTime)
    -> Maybe TrackId -> Signal.Tempo -> Derive.Deriver a -> Derive.Deriver a
with_hybrid toplevel range maybe_track_id signal deriver = do
    unless toplevel $ Derive.throw
        "nested hybrid tracks not supported yet, use 'with_tempo' as a model"
    let warp = tempo_to_score_warp signal
    place <- get_stretch_to_1 range $ \(block_start, block_end) -> do
        -- The special treatment of flat segments only works once: after that
        -- it's a normal warp and stretches like any other warp.  So I can't
        -- normalize to 0--1 expecting the caller to have stretched to the
        -- right placement, as 'with_tempo' does.  Instead, I have to
        -- derive the whole thing in real time, and stretch and shift to the
        -- expected time here.
        start <- RealTime.to_score <$> Derive.real (0 :: ScoreTime)
        end <- RealTime.to_score <$> Derive.real (1 :: ScoreTime)
        let block_dur = block_end - block_start
        let absolute = flat_duration (Score.warp_signal warp)
            real_dur = max (RealTime.score absolute)
                (Score.warp_pos warp block_dur)
            -- If the block's absolute time is greater than the time allotted,
            -- the non-absolute bits become infinitely fast.  Infinitely fast
            -- is not very musically interesting, so I limit to very fast.
            -- TODO This should be configurable.
            stretch = if block_dur == absolute then 1
                else max 0.001 $ (end - start - absolute)
                    / (block_dur - absolute)
        -- TODO this is probably wrong for block_start > 0, but I don't care
        -- at the moment.
        place <- require_nonzero block_dur real_dur $
            Internal.in_real_time . Derive.place start stretch
            . Derive.at (-block_start)
        return (place, real_dur)
    place $ hybrid_warp warp $ do
        Internal.add_new_track_warp maybe_track_id
        deriver
    where
    hybrid_warp warp = Internal.modify_warp (\w -> compose w warp)
    -- This is like 'tempo_to_warp', but replaces zero tempo with
    -- zeroes instead of a minimum, as is expected by 'Signal.compose_hybrid'.
    tempo_to_score_warp :: Signal.Tempo -> Score.Warp
    tempo_to_score_warp sig = Score.Warp (hybrid_to_warp sig) 0 1
    hybrid_to_warp :: Signal.Tempo -> Signal.Warp
    hybrid_to_warp = Signal.integrate Signal.tempo_srate
        . Signal.map_y (\y -> if y == 0 then 0 else 1 / y)
    -- Like 'Score.compose_warps', but use 'Signal.compose_hybrid'.  It also
    -- can't use the id signal optimization, since that only works with normal
    -- composition.
    compose w1 w2 = Score.Warp (Signal.compose_hybrid s1 s2) 0 1
        where
        s1 = Score.warp_to_signal w1
        s2 = Score.warp_to_signal w2

-- | Total duration of horizontal segments in the warp signal.  These are
-- the places where 'Signal.compose_hybrid' will emit a 1\/1 line.
flat_duration :: Signal.Warp -> ScoreTime
flat_duration =
    RealTime.to_score . fst . Vector.foldl' go (0, TimeVector.Sample 0 0)
        . Signal.sig_vec
    where
    go (!acc, TimeVector.Sample x0 y0) sample@(TimeVector.Sample x y)
        | y == y0 = (acc + (x - x0), sample)
        | otherwise = (acc, sample)
