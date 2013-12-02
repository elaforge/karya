-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE BangPatterns #-}
-- | Functions to handle tempo tracks.
module Derive.Tempo where
import qualified Data.Vector.Storable as Vector

import Util.Control
import qualified Util.TimeVector as TimeVector
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * normal

-- | Warp a block with the given deriver with the given signal.
--
-- Tempo is the tempo signal, which is the standard musical definition of
-- tempo: trackpos over time.  Warp is the time warping that the tempo
-- implies, which is integral (1/tempo).
--
-- TODO what to do about blocks with multiple tempo tracks?  I think it would
-- be best to stretch the block to the first one.  I could break out
-- stretch_to_1 and have compile apply it to only the first tempo track.
with_tempo :: ScoreTime
    -- ^ Used to stretch the block to a length of 1, regardless of the tempo.
    -- This means that when the calling block stretches it to the duration of
    -- the event it winds up being the right length.  This is skipped for the
    -- top level block or all pieces would last exactly 1 second.  This is
    -- another reason every block must have a 'with_tempo' at the top.
    --
    -- TODO relying on the stack seems a little implicit, would it be better
    -- to pass Maybe BlockId or Maybe ScoreTime?
    --
    -- 'Derive.Call.Block.d_block' might seem like a better place to do this,
    -- but it doesn't have the local warp yet.
    -> Maybe TrackId
    -- ^ Needed to record this track in TrackWarps.  It's optional because if
    -- there's no explicit tempo track there's an implicit tempo around the
    -- whole block, but the implicit one doesn't have a track of course.
    -> Signal.Tempo -> Derive.Deriver a -> Derive.Deriver a
with_tempo block_dur maybe_track_id signal deriver = do
    let warp = tempo_to_warp signal
    root <- Internal.is_root_block
    stretch_to_1 <- if root then return id else do
        let real_dur = Score.warp_pos block_dur warp
        return $ if block_dur == 0 then id
            else if real_dur == 0
            then const $ Derive.throw $ "real time of block with dur "
                ++ show block_dur ++ " was zero"
            else Derive.d_stretch (1 / RealTime.to_score real_dur)
    stretch_to_1 $ Internal.d_warp warp $ do
        Internal.add_new_track_warp maybe_track_id
        deriver

tempo_to_warp :: Signal.Tempo -> Score.Warp
tempo_to_warp sig
    -- Optimize for a constant (or missing) tempo.
    | Signal.is_constant sig =
        let stretch = 1 / max min_tempo (Signal.at 0 sig)
        in Score.Warp Score.id_warp_signal 0 (Signal.y_to_score stretch)
    | otherwise = Score.Warp warp_sig 0 1
    where
    warp_sig = Signal.integrate Signal.tempo_srate $ Signal.map_y (1/) $
         Signal.scalar_min min_tempo sig

min_tempo :: Signal.Y
min_tempo = 0.001


-- * hybrid

-- | This is like 'with_tempo', but
with_hybrid :: ScoreTime -> Maybe TrackId -> Signal.Tempo
    -> Derive.Deriver a -> Derive.Deriver a
with_hybrid block_dur maybe_track_id signal deriver = do
    let warp = tempo_to_score_warp signal
    root <- Internal.is_root_block
    place <- if root then return id else do
        -- The special treatment of flat segments only works once: after that
        -- it's a normal warp and stretches like any other warp.  So I can't
        -- normalize to 0--1 expecting the caller to have stretched to the
        -- right placement, as 'with_tempo' does.  Instead, I have to
        -- derive the whole thing in real time, and stretch and shift to the
        -- expected time here.
        start <- RealTime.to_score <$> Derive.real (0 :: ScoreTime)
        end <- RealTime.to_score <$> Derive.real (1 :: ScoreTime)
        let absolute = flat_duration (Score.warp_signal warp)
            real_dur = max (RealTime.score absolute)
                (Score.warp_pos block_dur warp)
            -- If the block's absolute time is greater than the time allotted,
            -- the non-absolute bits become infinitely fast.  Infinitely fast
            -- is not very musically interesting, so I limit to very fast.
            -- TODO This should be configurable.
            stretch = if block_dur == absolute then 1
                else max 0.001 $ (end - start - absolute)
                    / (block_dur - absolute)
        return $ if block_dur == 0 then id else if real_dur == 0
            then const $ Derive.throw $ "real time of non-zero block dur "
                ++ show block_dur ++ " was zero"
            else Internal.in_real_time . Internal.d_place start stretch
    place $ hybrid_warp warp $ do
        Internal.add_new_track_warp maybe_track_id
        deriver
    where
    hybrid_warp warp = Internal.modify_warp (\w -> compose w warp)
    -- This is like 'tempo_to_warp', but replaces zero tempo with
    -- zeroes instead of a minimum, as is expected by 'Signal.compose_hybrid'.
    tempo_to_score_warp :: Signal.Tempo -> Score.Warp
    tempo_to_score_warp sig = Score.Warp (tempo_to_warp sig) 0 1
    tempo_to_warp :: Signal.Tempo -> Signal.Warp
    tempo_to_warp = Signal.integrate Signal.tempo_srate
        . Signal.map_y (\y -> if y == 0 then 0 else 1 / y)
    -- Like 'Score.compose_warps', but use 'Signal.compose_hybrid'.  It also
    -- can't use the id signal optimization, since that only works with normal
    -- composition.
    compose w1 w2 = Score.Warp (Signal.compose_hybrid s1 s2) 0 1
        where
        s1 = Score.warp_to_signal w1
        s2 = Score.warp_to_signal w2

-- | Total duration of horizontal segments in the warp signal.  These are
-- the places where 'Signal.compose_hybrid' will
flat_duration :: Signal.Warp -> ScoreTime
flat_duration =
    RealTime.to_score . fst . Vector.foldl' go (0, TimeVector.Sample 0 0)
        . Signal.sig_vec
    where
    go (!acc, TimeVector.Sample x0 y0) sample@(TimeVector.Sample x y)
        | y == y0 = (acc + (x - x0), sample)
        | otherwise = (acc, sample)
