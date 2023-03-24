-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Integrate (
    library, unwarp
) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Seq as Seq
import qualified Derive.Call as Call
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.Library as Library
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import qualified Derive.Warp as Warp

import qualified Perform.RealTime as RealTime
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


library :: Library.Library
library = Library.transformers
    [ ("<<", c_block_integrate)
    , ("<", c_track_integrate)
    ]


-- * block integrate

c_block_integrate :: Derive.Transformer Derive.Note
c_block_integrate = Derive.transformer Module.prelude "block-integrate" mempty
    ("Integrate the output into a new block. The events are returned as-is\
    \ so the block can still be played normally."
    ) $ Sig.callt (
        Sig.defaulted "keep-controls" (Set.fromList [Controls.dynamic])
            "Keep only these controls. Transposers are always kept."
    ) $ \keep_controls _ deriver -> do
        events <- deriver
        block_integrate keep_controls events
        return events

block_integrate :: Set ScoreT.Control -> Stream.Stream Score.Event
    -> Derive.Deriver ()
block_integrate keep_controls events = do
    -- Only collect an integration if this is the top level block.  Otherwise
    -- I can get integrating blocks called from many places and who knows which
    -- one is supposed to be integrated.
    whenJustM (toplevel <$> Internal.get_stack) $ \(block_id, mb_track_id) -> do
        whenJust mb_track_id $ \track_id ->
            Derive.throw $ "block integrate seems to be in a track title: "
                <> pretty track_id
        events <- Derive.eval_ui $ unwarp block_id events
        let keep = keep_controls <> Controls.integrate_keep
        let integrated = Derive.Integrated (Left block_id) $
                fmap (strip_controls keep) events
        Internal.merge_collect $ mempty
            { Derive.collect_integrated = [integrated] }

-- | If the block uses a default tempo, it will get applied once during
-- integration, and again when it's played.  I should avoid applying the
-- default tempo at all for integration, but that's too much bother.  Instead,
-- unwarp the events if the default tempo was applied.
--
-- TODO Getting rid of the default tempo entirely is also an option.
unwarp :: Ui.M m => BlockId -> Stream.Stream Score.Event
    -> m (Stream.Stream Score.Event)
unwarp block_id events = ifM (uses_default_tempo block_id)
    (do tempo <- Ui.get_default UiConfig.default_tempo
        return $ move (RealTime.seconds tempo) events)
    (return events)
    where
    move tempo = fmap $ Score.move (*tempo) . Score.duration (*tempo)

uses_default_tempo :: Ui.M m => BlockId -> m Bool
uses_default_tempo block_id =
    Maybe.isJust . BlockUtil.has_top_tempo_track <$>
        TrackTree.events_tree_of block_id


-- * track integrate

c_track_integrate :: Derive.Transformer Derive.Note
c_track_integrate = Derive.transformer Module.prelude "track-integrate" mempty
    ("Integrate the output into new tracks. Events will be split into tracks\
    \ based on source track, instrument, and scale, as documented in\
    \ 'Cmd.Integrate.Convert'.\
    \\nUnlike block integrate, this doesn't return the events.\
    \ While an integrated block's output is likely to be playable, and\
    \ you can chose whether or not to play it, an integrated track\
    \ is part of a block, so it plays whether you want it or not."
    ) $ Sig.callt (
        Sig.defaulted "keep-controls" (Set.fromList [Controls.dynamic])
            "Keep only these controls. Transposers are always kept."
    ) $ \keep_controls _args deriver -> do
        -- Similar to block_integrate, only collect an integration if this is
        -- at the toplevel.
        toplevel <$> Internal.get_stack >>= \case
            Just (block_id, Nothing) -> Derive.throw $
                "track integrate seems to be in a block title: "
                <> pretty block_id
            Just (block_id, Just track_id) -> do
                -- The derive is intentionally outside of the
                -- 'only_destinations_damaged' check.  This is because I need
                -- the collect from it, specifically 'collect_track_dynamic'.
                -- I could avoid derivation by retaining the track dynamics
                -- from the previous performance, but it seems like this would
                -- lead to it hanging on to lots of garbage, especially since
                -- it would never drop track dynamics entries for deleted
                -- tracks.
                events <- unwarp_events =<< integrate_derive deriver
                -- Always include transposers because they affect the pitches.
                -- TODO: technically they should be from pscale_transposers,
                -- but that's so much work to collect, let's just assume the
                -- standards.
                let keep = keep_controls <> Controls.integrate_keep
                -- I originally guarded this with a hack that would not emit
                -- track integrates if only the destinations had received
                -- damage.  But the track cache now serves this purpose, since
                -- it intentionally doesn't retain 'Derive.collect_integrated'.
                track_integrate block_id track_id $
                    fmap (strip_controls keep) events
            Nothing -> return ()
        return Stream.empty

integrate_derive :: Derive.Deriver a -> Derive.Deriver a
integrate_derive deriver = do
    -- See comment in "Cmd.Integrate.Convert" for why.
    dyn <- Call.dynamic 0
    Derive.with_constant_control Controls.dynamic_integrate dyn deriver

-- | Unwarp integrated events, otherwise the tempo would be applied twice, once
-- during integration and again during derivation of the integrated output.
--
-- Previously I used 'Internal.in_real_time', but it turns out that yields
-- different results for calls that use RealTime.  To get the same results as a
-- play would, I have to do the derive normally and then unwrap after the fact.
-- This doesn't unwarp the control signals, but Integrate.Convert only looks
-- at values on note starts, so the complete signals don't matter.
unwarp_events :: Stream.Stream Score.Event
    -> Derive.Deriver (Stream.Stream Score.Event)
unwarp_events events = do
    warp <- Internal.get_dynamic Derive.state_warp
    return $ fmap (unwarp_event (Warp.unwarp warp)) events

unwarp_event :: (RealTime -> ScoreTime) -> Score.Event -> Score.Event
unwarp_event to_score event = Score.place start (end - start) event
    where
    start = convert (Score.event_start event)
    end = convert (Score.event_end event)
    convert = RealTime.from_score . to_score

strip_controls :: Set ScoreT.Control -> Score.Event -> Score.Event
strip_controls keep event = event
    { Score.event_environ = Env.from_map $ Map.fromAscList $ filter wanted $
        Map.toAscList $ Env.to_map $ Score.event_environ event
    }
    where
    wanted (key, (DeriveT.VSignal _)) = Set.member (ScoreT.Control key) keep
    wanted _ = True

track_integrate :: BlockId -> TrackId -> Stream.Stream Score.Event
    -> Derive.Deriver ()
track_integrate block_id track_id events = do
    events <- Derive.eval_ui $ unwarp block_id events
    let integrated = Derive.Integrated (Right track_id) events
    Internal.merge_collect $ mempty
        { Derive.collect_integrated = [integrated] }

toplevel :: Stack.Stack -> Maybe (BlockId, Maybe TrackId)
toplevel stack = case Stack.block_tracks_of stack of
    [(block_id, track_ids)] -> Just (block_id, Seq.last track_ids)
    _ -> Nothing
