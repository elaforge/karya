-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Integrate (
    library, unwarp
) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Lists as Lists
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
    [ ("<<", cBlockIntegrate)
    , ("<", cTrackIntegrate)
    ]


-- * block integrate

cBlockIntegrate :: Derive.Transformer Derive.Note
cBlockIntegrate = Derive.transformer Module.prelude "block-integrate" mempty
    ("Integrate the output into a new block. The events are returned as-is\
    \ so the block can still be played normally."
    ) $ Sig.callt (
        Sig.defaulted "keep-controls" (Set.fromList [Controls.dynamic])
            "Keep only these controls. Transposers are always kept."
    ) $ \keepControls _ deriver -> do
        events <- deriver
        blockIntegrate keepControls events
        pure events

blockIntegrate :: Set ScoreT.Control -> Stream.Stream Score.Event
    -> Derive.Deriver ()
blockIntegrate keepControls events = do
    -- Only collect an integration if this is the top level block.  Otherwise
    -- I can get integrating blocks called from many places and who knows which
    -- one is supposed to be integrated.
    whenJustM (toplevel <$> Internal.getStack) $ \(blockId, mbTrackId) -> do
        whenJust mbTrackId $ \trackId ->
            Derive.throw $ "block integrate seems to be in a track title: "
                <> pretty trackId
        events <- Derive.evalUi $ unwarp blockId events
        let keep = keepControls <> Controls.integrateKeep
        let integrated = Derive.Integrated (Left blockId) $
                fmap (stripControls keep) events
        Internal.mergeCollect $ mempty
            { Derive.collectIntegrated = [integrated] }

-- | If the block uses a default tempo, it will get applied once during
-- integration, and again when it's played.  I should avoid applying the
-- default tempo at all for integration, but that's too much bother.  Instead,
-- unwarp the events if the default tempo was applied.
--
-- TODO Getting rid of the default tempo entirely is also an option.
unwarp :: Ui.M m => BlockId -> Stream.Stream Score.Event
    -> m (Stream.Stream Score.Event)
unwarp blockId events = ifM (usesDefaultTempo blockId)
    (do tempo <- Ui.getDefault UiConfig.defaultTempo
        pure $ move (RealTime.seconds tempo) events)
    (pure events)
    where
    move tempo = fmap $ Score.move (*tempo) . Score.duration (*tempo)

usesDefaultTempo :: Ui.M m => BlockId -> m Bool
usesDefaultTempo blockId =
    Maybe.isJust . BlockUtil.hasTopTempoTrack <$>
        TrackTree.eventsTreeOf blockId


-- * track integrate

cTrackIntegrate :: Derive.Transformer Derive.Note
cTrackIntegrate = Derive.transformer Module.prelude "track-integrate" mempty
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
    ) $ \keepControls _args deriver -> do
        -- Similar to block_integrate, only collect an integration if this is
        -- at the toplevel.
        toplevel <$> Internal.getStack >>= \case
            Just (blockId, Nothing) -> Derive.throw $
                "track integrate seems to be in a block title: "
                <> pretty blockId
            Just (blockId, Just trackId) -> do
                -- The derive is intentionally outside of the
                -- 'only_destinations_damaged' check.  This is because I need
                -- the collect from it, specifically 'collect_track_dynamic'.
                -- I could avoid derivation by retaining the track dynamics
                -- from the previous performance, but it seems like this would
                -- lead to it hanging on to lots of garbage, especially since
                -- it would never drop track dynamics entries for deleted
                -- tracks.
                events <- unwarpEvents =<< integrateDerive deriver
                -- Always include transposers because they affect the pitches.
                -- TODO: technically they should be from pscale_transposers,
                -- but that's so much work to collect, let's just assume the
                -- standards.
                let keep = keepControls <> Controls.integrateKeep
                -- I originally guarded this with a hack that would not emit
                -- track integrates if only the destinations had received
                -- damage.  But the track cache now serves this purpose, since
                -- it intentionally doesn't retain 'Derive.collect_integrated'.
                trackIntegrate blockId trackId $
                    fmap (stripControls keep) events
            Nothing -> pure ()
        pure Stream.empty

integrateDerive :: Derive.Deriver a -> Derive.Deriver a
integrateDerive deriver = do
    -- See comment in "Cmd.Integrate.Convert" for why.
    dyn <- Call.dynamic 0
    Derive.withConstantControl Controls.dynamicIntegrate dyn deriver

-- | Unwarp integrated events, otherwise the tempo would be applied twice, once
-- during integration and again during derivation of the integrated output.
--
-- Previously I used 'Internal.in_real_time', but it turns out that yields
-- different results for calls that use RealTime.  To get the same results as a
-- play would, I have to do the derive normally and then unwrap after the fact.
-- This doesn't unwarp the control signals, but Integrate.Convert only looks
-- at values on note starts, so the complete signals don't matter.
unwarpEvents :: Stream.Stream Score.Event
    -> Derive.Deriver (Stream.Stream Score.Event)
unwarpEvents events = do
    warp <- Internal.getDynamic Derive.stateWarp
    pure $ fmap (unwarpEvent (Warp.unwarp warp)) events

unwarpEvent :: (RealTime -> ScoreTime) -> Score.Event -> Score.Event
unwarpEvent toScore event = Score.place start (end - start) event
    where
    start = convert (Score.eventStart event)
    end = convert (Score.eventEnd event)
    convert = RealTime.fromScore . toScore

stripControls :: Set ScoreT.Control -> Score.Event -> Score.Event
stripControls keep event = event
    { Score.eventEnviron = Env.fromMap $ Map.fromAscList $ filter wanted $
        Map.toAscList $ Env.toMap $ Score.eventEnviron event
    }
    where
    wanted (key, (DeriveT.VSignal _)) = Set.member (ScoreT.Control key) keep
    wanted _ = True

trackIntegrate :: BlockId -> TrackId -> Stream.Stream Score.Event
    -> Derive.Deriver ()
trackIntegrate blockId trackId events = do
    events <- Derive.evalUi $ unwarp blockId events
    let integrated = Derive.Integrated (Right trackId) events
    Internal.mergeCollect $ mempty
        { Derive.collectIntegrated = [integrated] }

toplevel :: Stack.Stack -> Maybe (BlockId, Maybe TrackId)
toplevel stack = case Stack.blockTracksOf stack of
    [(blockId, trackIds)] -> Just (blockId, Lists.last trackIds)
    _ -> Nothing
