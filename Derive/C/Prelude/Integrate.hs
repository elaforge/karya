-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Integrate (
    note_calls, unwarp
) where
import qualified Data.Maybe as Maybe

import qualified Ui.Ui as Ui
import qualified Ui.TrackTree as TrackTree
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.RealTime as RealTime
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.call_maps []
    [ ("<<", c_block_integrate)
    , ("<", c_track_integrate)
    ]


-- * block integrate

c_block_integrate :: Derive.Transformer Derive.Note
c_block_integrate = Derive.transformer Module.prelude "block-integrate" mempty
    ("Integrate the output into a new block. The events are returned as-is\
    \ so the block can still be played normally."
    ) $ Sig.call0t $ \_ deriver -> do
        events <- deriver
        block_integrate events
        return events

block_integrate :: Stream.Stream Score.Event -> Derive.Deriver ()
block_integrate events = do
    -- Only collect an integration if this is the top level block.  Otherwise
    -- I can get integrating blocks called from many places and who knows which
    -- one is supposed to be integrated.
    maybe_block_id <- frame_of Stack.block_of <$> Internal.get_stack
    whenJust maybe_block_id $ \block_id -> do
        events <- Derive.eval_ui "c_block_integrate" $ unwarp block_id events
        let integrated = Derive.Integrated (Left block_id) events
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
    (do tempo <- Ui.get_default Ui.default_tempo
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
    ) $ Sig.call0t $ \_ deriver -> do
        stack <- Internal.get_stack
        case (frame_of Stack.block_of stack, frame_of Stack.track_of stack) of
            (Just block_id, Just track_id) -> do
                -- The derive is intentionally outside of the
                -- 'only_destinations_damaged' check.  This is because I need
                -- the collect from it, specifically 'collect_track_dynamic'.
                -- I could avoid derivation by retaining the track dynamics
                -- from the previous performance, but it seems like this would
                -- lead to it hanging on to lots of garbage, especially since
                -- it would never drop track dynamics entries for deleted
                -- tracks.
                -- Derive in_real_time, otherwise the tempo would be applied
                -- twice, once during integration and again during derivation
                -- of the integrated output.
                events <- Internal.in_real_time deriver
                -- I originally guarded this with a hack that would not emit
                -- track integrates if only the destinations had received
                -- damage.  But the track cache now serves this purpose, since
                -- it intentionally doesn't retain 'Derive.collect_integrated'.
                track_integrate block_id track_id events
            _ -> return ()
        return Stream.empty

track_integrate :: BlockId -> TrackId -> Stream.Stream Score.Event
    -> Derive.Deriver ()
track_integrate block_id track_id events = do
    events <- Derive.eval_ui "c_track_integrate" $ unwarp block_id events
    let integrated = Derive.Integrated (Right track_id) events
    Internal.merge_collect $ mempty
        { Derive.collect_integrated = [integrated] }

frame_of :: (Stack.Frame -> Maybe a) -> Stack.Stack -> Maybe a
frame_of f = msum . map f . Stack.innermost
