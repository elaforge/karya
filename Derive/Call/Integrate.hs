module Derive.Call.Integrate (
    note_calls, unwarp
) where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.RealTime as RealTime
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("<<", c_block_integrate)
    , ("<", c_track_integrate)
    ]

-- * block integrate

c_block_integrate :: Derive.NoteCall
c_block_integrate = Derive.transformer "block-integrate" $ \args deriver ->
    CallSig.call0 args $ do
        events <- deriver
        block_integrate events
        return events

block_integrate :: Derive.Events -> Derive.Deriver ()
block_integrate events = do
    -- Only collect an integration if this is the top level block.  Otherwise
    -- I can get integrating blocks called from many places and who knows which
    -- one is supposed to be integrated.
    maybe_block_id <- frame_of Stack.block_of <$> Internal.get_stack
    when_just maybe_block_id $ \block_id -> do
        events <- Derive.eval_ui "c_block_integrate" $ unwarp block_id events
        key <- Util.lookup_key
        let integrated = Derive.Integrated (Left block_id) events key
        Internal.merge_collect $ mempty
            { Derive.collect_integrated = [integrated] }

-- | If the block uses a default tempo, it will get applied once during
-- integration, and again when it's played.  I should avoid applying the
-- default tempo at all for integration, but that's too much bother.  Instead,
-- unwarp the events if the default tempo was applied.
--
-- TODO Getting rid of the default tempo entirely is also an option.
unwarp :: (State.M m) => BlockId -> Derive.Events -> m Derive.Events
unwarp block_id events = ifM (uses_default_tempo block_id)
    (do tempo <- State.get_default State.default_tempo
        return $ move (RealTime.seconds tempo) events)
    (return events)
    where
    move tempo = map $ fmap $ Score.move (*tempo) . Score.duration (*tempo)

uses_default_tempo :: (State.M m) => BlockId -> m Bool
uses_default_tempo block_id =
    BlockUtil.has_nontempo_track <$> State.events_tree_of block_id


-- * track integrate

-- | Unlike 'c_block_integrate', track integrate doesn't return the integrated
-- events.  While a integrated block's output is likely to be playable in its
-- own right, and you can choose whether or not to play it, an integrated track
-- is part of the block, so it plays whether you want it or not.  Also, it
-- can't be hooked up to the tempo track so it's unlikely to play normally.
c_track_integrate :: Derive.NoteCall
c_track_integrate = Derive.transformer "track-integrate" $ \args deriver ->
    CallSig.call0 args $ do
        stack <- Internal.get_stack
        case (frame_of Stack.block_of stack, frame_of Stack.track_of stack) of
            (Just block_id, Just track_id) ->
                unlessM (only_destinations_damaged block_id track_id) $ do
                    events <- deriver
                    track_integrate block_id track_id events
            _ -> return ()
        return []

-- | If damage is only on the destination tracks of the track I am
-- integrating, then I can take a shortcut and skip integration.  This is
-- very common because integration itself will result in damage on those
-- tracks.
--
-- Without this, every integration has to happen twice, once to modify the
-- destination tracks, and again to discover there was no further change.
--
-- However, blocks with no previous derivation are derived even though they
-- have no damage, so I need another little hack to make this work: blocks
-- deriving for the first time get block damage.
only_destinations_damaged :: BlockId -> TrackId -> Derive.Deriver Bool
only_destinations_damaged block_id track_id = do
    damage <- Internal.get_constant Derive.state_score_damage
    if not (Set.null (Derive.sdamage_blocks damage)) then return False else do
        tracks <- Block.block_integrated_tracks <$> Derive.get_block block_id
        let damaged = Map.keysSet (Derive.sdamage_tracks damage)
        return $ Set.null $ damaged `Set.difference` destinations tracks
    where
    destinations tracks = Set.fromList $ concat
        [Block.integrated_destinations track | track <- tracks,
            Block.integrated_source track == track_id]

track_integrate :: BlockId -> TrackId -> Derive.Events -> Derive.Deriver ()
track_integrate block_id track_id events = do
    key <- Util.lookup_key
    events <- Derive.eval_ui "c_track_integrate" $ unwarp block_id events
    let integrated = Derive.Integrated (Right track_id) events key
    Internal.merge_collect $ mempty
        { Derive.collect_integrated = [integrated] }

frame_of :: (Stack.Frame -> Maybe a) -> Stack.Stack -> Maybe a
frame_of f = msum . map f . Stack.innermost
