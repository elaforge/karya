module Derive.Call.Integrate (
    note_calls, unwarp
) where
import qualified Data.Maybe as Maybe

import Util.Control
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


-- * call

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls [("<", c_integrate)]

-- | Integrate.
c_integrate :: Derive.NoteCall
c_integrate = Derive.transformer "integrate" $ \args deriver ->
    CallSig.call0 args $ do
        events <- deriver
        integrate_events events
        return events

integrate_events :: Derive.Events -> Derive.Deriver ()
integrate_events events = do
    stack <- Internal.get_stack
    case Maybe.mapMaybe Stack.block_of (Stack.innermost stack) of
        -- Only collect an integration if this is the top level block.
        -- Otherwise I can get integrating blocks called from many places and
        -- who knows which one is supposed to be integrated.
        [block_id] -> do
            events <- Derive.eval_ui "c_integrate" $ unwarp block_id events
            key <- Util.lookup_key
            let integrated = Derive.Integrated events key
            Internal.merge_collect $ mempty
                { Derive.collect_integrated = [integrated] }
        _ -> return ()

-- * create block

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
