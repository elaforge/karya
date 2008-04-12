{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{- |
The overall UI state is described here.  This is an immutable data structure
that contains all the tracks, rulers, note data, and so forth.  It exports
a StateT monad for modification and access.

Since the same block may have >=0 views, and a single track may appear in >=0
blocks, these are stored as IDs rather than directly in their containers.
Using explicit references introduces all the usual problems with pointers like
invalid references and unreferenced data.  The latter is actually a feature
(e.g. having a block with no associated view is perfectly normal), but the
former is a pain.  To ease the pain, IDs should only be created via the monadic
insert_* interface in this module, even though I'm forced to export their
constructors to avoid circular imports.  There may still be problems with IDs
from one State being applied to a different State (likely an older and newer
version of the same State), but I'll deal with that when I get there.

A higher level interface may ease this by automatically creating objects with
automaticly generated IDs.

-}
module Ui.State where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.State as State
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Ui.Update as Update
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track


data State = State {
    state_views :: Map.Map Block.ViewId Block.View
    , state_blocks :: Map.Map Block.BlockId Block.Block
    -- Track data also gets a symbol table.  This is so that I can
    -- efficiently compare a track for identity, and also so I can
    -- change it here and all of its occurrances change.
    , state_tracks :: Map.Map Track.TrackId Track.Track
    , state_rulers :: Map.Map Ruler.RulerId Ruler.Ruler
    } deriving Show
empty = State Map.empty Map.empty Map.empty Map.empty

-- * StateT monadic access

-- TrackUpdates are stored directly instead of being calculated from the state
-- diff.  Is there any way they could get out of sync with the actual change?
-- I don't see how, since the updates are stored by track_id, which should
-- always be associated with the same track, and an operation to move event
-- positions will simply generate another TrackUpdate over the whole track.
-- This does mean TrackUpdates can overlay, so Sync should collapse them.
type StateM m = State.StateT State
    -- State functions can directly write updates.  This is because diffing
    -- event tracks could be too expensive.
    (Writer.WriterT [Update.Update]
        (Error.ErrorT StateError m))
newtype Monad m => StateT m a = StateT (StateM m a)
    deriving (Functor, Monad, Trans.MonadIO)
run_state_t (StateT x) = x

instance Trans.MonadTrans StateT where
    -- lift the op through all monads
    lift = StateT . Trans.lift . Trans.lift . Trans.lift

data StateError = StateError String deriving (Eq, Show)
instance Error.Error StateError where
    strMsg = StateError
throw msg = StateT (Error.throwError (StateError msg))

get :: Monad m => StateT m State
get = StateT State.get
put :: Monad m => State -> StateT m ()
put st = StateT (State.put st)
modify :: Monad m => (State -> State) -> StateT m ()
modify f = StateT (State.modify f)
update :: Monad m => Update.Update -> StateT m ()
update upd = StateT (Writer.tell [upd])

run :: (Monad m) =>
    State -> StateT m a -> m (Either StateError (State, [Update.Update]))
run state = Error.runErrorT . Writer.runWriterT . flip State.execStateT state
    . run_state_t

test :: IO ()
test = do
    print =<< run empty (get_view (Block.ViewId "hi"))
    print =<< run empty (get_block (Block.BlockId "there"))

test2 :: IO ()
test2 = do
    v <- run empty $ do
        get_view (Block.ViewId "hi")
        Trans.liftIO $ print "hi"
    print v
    return ()

-- * Resolve IDs to their referents, or throw.

-- ** view

get_view :: (Monad m) => Block.ViewId -> StateT m Block.View
get_view view_id = get >>= lookup_id view_id . state_views
insert_view :: (Monad m) => String -> Block.View -> StateT m Block.ViewId
insert_view id view = get >>= insert (Block.ViewId id) view state_views
    (\views st -> st { state_views = views })

-- ** block

get_block :: (Monad m) => Block.BlockId -> StateT m Block.Block
get_block block_id = get >>= lookup_id block_id . state_blocks
insert_block :: (Monad m) => String -> Block.Block -> StateT m Block.BlockId
insert_block id block = get >>= insert (Block.BlockId id) block state_blocks
    (\blocks st -> st { state_blocks = blocks })

get_view_ids_of :: (Monad m) => Block.BlockId -> StateT m [Block.ViewId]
get_view_ids_of block_id = do
    st <- get
    return [view_id | (view_id, view) <- Map.assocs (state_views st),
            Block.view_block view == block_id]

-- ** track

get_track :: (Monad m) => Track.TrackId -> StateT m Track.Track
get_track track_id = get >>= lookup_id track_id . state_tracks
insert_track :: (Monad m) => String -> Track.Track -> StateT m Track.TrackId
insert_track id track = get >>= insert (Track.TrackId id) track state_tracks
    (\tracks st -> st { state_tracks = tracks })

modify_track :: (Monad m) =>
    Track.TrackId -> (Track.Track -> Track.Track) -> StateT m ()
modify_track track_id f = do
    get_track track_id -- Throw if track_id is invalid.
    modify (\st -> st { state_tracks =
        Map.adjust f track_id (state_tracks st) })

insert_events track_id pos_evts = do
    update $ Update.TrackUpdate track_id $ Update.UpdateTrack
        (fst (head pos_evts)) (Track.event_end (last pos_evts))
    modify_track track_id $ \track ->
        track { Track.track_events = Track.insert_events pos_evts
            (Track.track_events track) }

-- ** ruler

get_ruler :: (Monad m) => Ruler.RulerId -> StateT m Ruler.Ruler
get_ruler ruler_id = get >>= lookup_id ruler_id . state_rulers
insert_ruler :: (Monad m) => String -> Ruler.Ruler -> StateT m Ruler.RulerId
insert_ruler id ruler = get >>= insert (Ruler.RulerId id) ruler state_rulers
    (\rulers st -> st { state_rulers = rulers })

-- ** util

lookup_id :: (Ord k, Monad m, Show k) => k -> Map.Map k a -> StateT m a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

insert key val get_map set_map state = do
    when (key `Map.member` get_map state) $
        throw $ show key ++ " already exists"
    put (set_map (Map.insert key val (get_map state)) state)
    return key
