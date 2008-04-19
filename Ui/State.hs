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
create_* interface in this module, even though I'm forced to export their
constructors to avoid circular imports.  There may still be problems with IDs
from one State being applied to a different State (likely an older and newer
version of the same State), but I'll deal with that when I get there.

A higher level interface may ease this by automatically creating objects with
automaticly generated IDs.

-}
module Ui.State where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Control.Monad.State as State
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Logger as Logger

import Ui.Types
import qualified Ui.Update as Update
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event


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

-- | Run the given StateT with the given initial state, and return a new
-- state along with updates.  Normally updates are produced by Diff.diff, but
-- for efficiency updates to track data are accumulated when they are actually
-- made.  All the UI needs is a TrackPos range to redraw in, and redrawing
-- the whole track isn't that expensive.
--
-- See the StateM comment for more.
run :: (Monad m) =>
   State -> StateT m a -> m (Either StateError (a, State, [Update.Update]))
run state m = do
    res <- (Error.runErrorT . Logger.run . flip State.runStateT state
        . run_state_t) m
    return $ case res of
        Left err -> Left err
        Right ((val, state), updates) -> Right (val, state, updates)

-- | TrackUpdates are stored directly instead of being calculated from the
-- state diff.  Is there any way they could get out of sync with the actual
-- change?  I don't see how, since the updates are stored by track_id, which
-- should always be associated with the same track, and an operation to move
-- event positions will simply generate another TrackUpdate over the whole
-- track.  This does mean TrackUpdates can overlap, so Sync should collapse
-- them.
type StateM m = State.StateT State
    (Logger.LoggerT Update.Update
        (Error.ErrorT StateError m))
newtype Monad m => StateT m a = StateT (StateM m a)
    deriving (Functor, Monad, Trans.MonadIO)
run_state_t (StateT x) = x

instance Trans.MonadTrans StateT where
    lift = StateT . lift . lift . lift

data StateError = StateError String deriving (Eq, Show)
instance Error.Error StateError where
    strMsg = StateError

class Monad m => UiStateMonad m where
    get :: m State
    put :: State -> m ()
    modify :: (State -> State) -> m ()
    update :: Update.Update -> m ()
    throw :: String -> m a

instance Monad m => UiStateMonad (StateT m) where
    get = StateT State.get
    put st = StateT (State.put st)
    modify f = StateT (State.modify f)
    update upd = (StateT . lift) (Logger.record upd)
    throw msg = (StateT . lift . lift) (Error.throwError (StateError msg))

-- * Resolve IDs to their referents, or throw.

-- ** view
get_view :: (UiStateMonad m) => Block.ViewId -> m Block.View
get_view view_id = get >>= lookup_id view_id . state_views
-- Since the tracks are in the block, Block.view doesn't fill those in, leaving
-- it for this function.  That way the caller doesn't need to go find the block
-- itself.
-- It's a little messy, but as long as the caller uses the Block.array
-- constructor it should be ok.
create_view :: (UiStateMonad m) => String -> Block.View -> m Block.ViewId
create_view id view = do
    block <- get_block (Block.view_block view)
    let view' = view
            { Block.view_track_widths = map snd (Block.block_tracks block) }
    get >>= insert (Block.ViewId id) view' state_views
        (\views st -> st { state_views = views })

-- | Update @tracknum@ of @view_id@ to have width @width@.
-- Functional update still sucks.  An imperative language would have:
-- state.get_view(view_id).tracks[tracknum].width = width
set_track_width :: (UiStateMonad m) =>
    Block.ViewId -> Int -> Block.Width -> m ()
set_track_width view_id tracknum width = do
    view <- get_view view_id
    widths <- modify_at (Block.view_track_widths view) tracknum (const width)
    update_view view_id (view { Block.view_track_widths = widths })

-- ** selections

-- TODO: I'll need something make creating selections with the color from
-- block_selection_colors easy.

-- | Get @view_id@'s selection at @selnum@, or Nothing if there is none.
get_selection :: (UiStateMonad m) => Block.ViewId -> Block.SelNum
    -> m (Maybe Block.Selection)
get_selection view_id selnum = do
    view <- get_view view_id
    return (Map.lookup selnum (Block.view_selections view))

-- | Replace any selection on @view_id@ at @selnum@ with @sel@.
set_selection :: (UiStateMonad m) => Block.ViewId -> Block.SelNum
    -> Block.Selection -> m ()
set_selection view_id selnum sel = do
    view <- get_view view_id
    let sels = Map.insert selnum sel (Block.view_selections view)
    update_view view_id (view { Block.view_selections = sels })

update_view view_id view = modify (\st -> st
    { state_views = Map.adjust (const view) view_id (state_views st)})

-- | Modify the @i@th element of @xs@ by applying @f@ to it.
modify_at :: (UiStateMonad m) => [a] -> Int -> (a -> a) -> m [a]
modify_at xs i f = case post of
    [] -> throw $ "can't replace index " ++ show i
        ++ " of list with length " ++ show (length xs)
    (elt:rest) -> return (pre ++ f elt : rest)
    where (pre, post) = splitAt i xs

-- ** block

get_block :: (UiStateMonad m) => Block.BlockId -> m Block.Block
get_block block_id = get >>= lookup_id block_id . state_blocks
create_block :: (UiStateMonad m) => String -> Block.Block -> m Block.BlockId
create_block id block = get >>= insert (Block.BlockId id) block state_blocks
    (\blocks st -> st { state_blocks = blocks })

get_view_ids_of :: (UiStateMonad m) => Block.BlockId -> m [Block.ViewId]
get_view_ids_of block_id = do
    st <- get
    return [view_id | (view_id, view) <- Map.assocs (state_views st),
            Block.view_block view == block_id]

-- ** track

get_track :: (UiStateMonad m) => Track.TrackId -> m Track.Track
get_track track_id = get >>= lookup_id track_id . state_tracks
create_track :: (UiStateMonad m) => String -> Track.Track -> m Track.TrackId
create_track id track = get >>= insert (Track.TrackId id) track state_tracks
    (\tracks st -> st { state_tracks = tracks })

modify_track :: (UiStateMonad m) =>
    Track.TrackId -> (Track.Track -> Track.Track) -> m ()
modify_track track_id f = do
    get_track track_id -- Throw if track_id is invalid.
    modify (\st -> st { state_tracks =
        Map.adjust f track_id (state_tracks st) })

insert_events :: (UiStateMonad m) =>
    Track.TrackId -> [(TrackPos, Event.Event)] -> m ()
insert_events track_id pos_evts = do
    -- Save stash a track update, see 'run' comment.
    update $ Update.TrackUpdate track_id $ Update.UpdateTrack
        (fst (head pos_evts)) (Track.event_end (last pos_evts))
    modify_track track_id $ \track ->
        track { Track.track_events = Track.insert_events pos_evts
            (Track.track_events track) }

-- ** ruler

get_ruler :: (UiStateMonad m) => Ruler.RulerId -> m Ruler.Ruler
get_ruler ruler_id = get >>= lookup_id ruler_id . state_rulers
create_ruler :: (UiStateMonad m) => String -> Ruler.Ruler -> m Ruler.RulerId
create_ruler id ruler = get >>= insert (Ruler.RulerId id) ruler state_rulers
    (\rulers st -> st { state_rulers = rulers })

-- ** util

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, UiStateMonad m, Show k) => k -> Map.Map k a -> m a
lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- | Insert @val@ at @key@ in @get_map state@, throwing if it already exists.
-- Put the map back into @state@ by applying @set_map new_map state@ to it.
insert :: (UiStateMonad m, Ord k, Show k) =>
    k -> a -> (t -> Map.Map k a) -> (Map.Map k a -> t -> State) -> t -> m k
insert key val get_map set_map state = do
    when (key `Map.member` get_map state) $
        throw $ show key ++ " already exists"
    put (set_map (Map.insert key val (get_map state)) state)
    return key
