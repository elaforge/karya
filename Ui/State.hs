{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Ui.State where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.State as State
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- import Ui.Types
-- import qualified Ui.Color as Color

import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track


data State = State {
    state_views :: Map.Map Block.ViewId (Block.View, Maybe Block.ViewPtr)
    , state_blocks :: Map.Map Block.BlockId Block.Block
    -- Track data also gets a symbol table.  This is so that I can
    -- efficiently compare a track for identity, and also so I can
    -- change it here and all of its occurrances change.
    , state_tracks :: Map.Map Track.TrackId Track.Track
    , state_rulers :: Map.Map Ruler.RulerId Ruler.Ruler
    } deriving Show
empty = State Map.empty Map.empty Map.empty Map.empty

-- * StateT monadic access

type StateM m = State.StateT State (Error.ErrorT StateError m)
newtype Monad m => StateT m a = StateT (StateM m a)
    deriving (Functor, Monad, Trans.MonadIO)
run_state_t (StateT x) = x

instance Trans.MonadTrans StateT where
    -- lift the op through both monads
    lift = StateT . Trans.lift . Trans.lift

data StateError = StateError String deriving (Eq, Show)
instance Error.Error StateError where
    strMsg = StateError
throw msg = StateT (Error.throwError (StateError msg))

-- get :: StateT State
get :: Monad m => StateT m State
get = StateT State.get
-- put :: State -> StateT ()
put st = StateT (State.put st)
-- modify :: (State -> State) -> StateT ()
modify f = StateT (State.modify f)

run :: Monad m => State -> StateT m a -> m (Either StateError State)
run state = Error.runErrorT . flip State.execStateT state . run_state_t

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

get_view view_id = do
    st <- get
    case Map.lookup view_id (state_views st) of
        Nothing -> throw $ "unknown " ++ show view_id
        Just (view, _) -> return view

insert_view id view = do
    st <- get
    let view_id = Block.ViewId id
    when (view_id `Map.member` state_views st) $
        throw $ show view_id ++ " already exists"
    put (st { state_views =
        Map.insert view_id (view, Nothing) (state_views st) })

get_view_ptr view_id = do
    st <- get
    case Map.lookup view_id (state_views st) of
        Nothing -> throw $ "get_view_ptr: unknown " ++ show view_id
        Just (_, Nothing) -> throw $ "get_view_ptr: no ptr for " ++ show view_id
        Just (_, Just viewp) -> return viewp

add_view_ptr view_id viewp = do
    st <- get
    new_view <- case Map.lookup view_id (state_views st) of
        Nothing -> throw $ "can't add ptr to nonexistent " ++ show view_id
        Just (_, Just ptr) -> throw $ "can't add ptr to " ++ show view_id
            ++ " since it already has " ++ show ptr
        Just (view, Nothing) -> return $ (view, Just viewp)
    put $ st { state_views = Map.insert view_id new_view (state_views st) }

-- ** block

get_block block_id = do
    st <- get
    case Map.lookup block_id (state_blocks st) of
        Nothing -> throw $ "unknown " ++ show block_id
        Just block -> return block

get_view_ptrs_of block_id = do
    st <- get
    return [viewp |
            (view_id, (view, Just viewp)) <- Map.assocs (state_views st),
            Block.view_block view == block_id]

insert_block id block = do
    st <- get
    let block_id = Block.BlockId id
    when (block_id `Map.member` state_blocks st) $
        throw $ show block_id ++ " already exists"
    put (st { state_blocks = Map.insert block_id block (state_blocks st) })

-- ** track

get_track track_id = get >>= lookup_id track_id . state_tracks

insert_track id track = do
    st <- get
    let track_id = Track.TrackId id
    when (track_id `Map.member` state_tracks st) $
        throw $ show track_id ++ " already exists"
    put (st { state_tracks = Map.insert track_id track (state_tracks st) })

lookup_id key map = case Map.lookup key map of
    Nothing -> throw $ "unknown " ++ show key
    Just val -> return val

-- ** ruler

get_ruler ruler_id = do
    st <- get
    case Map.lookup ruler_id (state_rulers st) of
        Nothing -> throw $ "unknown " ++ show ruler_id
        Just ruler -> return ruler

insert_ruler id ruler = do
    st <- get
    let ruler_id = Ruler.RulerId id
    when (ruler_id `Map.member` state_rulers st) $
        throw $ show ruler_id ++ " already exists"
    put (st { state_rulers = Map.insert ruler_id ruler (state_rulers st) })
