{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}
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
automatically generated IDs.
-}
module Ui.State where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as State
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Generics as Generics

import qualified Util.Seq as Seq
import qualified Util.Log as Log
import qualified Util.Logger as Logger

import Ui.Types
import qualified Ui.Update as Update
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Perform.Midi.Instrument as Instrument


data State = State {
    -- | The project name is prepended to automatically created IDs, so
    -- each project is effectively in its own namespace, and can import
    -- other projects without clashes.  The save file is also derived from
    -- the project name.
    state_project :: String
    , state_project_dir :: String
    , state_views :: Map.Map Block.ViewId Block.View
    , state_blocks :: Map.Map Block.BlockId Block.Block
    -- Track data also gets a symbol table.  This is so that I can
    -- efficiently compare a track for identity, and also so I can
    -- change it here and all of its occurrances change.
    , state_tracks :: Map.Map Track.TrackId Track.Track
    , state_rulers :: Map.Map Ruler.RulerId Ruler.Ruler

    -- | This maps the midi instruments used in this State to their Addrs.
    , state_midi_config :: Instrument.Config
    } deriving (Show, Read, Generics.Typeable, Generics.Data)

-- TODO "initial_state" would be more consistent
empty = State "untitled" "save" Map.empty Map.empty Map.empty ruler_map
    (Instrument.config [] Nothing)
    where ruler_map = Map.fromList [(no_ruler, Ruler.no_ruler)]

-- | Since all TracklikeIds must have a ruler, all States have a special empty
-- ruler that can be used in a \"no ruler\" situation.
no_ruler :: Ruler.RulerId
no_ruler = Ruler.RulerId "* no ruler *"

-- * StateT monadic access

-- | Run the given StateT with the given initial state, and return a new
-- state along with updates.  Normally updates are produced by 'Ui.Diff.diff',
-- but for efficiency updates to track data are accumulated when they are
-- actually made.  All the UI needs is a TrackPos range to redraw in, and
-- redrawing the whole track isn't that expensive.
--
-- See the StateStack comment for more.
run :: (Monad m) =>
   State -> StateT m a -> m (Either StateError (a, State, [Update.Update]))
run state m = do
    res <- (Error.runErrorT . Logger.run . flip State.runStateT state
        . run_state_t) m
    return $ case res of
        Left err -> Left err
        Right ((val, state), updates) -> Right (val, state, updates)

run_state :: State -> StateT Identity.Identity a -> (a, State)
run_state state m = case st of
        Left err -> error $ "state error: " ++ show err
        Right (val, state', _) -> (val, state')
    where st = Identity.runIdentity (run state m)

eval_rethrow :: (UiStateMonad m) => State -> StateT Identity.Identity a -> m a
eval_rethrow = eval throw return

-- | A form of 'run' that throws away the output state and updates, and applies
-- either 'failed' or 'succeeded' on the result depending on if the monad threw
-- or not.
eval :: (String -> t) -> (a -> t) -> State -> StateT Identity.Identity a -> t
eval failed succeeded state m = case st of
        Left (StateError err) -> failed err
        Right (val, _, _) -> succeeded val
    where st = Identity.runIdentity (run state m)

-- | TrackUpdates are stored directly instead of being calculated from the
-- state diff.
--
-- Is there any way they could get out of sync with the actual change?  I don't
-- see how, since the updates are stored by track_id, which should always be
-- associated with the same track, and an operation to move event positions
-- will simply generate another TrackUpdate over the whole track.  This does
-- mean TrackUpdates can overlap, so 'Ui.Sync.sync' should collapse them.
type StateStack m = State.StateT State
    (Logger.LoggerT Update.Update
        (Error.ErrorT StateError m))
newtype StateT m a = StateT (StateStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError StateError)
run_state_t (StateT x) = x

instance Trans.MonadTrans StateT where
    lift = StateT . lift . lift . lift

data StateError = StateError String deriving (Eq, Show, Generics.Typeable)
instance Error.Error StateError where
    strMsg = StateError

-- TODO remove modify and implement in terms of get and put?
class (Monad m, Functor m) => UiStateMonad m where
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

-- * misc

-- | Unfortunately there are some invariants to protect within State.  This
-- will check the invariants, log warnings and fix them if possible, or
-- throw an error if not.
--
-- The invariants should be protected by the modifiers in this module, but
-- this is just in case.
verify :: State -> (Either StateError State, [Log.Msg])
verify state = (fmap (\(_, s, _) -> s) result, logs)
    where (result, logs) = Identity.runIdentity (Log.run (run state do_verify))

-- TODO
-- check that all views refer to valid blocks, and all TracklikeIds have
-- referents
-- anything else?
do_verify = do
    view_ids <- get_all_view_ids
    mapM_ verify_view view_ids

verify_view :: Block.ViewId -> StateT (Log.LogT Identity.Identity) ()
verify_view view_id = do
    view <- get_view view_id
    block <- get_block (Block.view_block view)
    let btracks = length (Block.block_tracks block)
        vtracks = length (Block.view_tracks view)
    when (btracks /= vtracks) $
        Trans.lift $ Log.warn $ "block has " ++ show btracks
            ++ " tracks while view has " ++ show vtracks ++ ", fixing"
    -- Add track views for all the block tracks.
    forM_ [vtracks .. btracks-1] $ \tracknum ->
        modify_view view_id $ \v -> insert_into_view tracknum 20 v

get_project :: (UiStateMonad m) => m String
get_project = fmap state_project get

set_project :: (UiStateMonad m) => String -> m ()
set_project name = modify $ \st -> st { state_project = name }

get_midi_config :: (UiStateMonad m) => m Instrument.Config
get_midi_config = fmap state_midi_config get

set_midi_config :: (UiStateMonad m) => Instrument.Config -> m ()
set_midi_config config = modify $ \st -> st { state_midi_config = config}

-- * view

get_view :: (UiStateMonad m) => Block.ViewId -> m Block.View
get_view view_id = get >>= lookup_id view_id . state_views

get_all_view_ids :: (UiStateMonad m) => m [Block.ViewId]
get_all_view_ids = fmap (Map.keys . state_views) get

-- | Create a new view.  Block.view_tracks can be left empty, since it will
-- be replaced by views generated from the the block.  If the caller uses the
-- 'Block.view' constructor, it won't have to worry about this.
create_view :: (UiStateMonad m) => String -> Block.View -> m Block.ViewId
create_view id view = do
    block <- get_block (Block.view_block view)
    let view' = view { Block.view_tracks = initial_track_views block }
    get >>= insert (Block.ViewId id) view' state_views
        (\views st -> st { state_views = views })
initial_track_views block = map Block.TrackView widths
    where widths = map snd (Block.block_tracks block)

destroy_view :: (UiStateMonad m) => Block.ViewId -> m ()
destroy_view view_id = modify $ \st ->
    st { state_views = Map.delete view_id (state_views st) }

set_view_config :: (UiStateMonad m) => Block.ViewId -> Block.ViewConfig -> m ()
set_view_config view_id config =
    modify_view view_id (\view -> view { Block.view_config = config })

-- | Update @tracknum@ of @view_id@ to have width @width@.
set_track_width :: (UiStateMonad m) =>
    Block.ViewId -> Block.TrackNum -> Block.Width -> m ()
set_track_width view_id tracknum width = do
    view <- get_view view_id
    -- Functional update still sucks.  An imperative language would have:
    -- state.get_view(view_id).tracks[tracknum].width = width
    track_views <- modify_at "set_track_width"
        (Block.view_tracks view) tracknum $ \tview ->
            tview { Block.track_view_width = width }
    update_view view_id (view { Block.view_tracks = track_views })

-- ** zoom and track scroll

set_zoom :: (UiStateMonad m) => Block.ViewId -> Block.Zoom -> m ()
set_zoom view_id zoom =
    modify_view view_id (\view -> view { Block.view_zoom = clamped })
    where
    clamped = zoom
        { Block.zoom_offset = max (TrackPos 0) (Block.zoom_offset zoom) }

set_track_scroll :: (UiStateMonad m) => Block.ViewId -> Block.Width -> m ()
set_track_scroll view_id offset =
    modify_view view_id (\view -> view { Block.view_track_scroll = offset })

set_view_rect :: (UiStateMonad m) => Block.ViewId -> Block.Rect -> m ()
set_view_rect view_id rect =
    modify_view view_id (\view -> view { Block.view_rect = rect })

-- ** selections

-- | Get @view_id@'s selection at @selnum@, or Nothing if there is none.
get_selection :: (UiStateMonad m) => Block.ViewId -> Block.SelNum
    -> m (Maybe Block.Selection)
get_selection view_id selnum = do
    view <- get_view view_id
    return (Map.lookup selnum (Block.view_selections view))

-- | Replace any selection on @view_id@ at @selnum@ with @sel@.
set_selection :: (UiStateMonad m) => Block.ViewId -> Block.SelNum
    -> Maybe Block.Selection -> m ()
set_selection view_id selnum maybe_sel = do
    view <- get_view view_id
    let sels = case maybe_sel of
            Nothing -> Map.delete selnum (Block.view_selections view)
            Just sel -> Map.insert selnum sel (Block.view_selections view)
    update_view view_id (view { Block.view_selections = sels })

-- ** util

update_view view_id view = modify $ \st -> st
    { state_views = Map.adjust (const view) view_id (state_views st) }
modify_view view_id f = do
    view <- get_view view_id
    update_view view_id (f view)

-- * block

get_all_block_ids :: (UiStateMonad m) => m [Block.BlockId]
get_all_block_ids = fmap (Map.keys . state_blocks) get

get_block :: (UiStateMonad m) => Block.BlockId -> m Block.Block
get_block block_id = get >>= lookup_id block_id . state_blocks

create_block :: (UiStateMonad m) => String -> Block.Block -> m Block.BlockId
create_block id block = get >>= insert (Block.BlockId id) block state_blocks
    (\blocks st -> st { state_blocks = blocks })

-- | Destroy the block and all the views that display it.
destroy_block :: (UiStateMonad m) => Block.BlockId -> m ()
destroy_block block_id = do
    views <- get_views_of block_id
    mapM_ destroy_view (Map.keys views)
    modify $ \st -> st { state_blocks = Map.delete block_id (state_blocks st) }

block_of_view :: (UiStateMonad m) => Block.ViewId -> m Block.Block
block_of_view view_id = get_block . Block.view_block =<< get_view view_id

set_block_config :: (UiStateMonad m) => Block.BlockId -> Block.Config -> m ()
set_block_config block_id config =
    modify_block block_id (\block -> block { Block.block_config = config })

set_edit_box :: (UiStateMonad m) => Block.BlockId -> Color -> m ()
set_edit_box block_id color = do
    block <- get_block block_id
    set_block_config block_id $
        (Block.block_config block) { Block.config_track_box_color = color }

set_play_box :: (UiStateMonad m) => Block.BlockId -> Color -> m ()
set_play_box block_id color = do
    block <- get_block block_id
    set_block_config block_id $
        (Block.block_config block) { Block.config_sb_box_color = color }

-- ** tracks

insert_track :: (UiStateMonad m) => Block.BlockId -> Block.TrackNum
    -> Block.TracklikeId -> Block.Width -> m ()
insert_track block_id tracknum track width = do
    block <- get_block block_id
    views <- get_views_of block_id
    let tracks = Block.block_tracks block
        tracks' = Seq.insert_at tracks tracknum (track, width)
        views' = Map.map (insert_into_view tracknum width) views
    update_block block_id (block { Block.block_tracks = tracks' })
    modify $ \st -> st { state_views = Map.union views' (state_views st) }

remove_track :: (UiStateMonad m) => Block.BlockId -> Block.TrackNum -> m ()
remove_track block_id tracknum = do
    block <- get_block block_id
    views <- get_views_of block_id
    let tracks' = Seq.remove_at (Block.block_tracks block) tracknum
        views' = Map.map (remove_from_view tracknum) views
    update_block block_id (block { Block.block_tracks = tracks' })
    modify $ \st -> st { state_views = Map.union views' (state_views st) }

-- | Get the TracklikeId at @tracknum@, or Nothing if its out of range.
-- This is a little inconsistent with 'insert_track' and 'remove_track' which
-- automatically clip to range, but is convenient in practice.
track_at :: (UiStateMonad m) => Block.BlockId -> Block.TrackNum
    -> m (Maybe (Block.TracklikeId, Block.Width))
track_at block_id tracknum = do
    block <- get_block block_id
    return $ Seq.at (Block.block_tracks block) tracknum

tracks :: (UiStateMonad m) => Block.BlockId -> m Block.TrackNum
tracks block_id = do
    block <- get_block block_id
    return $ length (Block.block_tracks block)

get_tracklike :: (UiStateMonad m) => Block.TracklikeId -> m Block.Tracklike
get_tracklike track = case track of
    Block.TId track_id ruler_id ->
        liftM2 Block.T (get_track track_id) (get_ruler ruler_id)
    Block.RId ruler_id ->
        liftM Block.R (get_ruler ruler_id)
    Block.DId divider -> return (Block.D divider)

-- *** track util

-- Insert a new track into Block.view_tracks, moving selections as
-- appropriate.  @tracknum@ is clipped to be in range.
insert_into_view tracknum width view = view
    { Block.view_tracks = Seq.insert_at (Block.view_tracks view) tracknum
        (Block.TrackView width)
    , Block.view_selections =
        Map.map (insert_into_selection tracknum) (Block.view_selections view)
    }

-- Remove @tracknum@ from Block.view_tracks, moving selections as
-- appropriate.  Ignored if @tracknum@ is out of range.
remove_from_view tracknum view = view
    { Block.view_tracks = Seq.remove_at (Block.view_tracks view) tracknum
    , Block.view_selections = Map.mapMaybe
        (remove_from_selection tracknum) (Block.view_selections view)
    }

-- If tracknum is before or at the selection, push it to the right.  If it's
-- inside, extend it.  If it's to the right, do nothing.
insert_into_selection tracknum sel
    | tracknum <= start = sel { Block.sel_start_track = start + 1 }
    | tracknum < start + tracks = sel { Block.sel_tracks = tracks + 1 }
    | otherwise = sel
    where
    start = Block.sel_start_track sel
    tracks = Block.sel_tracks sel
remove_from_selection tracknum sel
    | tracknum <= start = Just (sel { Block.sel_start_track = start - 1 })
    | tracknum < start + tracks = if tracks == 1
        then Nothing
        else Just (sel { Block.sel_tracks = tracks - 1 })
    | otherwise = Just sel
    where
    start = Block.sel_start_track sel
    tracks = Block.sel_tracks sel

-- ** other

set_block_title :: (UiStateMonad m) => Block.BlockId -> String -> m ()
set_block_title block_id title =
    modify_block block_id (\block -> block { Block.block_title = title })

-- | Set a status variable on a view.
set_view_status :: (UiStateMonad m) => Block.ViewId -> String -> Maybe String
    -> m ()
set_view_status view_id key val =
    modify_view view_id $ \view -> view { Block.view_status =
        Map.alter (const val) key (Block.view_status view) }

-- ** util

update_block block_id block = modify $ \st -> st
    { state_blocks = Map.adjust (const block) block_id (state_blocks st) }
modify_block block_id f = do
    block <- get_block block_id
    update_block block_id (f block)

-- * track

get_track :: (UiStateMonad m) => Track.TrackId -> m Track.Track
get_track track_id = get >>= lookup_id track_id . state_tracks

create_track :: (UiStateMonad m) => String -> Track.Track -> m Track.TrackId
create_track id track = get >>= insert (Track.TrackId id) track state_tracks
    (\tracks st -> st { state_tracks = tracks })

-- | Destroy the track and remove it from all the blocks it's in.
destroy_track :: (UiStateMonad m) => Track.TrackId -> m ()
destroy_track track_id = do
    blocks <- blocks_with_track track_id
    forM_ blocks $ \(block_id, tracknum, _) -> do
        remove_track block_id tracknum
    modify $ \st -> st { state_tracks = Map.delete track_id (state_tracks st) }

set_track_title :: (UiStateMonad m) => Track.TrackId -> String -> m ()
set_track_title track_id text = modify_track track_id $ \track ->
    track { Track.track_title = text }

set_track_bg :: (UiStateMonad m) => Track.TrackId -> Color -> m ()
set_track_bg track_id color = modify_track track_id $ \track ->
    track { Track.track_bg = color }

-- | Insert events into track_id as per 'Track.insert_events'.
insert_events :: (UiStateMonad m) =>
    Track.TrackId -> [(TrackPos, Event.Event)] -> m ()
insert_events track_id pos_evts = do
    -- Stash a track update, see 'run' comment.
    modify_events track_id (Track.insert_events pos_evts)
    when (not (null pos_evts)) $
        update $ Update.TrackUpdate track_id $ Update.TrackEvents
            (fst (head pos_evts)) (Track.event_end (last pos_evts))

-- | Remove any events whose starting positions fall within the half-open
-- range given.
remove_events :: (UiStateMonad m) =>
    Track.TrackId -> TrackPos -> TrackPos -> m ()
remove_events track_id start end = do
    track <- get_track track_id
    let evts = takeWhile ((<end) . fst)
            (Track.forward (Track.track_events track) start)
    modify_events track_id (Track.remove_events start end)
    when (not (null evts)) $
        update $ Update.TrackUpdate track_id
            (Update.TrackEvents start (Track.event_end (last evts)))

-- | Remove a single event at @pos@, if there is one.
remove_event :: (UiStateMonad m) => Track.TrackId -> TrackPos -> m ()
remove_event track_id pos = do
    track <- get_track track_id
    case Track.event_at (Track.track_events track) pos of
        Nothing -> return ()
        Just evt -> do
            let end = Track.event_end (pos, evt)
            modify_events track_id (Track.remove_events pos end)
            update $ Update.TrackUpdate track_id (Update.TrackEvents pos end)

-- | Emit track updates for all tracks.  Use this when events have changed but
-- I don't know which ones, e.g. when loading a file or restoring a previous
-- state.
update_all_tracks :: (UiStateMonad m) => m ()
update_all_tracks = do
    st <- get
    let updates = map (flip Update.TrackUpdate Update.TrackAllEvents)
            (Map.keys (state_tracks st))
    mapM_ update updates

-- ** util

update_track track_id track = modify $ \st -> st
    { state_tracks = Map.adjust (const track) track_id (state_tracks st) }
modify_track track_id f = do
    track <- get_track track_id
    update_track track_id (f track)
modify_events track_id f = modify_track track_id $ \track ->
    track { Track.track_events = f (Track.track_events track) }

-- * ruler

get_ruler :: (UiStateMonad m) => Ruler.RulerId -> m Ruler.Ruler
get_ruler ruler_id = get >>= lookup_id ruler_id . state_rulers
create_ruler :: (UiStateMonad m) => String -> Ruler.Ruler -> m Ruler.RulerId
create_ruler id ruler = get >>= insert (Ruler.RulerId id) ruler state_rulers
    (\rulers st -> st { state_rulers = rulers })

insert_marklist :: (UiStateMonad m) =>
    Ruler.RulerId -> Int -> (Ruler.MarklistName, Ruler.Marklist) -> m ()
insert_marklist ruler_id i marklist = modify_ruler ruler_id $ \ruler ->
    ruler { Ruler.ruler_marklists =
        Seq.insert_at (Ruler.ruler_marklists ruler) i marklist }

remove_marklist :: (UiStateMonad m) => Ruler.RulerId -> Block.TrackNum -> m ()
remove_marklist ruler_id n = modify_ruler ruler_id $ \ruler -> ruler
    { Ruler.ruler_marklists = Seq.remove_at (Ruler.ruler_marklists ruler) n }

modify_ruler ruler_id f = do
    ruler <- get_ruler ruler_id
    modify $ \st ->
        st { state_rulers = Map.insert ruler_id (f ruler) (state_rulers st) }

-- * search

-- | Get all views of a given block.
get_views_of :: (UiStateMonad m) =>
    Block.BlockId -> m (Map.Map Block.ViewId Block.View)
get_views_of block_id = do
    views <- fmap state_views get
    return $ Map.filter ((==block_id) . Block.view_block) views

-- | Get all the tracks in a given block.
get_tracks_of :: (UiStateMonad m) =>
    Block.BlockId -> m (Map.Map Track.TrackId Track.Track)
get_tracks_of block_id = do
    tracks <- fmap state_tracks get
    block <- get_block block_id
    let track_ids = [tid | (Block.TId tid _, _) <- Block.block_tracks block]
    tracks <- mapM get_track track_ids
    return $ Map.fromList (zip track_ids tracks)

-- | Find @track_id@ in all the blocks it exists in, and return the track info
-- for each tracknum at which @track_id@ lives.
blocks_with_track :: (UiStateMonad m) =>
    Track.TrackId -> m [(Block.BlockId, Block.TrackNum, Block.TracklikeId)]
blocks_with_track track_id = find_tracks ((== Just track_id) . track_id_of)
    where
    track_id_of (Block.TId tid _) = Just tid
    track_id_of _ = Nothing

-- | Just like 'blocks_with_track' except for ruler_id.
blocks_with_ruler :: (UiStateMonad m) =>
    Ruler.RulerId -> m [(Block.BlockId, Block.TrackNum, Block.TracklikeId)]
blocks_with_ruler ruler_id = find_tracks ((== Just ruler_id) . ruler_id_of)
    where
    ruler_id_of (Block.TId _ rid) = Just rid
    ruler_id_of (Block.RId rid) = Just rid
    ruler_id_of _ = Nothing

find_tracks :: (UiStateMonad m) => (Block.TracklikeId -> Bool)
    -> m [(Block.BlockId, Block.TrackNum, Block.TracklikeId)]
find_tracks f = do
    st <- get
    return
        [ (block_id, tracknum, tracklike_id)
        | (block_id, block) <- Map.assocs (state_blocks st)
        , (tracknum, tracklike_id) <- all_tracks block
        , f tracklike_id
        ]
    where all_tracks block = Seq.enumerate (map fst (Block.block_tracks block))

-- * util

-- | Lookup @map!key@, throwing if it doesn't exist.
lookup_id :: (Ord k, Show k, UiStateMonad m) => k -> Map.Map k a -> m a
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

-- | Modify the @i@th element of @xs@ by applying @f@ to it.
modify_at :: (UiStateMonad m) => String -> [a] -> Int -> (a -> a) -> m [a]
modify_at msg xs i f = case post of
    [] -> throw $ msg ++ ": can't replace index " ++ show i
        ++ " of list with length " ++ show (length xs)
    (elt:rest) -> return (pre ++ f elt : rest)
    where (pre, post) = splitAt i xs
