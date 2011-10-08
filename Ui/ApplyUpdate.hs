module Ui.ApplyUpdate (apply) where
import Control.Monad
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update


-- | Apply an Update to a State to produce a new state.
--
-- This is basically like the haskell version of "Ui.Sync".
apply :: Update.CmdUpdate -> State.State -> Either String State.State
apply update state = case State.exec state (applym update) of
    Left err -> Left $
        "applying " ++ Pretty.pretty update ++ ": " ++ Pretty.pretty err
    Right state2 -> Right state2

applym :: Update.CmdUpdate -> State.StateId ()
applym (Update.ViewUpdate view_id update) = case update of
    Update.CreateView view -> insert view_id view State.state_views
        (\views st -> st { State.state_views = views })
    Update.DestroyView -> delete view_id State.state_views
        (\views st -> st { State.state_views = views })
    Update.ViewSize rect -> modify $ \view -> view { Block.view_rect = rect }
    Update.Status status ->
        modify $ \view -> view { Block.view_status = status }
    Update.TrackScroll scroll ->
        modify $ \view -> view { Block.view_track_scroll = scroll }
    Update.Zoom zoom -> modify $ \view -> view { Block.view_zoom = zoom }
    Update.Selection selnum sel -> State.set_selection view_id selnum sel
    Update.BringToFront -> return ()
    where modify = State.modify_view view_id

applym (Update.BlockUpdate block_id update) = case update of
    Update.BlockTitle title ->
        modify $ \block -> return $ block { Block.block_title = title }
    Update.BlockConfig config ->
        modify $ \block -> return $ block { Block.block_config = config }
    Update.BlockSkeleton skel ->
        modify $ \block -> return $ block { Block.block_skeleton = skel }
    Update.RemoveTrack tracknum -> do
        modify $ \block -> do
            tracks <- remove_at tracknum (Block.block_tracks block)
            return $ block { Block.block_tracks = tracks }
    Update.InsertTrack tracknum track -> modify $ \block ->
        return $ block { Block.block_tracks =
            Seq.insert_at tracknum track (Block.block_tracks block) }
    Update.BlockTrack tracknum track -> modify $ \block -> do
        tracks <- modify_at (Block.block_tracks block) tracknum (const track)
        return $ block { Block.block_tracks = tracks }
    where
    modify = modify_block block_id
    modify_block block_id f = do
        block <- State.get_block block_id
        State.set_block block_id =<< f block

applym (Update.TrackUpdate track_id update) = case update of
    Update.TrackEvents s e events -> modify $ \track ->
        track { Track.track_events = replace s e events track }
    Update.TrackAllEvents events ->
        modify $ \track -> track { Track.track_events = events }
    Update.TrackTitle title ->
        modify $ \track -> track { Track.track_title = title }
    Update.TrackBg color ->
        modify $ \track -> track { Track.track_bg = color }
    Update.TrackRender render ->
        modify $ \track -> track { Track.track_render = render }
    where
    modify = State.modify_track track_id
    replace s e events = Events.merge events . Events.remove_events s e
        . Track.track_events

applym (Update.RulerUpdate ruler_id ruler) =
    State.modify_ruler ruler_id (const ruler)

applym (Update.StateUpdate update) = case update of
    Update.Config config -> State.modify $
        \st -> st { State.state_config = config }
    Update.CreateBlock block_id block ->
        insert block_id block State.state_blocks set_blocks
    Update.DestroyBlock block_id ->
        delete block_id State.state_blocks set_blocks
    Update.CreateTrack track_id track ->
        insert track_id track State.state_tracks set_tracks
    Update.DestroyTrack track_id ->
        delete track_id State.state_tracks set_tracks
    Update.CreateRuler ruler_id ruler ->
        insert ruler_id ruler State.state_rulers set_rulers
    Update.DestroyRuler ruler_id ->
        delete ruler_id State.state_rulers set_rulers
    where
    set_blocks blocks st = st { State.state_blocks = blocks }
    set_tracks tracks st = st { State.state_tracks = tracks }
    set_rulers rulers st = st { State.state_rulers = rulers }


-- * util

-- These are similar to the utils in "Ui.State" except they are stricter
-- in that they want to throw for out out of range indices, etc.  This is
-- because bad data in an Update indicates that the state is out of sync,
-- which is a bug, while Ui.State functions are expected to be called
-- externally and can be more lenient.

-- | Insert @val@ at @key@ in @get_map state@, throwing if it already exists.
-- Put the map back into @state@ by applying @set_map new_map state@ to it.
insert :: (State.M m, Ord k, Show k) => k -> a -> (State.State -> Map.Map k a)
    -> (Map.Map k a -> State.State -> State.State) -> m ()
insert key val get_map set_map = do
    state <- State.get
    when (key `Map.member` get_map state) $
        State.throw $ show key ++ " already exists"
    State.put (set_map (Map.insert key val (get_map state)) state)

delete :: (State.M m, Ord k, Show k) => k -> (State.State -> Map.Map k a)
    -> (Map.Map k a -> State.State -> State.State) -> m ()
delete key get_map set_map = do
    state <- State.get
    when (key `Map.notMember` get_map state) $
        State.throw $ show key ++ " doesn't exist"
    State.put (set_map (Map.delete key (get_map state)) state)

-- | Modify the @i@th element of @xs@ by applying @f@ to it.
modify_at :: (State.M m) => [a] -> Int -> (a -> a) -> m [a]
modify_at xs i f = case post of
        [] -> State.throw $ "can't replace index " ++ show i
            ++ " of list with length " ++ show (length xs)
        elt : rest -> return (pre ++ f elt : rest)
    where (pre, post) = splitAt i xs

remove_at :: (State.M m) => Int -> [a] -> m [a]
remove_at i xs = case post of
        [] -> State.throw $ "can't remove index " ++ show i
            ++ " of list with length " ++ show (length xs)
        _ : rest -> return $ pre ++ rest
        where (pre, post) = splitAt i xs
