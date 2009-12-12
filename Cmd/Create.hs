{- | Cmds to create and destroy blocks, views, tracks, and rulers.

    IDs are automatically created from the state_namespace and the other IDs in
    existence.  In general I think it's a bad idea to try to hard to give IDs
    descriptive names because there's nothing keeping them that way.  The
    description should be in the track title.  Even the given numbers will get
    out of date with their position in the block.

    However, I do allow some naming beyond simple numbers for things which are
    unlikely to change, like tempo tracks and rulers, which don't have any
    other title.  And block IDs are used by the sub-derive mechanism, so those
    should be nameable.
-}
module Cmd.Create where
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control

import Ui
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Cmd.MakeRuler as MakeRuler

import qualified App.Config as Config


-- * global modifications

-- | Rename all IDs beginning with @from.@ to @to.@.
rename_project :: (State.UiStateMonad m, Trans.MonadIO m) =>
    Id.Namespace -> Id.Namespace -> m ()
rename_project from to = State.map_ids set_ns
    where
    set_ns ident
        | ns == from = Id.id to name
        | otherwise = ident
        where (ns, name) = Id.un_id ident

-- | Find tracks which are not found in any block.  Probably used to pass them
-- to State.destroy_track for \"gc\".
orphan_tracks :: (State.UiStateMonad m) => m [TrackId]
orphan_tracks = do
    blocks <- fmap (Map.elems . State.state_blocks) State.get
    let ref_tracks = Set.fromList (concatMap Block.block_track_ids blocks)
    tracks <- fmap (Set.fromAscList . Map.keys . State.state_tracks) State.get
    return $ Set.toList (tracks `Set.difference` ref_tracks)

-- | Find rulers which are not found in any block.
orphan_rulers :: (State.UiStateMonad m) => m [RulerId]
orphan_rulers = do
    blocks <- fmap (Map.elems . State.state_blocks) State.get
    let ref_rulers = Set.fromList (concatMap Block.block_ruler_ids blocks)
    rulers <- fmap (Set.fromAscList . Map.keys . State.state_rulers) State.get
    return $ Set.toList (rulers `Set.difference` ref_rulers)

-- | Find blocks with no associated views.
orphan_blocks :: (State.UiStateMonad m) => m [BlockId]
orphan_blocks = do
    views <- fmap (Map.elems . State.state_views) State.get
    let ref_blocks = Set.fromList (map Block.view_block views)
    blocks <- fmap (Set.fromAscList . Map.keys . State.state_blocks) State.get
    return $ Set.toList (blocks `Set.difference` ref_blocks)

-- * block

block_from_template :: (Monad m) => Bool -> Cmd.CmdT m BlockId
block_from_template include_tracks = do
    template_block_id <- Cmd.get_focused_block
    ruler_id <- get_ruler_id template_block_id 0
    block_id <- block ruler_id
    when include_tracks $ do
        template <- State.get_block template_block_id
        let tracks = drop 1 (Block.block_tracks template)
        forM_ (zip [1..] tracks) $ \(tracknum, track) ->
            case Block.tracklike_id track of
                Block.TId tid rid -> do
                    new_tid <- track_ruler
                        block_id rid tracknum (Block.track_width track)
                    title <- fmap Track.track_title (State.get_track tid)
                    State.set_track_title new_tid title
                _ -> State.insert_track block_id tracknum track
        State.set_skeleton block_id =<< State.get_skeleton template_block_id
    view block_id
    return block_id

-- | BlockIds look like \"ns/b0\", \"ns/b1\", etc.
block :: (Monad m) => RulerId -> Cmd.CmdT m BlockId
block ruler_id = do
    ns <- State.get_project
    blocks <- fmap State.state_blocks State.get
    block_id <- require "block id" $ generate_block_id ns blocks
    b <- Cmd.create_block block_id ""
        [Block.block_track (Block.RId ruler_id) Config.ruler_width]
    return b

-- | Create a block with the given ID name.  Useful for blocks meant to be
-- sub-derived.
named_block :: (Monad m) => String -> RulerId -> Cmd.CmdT m BlockId
named_block name ruler_id = do
    ns <- State.get_project
    b <- Cmd.create_block (Id.id ns name) ""
        [Block.block_track (Block.RId ruler_id) Config.ruler_width]
    return b

-- | Delete a block and any views it appears in.  Also delete any tracks
-- that only appeared in that block.
destroy_block :: (State.UiStateMonad m) => BlockId -> m ()
destroy_block block_id = do
    track_ids <- fmap Block.block_track_ids (State.get_block block_id)
    State.destroy_block block_id
    orphan <- orphan_tracks
    mapM_ State.destroy_track (filter (`elem` orphan) track_ids)

-- ** util

generate_block_id :: Id.Namespace -> Map.Map BlockId _a -> Maybe Id.Id
generate_block_id ns blocks = generate_id ns no_parent "b" Types.BlockId blocks

no_parent :: Id.Id
no_parent = Id.id [] ""

-- * view

view :: (State.UiStateMonad m) => BlockId -> m ViewId
view block_id = do
    views <- State.get_views_of block_id
    view_id <- require "view id" $ generate_view_id block_id views
    rect <- fmap (find_rect Config.view_size . map Block.view_rect . Map.elems
        . State.state_views) State.get
    State.create_view view_id $ Block.view block_id rect Config.zoom

block_view :: (Monad m) => RulerId -> Cmd.CmdT m ViewId
block_view ruler_id = block ruler_id >>= view

-- | ViewIds look like \"ns/b0.v0\", \"ns/b0.v1\", etc.
generate_view_id :: BlockId -> Map.Map ViewId _a -> Maybe Id.Id
generate_view_id block_id views =
    generate_id (Id.id_namespace ident) ident "v" Types.ViewId views
    where ident = Id.unpack_id block_id

-- | Destroy a view, along with the underlying block if there were no other
-- views.
destroy_view :: (State.UiStateMonad m) => ViewId -> m ()
destroy_view view_id = do
    block_id <- State.block_id_of_view view_id
    State.destroy_view view_id
    orphans <- orphan_blocks
    when (block_id `elem` orphans) $
        State.destroy_block block_id

-- * track

-- | Insert a track after the selection, or just append one if there isn't one.
-- This is useful for empty blocks which of course have no selection.
insert_track :: (Monad m) => Bool -> Cmd.CmdT m TrackId
insert_track splice = do
    view_id <- Cmd.get_focused_view
    maybe_sel <- State.get_selection view_id Config.insert_selnum
    case maybe_sel of
        Nothing -> append_track
        Just _ -> insert_track_after_selection splice

append_track :: (Monad m) => Cmd.CmdT m TrackId
append_track = do
    block_id <- Cmd.get_focused_block
    track block_id 99999

insert_track_after_selection :: (Monad m) => Bool -> Cmd.CmdT m TrackId
insert_track_after_selection splice = do
    (_, tracknum, _) <- Selection.get_insert_any
    block_id <- Cmd.get_focused_block
    block <- State.get_block block_id
    let new_tracknum = track_after block tracknum
    track_id <- track block_id new_tracknum
    when splice $
        State.splice_skeleton block_id (new_tracknum, tracknum)
    return track_id

-- | Tracks look like \"ns/b0.t0\", etc.
track_ruler :: (State.UiStateMonad m) =>
    BlockId -> RulerId -> TrackNum -> Types.Width -> m TrackId
track_ruler block_id ruler_id tracknum width = do
    tracks <- fmap State.state_tracks State.get
    track_id <- require "track id" $ generate_track_id block_id "t" tracks
    tid <- State.create_track track_id (empty_track "")
    State.insert_track block_id tracknum
        (Block.block_track (Block.TId tid ruler_id) width)
    return tid

-- | Like 'track_ruler', but copy the ruler from the track to the left.
--
-- If the track to the left is a ruler track, it will assume there is
-- a ".overlay" version of it.
track :: (State.UiStateMonad m) => BlockId -> TrackNum -> m TrackId
track block_id tracknum = do
    -- Clip to valid range so callers can use an out of range tracknum.
    tracknum <- clip_tracknum block_id tracknum
    ruler_id <- get_overlay_ruler_id block_id (tracknum-1)
    track_ruler block_id ruler_id tracknum Config.track_width

-- | Create a track with the given name and title.
-- Looks like \"ns/b0.tempo\".
named_track :: (State.UiStateMonad m) =>
    BlockId -> RulerId -> TrackNum -> String -> String -> m TrackId
named_track block_id ruler_id tracknum name title = do
    ident <- make_id (Id.id_name (Id.unpack_id block_id) ++ "." ++ name)
    all_tracks <- fmap State.state_tracks State.get
    when (Types.TrackId ident `Map.member` all_tracks) $
        State.throw $ "track " ++ show ident ++ " already exists"
    tid <- State.create_track ident (empty_track title)
    State.insert_track block_id tracknum
        (Block.block_track (Block.TId tid ruler_id) Config.track_width)
    return tid

remove_selected_tracks :: (Monad m) => Cmd.CmdT m ()
remove_selected_tracks = do
    block_id <- Cmd.get_focused_block
    (tracknums, _, _, _) <- Selection.tracks
    mapM_ (State.remove_track block_id) (reverse tracknums)

destroy_selected_tracks :: (Monad m) => Cmd.CmdT m ()
destroy_selected_tracks = do
    block_id <- Cmd.get_focused_block
    (tracknums, _, _, _) <- Selection.tracks
    mapM_ (destroy_track block_id) tracknums

-- | Remove a track from a block.  If that was the only block it appeared in,
-- delete the underlying track.  Rulers are never deleted automatically.
destroy_track :: (Monad m) => BlockId -> TrackNum -> Cmd.CmdT m ()
destroy_track block_id tracknum = do
    tracklike <- Cmd.require =<< State.track_at block_id tracknum
    State.remove_track block_id tracknum
    when_just (Block.track_id_of tracklike) $ \track_id -> do
        orphans <- orphan_tracks
        when (track_id `elem` orphans) $
            State.destroy_track track_id

-- | Swap the tracks at the given tracknums.  If one of the tracknums is out
-- of range, the track at the other tracknum will be moved to the beginning or
-- end, i.e. swapped with empty space.
swap_tracks :: (State.UiStateMonad m) => BlockId -> TrackNum -> TrackNum -> m ()
swap_tracks block_id num0 num1 = do
    track0 <- State.block_track_at block_id num0
    track1 <- State.block_track_at block_id num1
    case (track0, track1) of
        (Nothing, Nothing) -> return ()
        (Just t0, Nothing) -> remove num0 >> insert num1 t0
        (Nothing, Just t1) -> remove num1 >> insert num0 t1
        (Just t0, Just t1) -> do
            remove num0 >> insert num0 t1
            remove num1 >> insert num1 t0
    where
    remove = State.remove_track block_id
    insert = State.insert_track block_id

-- ** util

-- | Given a hypothetical track at @tracknum@, what should it's ruler id be?
get_overlay_ruler_id :: (State.UiStateMonad m) => BlockId -> TrackNum
    -> m RulerId
get_overlay_ruler_id block_id tracknum = do
    track <- State.block_track_at block_id tracknum
    -- The overlay suffix is just a convention, so it's not guaranteed to
    -- exist.
    let ruler_id = case fmap Block.tracklike_id track of
            Just (Block.TId _ rid) -> rid
            Just (Block.RId rid) -> add_overlay_suffix rid
            _ -> State.no_ruler
    maybe_ruler <- State.lookup_ruler ruler_id
    return $ maybe State.no_ruler (const ruler_id) maybe_ruler

get_ruler_id :: (State.UiStateMonad m) => BlockId -> TrackNum -> m RulerId
get_ruler_id block_id tracknum = do
    maybe_track <- State.block_track_at block_id tracknum
    let ruler_id = maybe State.no_ruler id $ do
            track <- maybe_track
            Block.ruler_id_of (Block.tracklike_id track)
    maybe_ruler <- State.lookup_ruler ruler_id
    return $ maybe State.no_ruler (const ruler_id) maybe_ruler

add_overlay_suffix :: RulerId -> RulerId
add_overlay_suffix ruler_id
    | overlay_suffix `List.isSuffixOf` ident = ruler_id
    | otherwise = Types.RulerId (Id.id ns (ident ++ overlay_suffix))
    where (ns, ident) = Id.un_id (Id.unpack_id ruler_id)

-- | Clip the tracknum to within the valid range.
clip_tracknum :: (State.UiStateMonad m) => BlockId -> TrackNum -> m TrackNum
clip_tracknum block_id tracknum = do
    tracks <- State.tracks block_id
    return $ max 0 (min tracks tracknum)

track_after :: Block.Block -> TrackNum -> TrackNum
track_after block tracknum
    -- It must already be the rightmost tracknum.
    | tracknum == next_tracknum = length (Block.block_tracks block)
    | otherwise = next_tracknum
    where next_tracknum = Selection.shift_tracknum block tracknum 1

empty_track title = Track.track title [] Config.track_bg Config.render_config

generate_track_id :: BlockId -> String -> Map.Map TrackId _a -> Maybe Id.Id
generate_track_id block_id code tracks =
    generate_id (Id.id_namespace ident) ident code Types.TrackId tracks
    where ident = Id.unpack_id block_id

-- * ruler

-- | This creates both a ruler with the given name, and an overlay version
-- named with .overlay.
ruler :: (State.UiStateMonad m) => String -> Ruler.Ruler -> m (RulerId, RulerId)
ruler name ruler = do
    ident <- make_id name
    overlay_ident <- make_id (name ++ overlay_suffix)
    rid <- State.create_ruler ident ruler
    over_rid <- State.create_ruler overlay_ident (MakeRuler.as_overlay ruler)
    return (rid, over_rid)

-- ** util

make_id :: (State.UiStateMonad m) => String -> m Id.Id
make_id name = do
    ns <- State.get_project
    return (Id.id ns name)

-- | An overlay versions of a ruler has id ruler_id ++ suffix.
overlay_suffix :: String
overlay_suffix = ".overlay"

-- * general util

generate_id :: (Ord a) => Id.Namespace -> Id.Id -> String -> (Id.Id -> a)
    -> Map.Map a _b -> Maybe Id.Id
generate_id ns parent_id code typ fm =
    List.find (not . (`Map.member` fm) . typ) candidates
    where candidates = ids_for ns (Id.id_name parent_id) code

ids_for :: Id.Namespace -> String -> String -> [Id.Id]
ids_for ns parent code =
    [Id.id ns (dotted parent ++ code ++ show n) | n <- [0..]]
    where dotted s = if null s then "" else s ++ "."

require msg = maybe (State.throw $ "somehow can't find ID for " ++ msg) return

-- TODO I also need the screen dimensions to do this right.  Before I go
-- too far here, though, I'll want to think about proper window manager stuff.
-- If I just allow the placement function to be passed as an arg...
find_rect (w, h) rects = Types.Rect right bottom w h
    where
    right = maximum $ 0 : map Types.rect_r rects
    bottom = 10
