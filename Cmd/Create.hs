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

import qualified Ui.Id as Id
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Ruler as Ruler

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
orphan_tracks :: (State.UiStateMonad m) => m [Track.TrackId]
orphan_tracks = do
    blocks <- fmap (Map.elems . State.state_blocks) State.get
    let ref_tracks = Set.fromList $
            concatMap (Block.track_ids_of . Block.block_tracks) blocks
    tracks <- fmap (Set.fromAscList . Map.keys . State.state_tracks) State.get
    return $ Set.toList (tracks `Set.difference` ref_tracks)

-- | Find rulers which are not found in any block.
orphan_rulers :: (State.UiStateMonad m) => m [Ruler.RulerId]
orphan_rulers = do
    blocks <- fmap (Map.elems . State.state_blocks) State.get
    let ref_rulers = Set.fromList $
            concatMap (Block.ruler_ids_of . Block.block_tracks) blocks
    rulers <- fmap (Set.fromAscList . Map.keys . State.state_rulers) State.get
    return $ Set.toList (rulers `Set.difference` ref_rulers)

-- | Find blocks with no associated views.
orphan_blocks :: (State.UiStateMonad m) => m [Block.BlockId]
orphan_blocks = do
    views <- fmap (Map.elems . State.state_views) State.get
    let ref_blocks = Set.fromList (map Block.view_block views)
    blocks <- fmap (Set.fromAscList . Map.keys . State.state_blocks) State.get
    return $ Set.toList (blocks `Set.difference` ref_blocks)

-- * block

-- | BlockIds look like \"ns/b0\", \"ns/b1\", etc.
block :: (State.UiStateMonad m) => Ruler.RulerId -> m Block.BlockId
block ruler_id = do
    ns <- State.get_project
    blocks <- fmap State.state_blocks State.get
    block_id <- require "block id" $ generate_block_id ns blocks
    b <- State.create_block block_id $
        Block.block "" Config.block_config [] Config.schema
    State.insert_track b 0 (Block.RId ruler_id) Config.ruler_width
    return b

-- | Create a block with the given ID name.  Useful for blocks meant to be
-- sub-derived.
named_block :: (State.UiStateMonad m) =>
    String -> Ruler.RulerId -> m Block.BlockId
named_block name ruler_id = do
    ns <- State.get_project
    b <- State.create_block (Id.id ns name) $
        Block.block "" Config.block_config [] Config.schema
    State.insert_track b 0 (Block.RId ruler_id) Config.ruler_width
    return b

generate_block_id ns blocks =
    generate_id ns no_parent "b" Block.BlockId blocks
no_parent = Id.id [] ""

-- * view

view :: (State.UiStateMonad m) => Block.BlockId -> m Block.ViewId
view block_id = do
    views <- State.get_views_of block_id
    view_id <- require "view id" $ generate_view_id views block_id
    rect <- fmap (find_rect Config.view_size . map Block.view_rect . Map.elems
        . State.state_views) State.get
    State.create_view view_id $
        Block.view block_id rect Config.zoom Config.view_config

block_view :: (State.UiStateMonad m) => Ruler.RulerId -> m Block.ViewId
block_view ruler_id = block ruler_id >>= view

-- | ViewIds look like \"ns/b0.v0\", \"ns/b0.v1\", etc.
generate_view_id views block_id =
    generate_id (Id.id_namespace ident) ident "v" Block.ViewId views
    where ident = Id.unpack_id block_id

-- | Same as State.destroy_view, included here for consistency.
destroy_view :: (State.UiStateMonad m) => Block.ViewId -> m ()
destroy_view view_id = State.destroy_view view_id

-- * track

-- | Tracks look like \"ns/b0.t0\", etc.
track_ruler :: (State.UiStateMonad m) =>
    Block.BlockId -> Ruler.RulerId -> Block.TrackNum -> Block.Width
    -> m Track.TrackId
track_ruler block_id ruler_id tracknum width = do
    tracks <- State.get_tracks_of block_id
    track_id <- require "track id" $
        generate_track_id block_id "t" tracks
    tid <- State.create_track track_id (empty_track "")
    State.insert_track block_id tracknum (Block.TId tid ruler_id) width
    return tid

-- | Like 'track_ruler', but copy the ruler and track width from the track to
-- the left.
-- If the track to the left is a ruler track, it will assume there is
-- a ".overlay" version of it.
track :: (State.UiStateMonad m) =>
    Block.BlockId -> Block.TrackNum -> m Track.TrackId
track block_id tracknum = do
    -- Clip to valid range to callers can use an out of range tracknum.
    tracknum <- clip_tracknum block_id tracknum
    maybe_track <- State.track_at block_id (tracknum-1)

    let (ruler_id, width) = case maybe_track of
            Just ((Block.TId _ rid), width) -> (rid, width)
            Just ((Block.RId rid), width) -> (add_overlay_suffix rid, width)
            _ -> (State.no_ruler, Config.track_width)
    -- The above can generate a bad ruler_id if they didn't use 'ruler' to
    -- create the ruler with the overlay version, so abort early if that's the
    -- case.
    State.get_ruler ruler_id
    track_ruler block_id ruler_id tracknum width

add_overlay_suffix ruler_id
    | overlay_suffix `List.isSuffixOf` ident = ruler_id
    | otherwise = Ruler.RulerId (Id.id ns (ident ++ overlay_suffix))
    where (ns, ident) = Id.un_id (Id.unpack_id ruler_id)

clip_tracknum block_id tracknum = do
    tracks <- State.tracks block_id
    return $ max 0 (min tracks tracknum)

-- | Create a track with the given name and title.
-- Looks like \"ns/b0.tempo\".
named_track :: (State.UiStateMonad m) =>
    Block.BlockId -> Ruler.RulerId -> Block.TrackNum
    -> String -> String -> m Track.TrackId
named_track block_id ruler_id tracknum name title = do
    ident <- make_id (Id.id_name (Id.unpack_id block_id) ++ "." ++ name)
    all_tracks <- fmap State.state_tracks State.get
    when (Track.TrackId ident `Map.member` all_tracks) $
        State.throw $ "track " ++ show ident ++ " already exists"
    tid <- State.create_track ident (empty_track title)
    State.insert_track block_id tracknum
        (Block.TId tid ruler_id) Config.track_width
    return tid

-- ** cmds

append_track :: (Monad m) => Cmd.CmdT m Track.TrackId
append_track = do
    block_id <- Cmd.get_focused_block
    track block_id 99999

insert_track_after_selection :: (Monad m) => Cmd.CmdT m Track.TrackId
insert_track_after_selection = do
    (_, tracknum, _) <- Selection.get_insert_pos
    block_id <- Cmd.get_focused_block
    track block_id (tracknum+1)

remove_selected_tracks :: (Monad m) => Cmd.CmdT m ()
remove_selected_tracks = do
    block_id <- Cmd.get_focused_block
    sel <- fmap snd $ Selection.selected_tracks Config.insert_selnum
    mapM_ (State.remove_track block_id) (reverse (Block.sel_tracknums sel))

-- ** util

empty_track title = Track.track title [] Config.track_bg Config.render_config

tracklike_track (Block.TId tid _) = Just tid
tracklike_track _ = Nothing

generate_track_id block_id code tracks =
    generate_id (Id.id_namespace ident) ident code Track.TrackId tracks
    where ident = Id.unpack_id block_id

-- | Swap the tracks at the given tracknums.  If one of the tracknums is out
-- of range, the track at the other tracknum will be moved to the beginning or
-- end, i.e. swapped with empty space.
swap_tracks :: (State.UiStateMonad m) => Block.BlockId
    -> Block.TrackNum -> Block.TrackNum -> m ()
swap_tracks block_id num0 num1 = do
    track0 <- State.track_at block_id num0
    track1 <- State.track_at block_id num1
    case (track0, track1) of
        (Nothing, Nothing) -> return ()
        (Just (t0, w0), Nothing) -> remove num0 >> insert num1 t0 w0
        (Nothing, Just (t1, w1)) -> remove num1 >> insert num0 t1 w1
        (Just (t0, w0), Just (t1, w1)) -> do
            remove num0 >> insert num0 t1 w1
            remove num1 >> insert num1 t0 w0
    where
    remove = State.remove_track block_id
    insert = State.insert_track block_id

-- * ruler

-- | This creates both a ruler with the given name, and an overlay version
-- named with .overlay.
ruler :: (State.UiStateMonad m) => String -> Ruler.Ruler
    -> m (Ruler.RulerId, Ruler.RulerId)
ruler name ruler = do
    ident <- make_id name
    overlay_ident <- make_id (name ++ overlay_suffix)
    rid <- State.create_ruler ident ruler
    over_rid <- State.create_ruler overlay_ident (MakeRuler.as_overlay ruler)
    return (rid, over_rid)

make_id :: (State.UiStateMonad m) => String -> m Id.Id
make_id name = do
    ns <- State.get_project
    return (Id.id ns name)

-- | An overlay versions of a ruler has id ruler_id ++ suffix.
overlay_suffix :: String
overlay_suffix = ".overlay"

-- * util

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
find_rect (w, h) rects = Block.Rect right bottom w h
    where
    right = maximum $ 0 : map Block.rect_r rects
    bottom = 10
