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
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Util.Tree as Tree

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Transform as Transform
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.Selection as Selection

import qualified App.Config as Config
import Types


-- * global modifications

-- | Set the project to the given value and renamespace the old project to the
-- new one.
rename_project :: (State.M m) => Id.Namespace -> m ()
rename_project ns = do
    old_ns <- State.get_namespace
    renamespace old_ns ns
    State.set_namespace ns

-- | Rename all IDs in namespace @from@ to @to@.
renamespace :: (State.M m) => Id.Namespace -> Id.Namespace -> m ()
renamespace from to = Transform.map_ids set_ns
    where
    set_ns ident
        | ns == from = fromMaybe ident (Id.id to name)
        | otherwise = ident
        where (ns, name) = Id.un_id ident

rename_ruler :: (State.M m) => RulerId -> RulerId -> m ()
rename_ruler ruler_id new_name = Transform.map_ruler_ids $ \id ->
    if Types.RulerId id == ruler_id then Id.unpack_id new_name else id

rename_block :: (State.M m) => BlockId -> BlockId -> m ()
rename_block block_id new_name = Transform.map_block_ids $ \id ->
    if Types.BlockId id == block_id then Id.unpack_id new_name else id

-- | Find tracks which are not found in any block.  Probably used to pass them
-- to State.destroy_track for \"gc\".
orphan_tracks :: (State.M m) => m (Set.Set TrackId)
orphan_tracks = do
    blocks <- State.gets (Map.elems . State.state_blocks)
    let ref_tracks = Set.fromList (concatMap Block.block_track_ids blocks)
    tracks <- State.gets (Set.fromAscList . Map.keys . State.state_tracks)
    return $ tracks `Set.difference` ref_tracks

-- | Like 'orphan_tracks' but more efficiently check if a single track is an
-- orphan.
orphan_track :: (State.M m) => TrackId -> m Bool
orphan_track track_id = do
    blocks <- State.gets (Map.elems . State.state_blocks)
    return $ any ((track_id `elem`) . Block.block_track_ids) blocks

-- | Find rulers which are not found in any block.
orphan_rulers :: (State.M m) => m [RulerId]
orphan_rulers = do
    blocks <- State.gets (Map.elems . State.state_blocks)
    let ref_rulers = Set.fromList (concatMap Block.block_ruler_ids blocks)
    rulers <- State.gets (Set.fromAscList . Map.keys . State.state_rulers)
    return $ Set.toList (rulers `Set.difference` ref_rulers)

-- | Find blocks with no associated views.
orphan_blocks :: (State.M m) => m [BlockId]
orphan_blocks = do
    views <- State.gets (Map.elems . State.state_views)
    let ref_blocks = Set.fromList (map Block.view_block views)
    blocks <- State.gets (Set.fromAscList . Map.keys . State.state_blocks)
    return $ Set.toList (blocks `Set.difference` ref_blocks)

-- | Modify track titles with a function.
--
-- TODO this is inadequate.  I need a function to get parsed inst and control
-- track titles separately.  Use TrackTree.get_track_tree to figure inst vs.
-- control.
map_track_titles :: (State.M m) => (String -> String) -> m ()
map_track_titles f = do
    tracks <- Map.assocs . State.state_tracks <$> State.get
    forM_ tracks $ \(track_id, track) ->
        State.set_track_title track_id (f (Track.track_title track))


-- * block

block_from_template :: (State.M m) => Bool -> BlockId -> m BlockId
block_from_template include_tracks template_id = do
    ruler_id <- State.block_ruler template_id
    block_id <- block ruler_id
    when include_tracks $ do
        template <- State.get_block template_id
        let tracks = drop 1 (Block.block_tracks template)
        forM_ (zip [1..] tracks) $ \(tracknum, track) ->
            case Block.tracklike_id track of
                Block.TId tid rid -> do
                    new_tid <- track_events block_id rid tracknum
                        (Block.track_width track) Track.empty
                    title <- fmap Track.track_title (State.get_track tid)
                    State.set_track_title new_tid title
                _ -> State.insert_track block_id tracknum track
        State.set_skeleton block_id =<< State.get_skeleton template_id
    view block_id
    return block_id

-- | BlockIds look like \"ns/b0\", \"ns/b1\", etc.
block :: (State.M m) => RulerId -> m BlockId
block ruler_id = do
    ns <- State.get_namespace
    blocks <- State.gets State.state_blocks
    block_id <- require "block id" $ generate_block_id ns blocks
    State.create_block block_id ""
        [Block.track (Block.RId ruler_id) Config.ruler_width]

-- | Create a block with the given ID name.  Useful for blocks meant to be
-- sub-derived.  If the name doesn't contain a @/@, it gets the current
-- namespace.
named_block :: (State.M m) => String -> RulerId -> m BlockId
named_block name ruler_id = do
    ns <- State.get_namespace
    case Id.make ns name of
        Nothing -> State.throw $ "invalid block name: " ++ show name
        Just ident -> State.create_block ident ""
            [Block.track (Block.RId ruler_id) Config.ruler_width]


-- | Delete a block and any views it appears in.  Also delete any tracks
-- that only appeared in that block.
destroy_block :: (State.M m) => BlockId -> m ()
destroy_block block_id = do
    track_ids <- fmap Block.block_track_ids (State.get_block block_id)
    State.destroy_block block_id
    orphans <- orphan_tracks
    mapM_ State.destroy_track (filter (`Set.member` orphans) track_ids)

-- ** util

generate_block_id :: Id.Namespace -> Map.Map BlockId _a -> Maybe Id.Id
generate_block_id ns blocks = generate_id ns no_parent "b" Types.BlockId blocks
    where no_parent = Id.global ""

-- * view

view :: (State.M m) => BlockId -> m ViewId
view block_id = do
    views <- State.get_views_of block_id
    view_id <- require "view id" $ generate_view_id block_id views
    rect <- State.gets (find_rect Config.view_size . map Block.view_rect
        . Map.elems . State.state_views)
    State.create_view view_id $ Block.view block_id rect Config.zoom

-- | This is like 'view', but tries to fit the view size to its contents.
--
-- It's in Cmd.M since it needs the screen dimensions.
fitted_view :: (Cmd.M m) => BlockId -> m ViewId
fitted_view block_id = do
    -- This is similar to ViewConfig.resize_to_fit, but not the same.  The
    -- reason is that resize_to_fit relies on Block.view_visible_track/block
    -- being set, which is in turn because the haskell layer doesn't track
    -- the size of the various widgets and so it has to wait for fltk to tell
    -- it after the view has been created.  So instead I add up the default
    -- sizes, which only works before the view has been created since that
    -- pesky user hasn't had a chance to change them yet.
    --
    -- It's gross, but still probably better than tracking a whole bunch of
    -- fltk state that I don't otherwise need.
    views <- State.get_views_of block_id
    view_id <- require "view id" $ generate_view_id block_id views
    block <- State.get_block block_id
    block_end <- State.block_event_end block_id
    let w = sum $ map Block.display_track_width (Block.block_tracks block)
        h = Types.zoom_to_pixels Config.zoom block_end
    rects <- State.gets (map Block.view_rect . Map.elems . State.state_views)
    screen <- Cmd.get_screen (0, 0) -- just pick the main screen for now
    let dimensions = (w + Block.default_track_padding block,
            h + Block.default_time_padding block)
    let vrect = find_screen_rect dimensions rects screen
    State.create_view view_id $ Block.view block_id vrect Config.zoom

block_view :: (Cmd.M m) => RulerId -> m ViewId
block_view ruler_id = block ruler_id >>= view

-- | ViewIds look like \"ns/b0.v0\", \"ns/b0.v1\", etc.
generate_view_id :: BlockId -> Map.Map ViewId _a -> Maybe Id.Id
generate_view_id block_id views =
    generate_id (Id.id_namespace ident) ident "v" Types.ViewId views
    where ident = Id.unpack_id block_id

-- | Destroy a view, along with the underlying block if there were no other
-- views.
destroy_view :: (State.M m) => ViewId -> m ()
destroy_view view_id = do
    block_id <- State.block_id_of view_id
    State.destroy_view view_id
    orphans <- orphan_blocks
    when (block_id `elem` orphans) $
        State.destroy_block block_id

-- * track

-- | Create a track and splice it below the current one.  The track will
-- be inserted to the right of the selected track.
splice_below :: (Cmd.M m) => m TrackId
splice_below = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    track_id <- focused_track block_id (tracknum+1)
    State.splice_skeleton_below block_id (tracknum+1) tracknum
    return track_id

{-
splice_above :: (Cmd.M m) => m TrackId
splice_above = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    track_id <- empty_track block_id tracknum
    State.splice_skeleton_above block_id tracknum (tracknum+1)
    return track_id

-- | Create a track and make it parent to the current one along with its
-- siblings.  If the selected track has no parent, the new track will become
-- parent to all toplevel tracks and be placed at tracknum 1.  Otherwise, it
-- will be inserted to the right of the parent.
splice_above_all :: (Cmd.M m) => m TrackId
splice_above_all = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    tree <- State.get_track_tree block_id
    (_, parents) <- Cmd.require_msg
        ("splice_above: tracknum not in tree: " ++ show tracknum) $
        Tree.find_with_parents ((==tracknum) . num) tree
    let new_tracknum = maybe 1 ((+1) . num . Tree.rootLabel) (Seq.head parents)
    let parent = bump . num . Tree.rootLabel <$> Seq.head parents
        bump n = if n >= new_tracknum then n + 1 else n
    track_id <- empty_track block_id new_tracknum
    -- Splice above means splice below the parent!
    case parent of
        Just parent ->
            State.splice_skeleton_below block_id new_tracknum parent
        Nothing -> do
            -- No parent?  Becomes the parent of all toplevel tracks.
            let toplevel = map ((+1) . num . Tree.rootLabel) tree
            State.add_edges block_id (map ((,) new_tracknum) toplevel)
    return track_id
    where num = State.track_tracknum
-}

-- | Get the ancestors (topmost parents) of the selected tracks and create
-- a parent track to them.  It will be inserted to the left of the leftmost
-- ancestor.
--
-- I don't think I need @splice_above@ because I can do the same with
-- 'splice_below', unless it's the leftmost track, in which case it probably is
-- also an ancestor, so 'splice_above_ancestors' will work.
splice_above_ancestors :: (Cmd.M m) => m TrackId
splice_above_ancestors = do
    (block_id, tracknums, _, _, _) <- Selection.tracks
    tree <- TrackTree.get_track_tree block_id
    let ancestors = Seq.unique $ mapMaybe (ancestor tree) tracknums
    insert_at <- Cmd.require_msg "no selected tracks" $ Seq.minimum ancestors
    track_id <- focused_track block_id insert_at
    State.add_edges block_id (map ((,) insert_at) (map (+1) ancestors))
    return track_id
    where
    ancestor tree tracknum = case List.find find (Tree.flat_paths tree) of
            Nothing -> Nothing
            Just (track, parents, _) ->
                Just $ State.track_tracknum $ last (track : parents)
        where find (track, _, _) = State.track_tracknum track == tracknum

-- | Insert a track after the selection, or just append one if there isn't one.
-- This is useful for empty blocks which of course have no selection.
insert_track :: (Cmd.M m) => m TrackId
insert_track = maybe append_track (const insert_track_after_selection)
    =<< Selection.lookup_insert

append_track :: (Cmd.M m) => m TrackId
append_track = do
    block_id <- Cmd.get_focused_block
    focused_track block_id 99999

insert_track_after_selection :: (Cmd.M m) => m TrackId
insert_track_after_selection = do
    (_, (block_id, tracknum, _)) <- Selection.get_any_insert
    focused_track block_id (tracknum + 1)

-- | Add a new track, give keyboard focus to the title, and scroll the view to
-- make sure it's visible.
focused_track :: (Cmd.M m) => BlockId -> TrackNum -> m TrackId
focused_track block_id tracknum = do
    -- This " " is a hack to tell fltk to set keyboard focus.
    track_id <- track block_id tracknum " " Events.empty
    block <- State.get_block =<< Cmd.get_focused_block
    view_id <- Cmd.get_focused_view
    view <- State.get_view view_id
    State.set_track_scroll view_id $ Selection.auto_track_scroll block view
        (Types.point_selection tracknum 0)
    return track_id

empty_track :: (State.M m) => BlockId -> TrackNum -> m TrackId
empty_track block_id tracknum = track block_id tracknum "" Events.empty

-- | Like 'track_events', but copy the ruler from the track to the left.
--
-- If the track to the left is a ruler track, it will assume there is
-- a ".overlay" version of it.
track :: (State.M m) => BlockId -> TrackNum -> String -> Events.Events
    -> m TrackId
track block_id tracknum title events = do
    -- Clip to valid range so callers can use an out of range tracknum.
    tracknum <- clip_tracknum block_id tracknum
    ruler_id <- get_overlay_ruler_id block_id (tracknum-1)
    track_events block_id ruler_id tracknum Config.track_width
        (Track.track title events)

-- | Lowest level track creator.
track_events :: (State.M m) =>
    BlockId -> RulerId -> TrackNum -> Types.Width -> Track.Track -> m TrackId
track_events block_id ruler_id tracknum width track = do
    tracks <- State.gets State.state_tracks
    track_id <- require "track id" $ generate_track_id block_id "t" tracks
    tid <- State.create_track track_id track
    State.insert_track block_id tracknum
        (Block.track (Block.TId tid ruler_id) width)
    return tid

-- | Create a track with the given name, in the same namespace as the BlockId.
named_track :: (State.M m) =>
    BlockId -> RulerId -> TrackNum -> String -> Track.Track -> m TrackId
named_track block_id ruler_id tracknum name track = do
    ident <- make_id (Id.ident_name block_id ++ "." ++ name)
    all_tracks <- State.gets State.state_tracks
    when (Types.TrackId ident `Map.member` all_tracks) $
        State.throw $ "track " ++ show ident ++ " already exists"
    tid <- State.create_track ident track
    State.insert_track block_id tracknum
        (Block.track (Block.TId tid ruler_id) Config.track_width)
    return tid

remove_selected_tracks :: (Cmd.M m) => m ()
remove_selected_tracks = do
    (block_id, tracknums, _, _, _) <- Selection.tracks
    mapM_ (State.remove_track block_id) (reverse tracknums)

destroy_selected_tracks :: (Cmd.M m) => m ()
destroy_selected_tracks = do
    (block_id, tracknums, _, _, _) <- Selection.tracks
    -- Deleting each track will decrease the tracknum of the ones after it.
    mapM_ (destroy_track block_id) (zipWith (-) tracknums [0..])

-- | Remove a track from a block.  If that was the only block it appeared in,
-- delete the underlying track.  Rulers are never deleted automatically.
destroy_track :: (State.M m) => BlockId -> TrackNum -> m ()
destroy_track block_id tracknum = do
    tracklike <- State.require ("invalid tracknum: " ++ show tracknum)
        =<< State.track_at block_id tracknum
    State.remove_track block_id tracknum
    when_just (Block.track_id_of tracklike) $ \track_id ->
        whenM (orphan_track track_id) $
            State.destroy_track track_id

-- | Swap the tracks at the given tracknums.  If one of the tracknums is out
-- of range, the track at the other tracknum will be moved to the beginning or
-- end, i.e. swapped with empty space.
swap_tracks :: (State.M m) => BlockId -> TrackNum -> TrackNum -> m ()
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
get_overlay_ruler_id :: (State.M m) => BlockId -> TrackNum -> m RulerId
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

add_overlay_suffix :: RulerId -> RulerId
add_overlay_suffix ruler_id
    | overlay_suffix `List.isSuffixOf` ident = ruler_id
    | otherwise = Types.RulerId (Id.unsafe_id ns (ident ++ overlay_suffix))
    where (ns, ident) = Id.un_id (Id.unpack_id ruler_id)

-- | Clip the tracknum to within the valid range.
clip_tracknum :: (State.M m) => BlockId -> TrackNum -> m TrackNum
clip_tracknum block_id tracknum = do
    tracks <- State.track_count block_id
    return $ max 0 (min tracks tracknum)

-- | Get the track to the right of the given tracknum.  This isn't just (+1)
-- because it skips collapsed tracks.
--
-- TODO Currently unused.  At one time I thought newly added tracks should
-- skip collapsed ones, but now I don't think so.
track_after :: Block.Block -> TrackNum -> TrackNum
track_after block tracknum
    -- It must already be the rightmost tracknum.
    | tracknum == next_tracknum = length (Block.block_tracks block)
    | otherwise = next_tracknum
    where next_tracknum = State.shift_tracknum block tracknum 1

generate_track_id :: BlockId -> String -> Map.Map TrackId _a -> Maybe Id.Id
generate_track_id block_id code tracks =
    generate_id (Id.id_namespace ident) ident code Types.TrackId tracks
    where ident = Id.unpack_id block_id

-- * ruler

-- | This creates both a ruler with the given name, and an overlay version
-- named with .overlay.
ruler :: (State.M m) => String -> Ruler.Ruler -> m (RulerId, RulerId)
ruler name ruler = do
    ident <- make_id name
    overlay_ident <- make_id (name ++ overlay_suffix)
    rid <- State.create_ruler ident ruler
    over_rid <- State.create_ruler overlay_ident (MakeRuler.as_overlay ruler)
    return (rid, over_rid)

-- | Set a block to a new ruler.
new_ruler :: (State.M m) => BlockId -> String -> Ruler.Ruler -> m RulerId
new_ruler block_id name r = do
    (ruler_id, overlay_id) <- ruler name r
    set_block_ruler ruler_id overlay_id block_id
    return ruler_id

set_block_ruler :: (State.M m) => RulerId -> RulerId -> BlockId -> m ()
set_block_ruler ruler_id overlay_id block_id = Transform.tracks block_id set
    where
    set (Block.TId tid _) = Block.TId tid overlay_id
    set (Block.RId _) = Block.RId ruler_id
    set t = t

-- ** util

make_id :: (State.M m) => String -> m Id.Id
make_id name = do
    ns <- State.get_namespace
    State.require ("make_id: invalid name: " ++ show name) $ Id.id ns name

-- | An overlay versions of a ruler has id ruler_id ++ suffix.
overlay_suffix :: String
overlay_suffix = ".overlay"

-- * general util

generate_id :: (Ord a) => Id.Namespace -> Id.Id -> String -> (Id.Id -> a)
    -> Map.Map a _b -> Maybe Id.Id
generate_id ns parent_id code typ fm =
    List.find (not . (`Map.member` fm) . typ) candidates
    where candidates = ids_for ns (Id.id_name parent_id) code

-- | IDs are numbered, and they start at 1 instead of 0.
--
-- This is because usually tracknum 0 is the ruler, so counting with tracknums,
-- event tracks start at 1.  The actual TrackId should be irrelevant (and would
-- be out of date as soon as a track is swapped), but for testing it's very
-- convenient if they line up with the tracknums.  So even though it's purely
-- for testing and only for TrackIds, I start everything at 1 just for
-- consistency.
ids_for :: Id.Namespace -> String -> String -> [Id.Id]
ids_for ns parent code =
    [Id.unsafe_id ns (dotted parent ++ code ++ padded n) | n <- [1..]]
    where
    dotted s = if null s then "" else s ++ "."
    -- Add a leading 0 so TrackIds will sort by their tracknum.  This is
    -- a brittle hack because TrackIds aren't guaranteed to be anything, but
    -- hopefully I can get rid of TrackIds someday and use TrackNums.
    padded n = let s = show n in if length s < 2 then '0' : s else s

require :: (State.M m) => String -> Maybe a -> m a
require msg = maybe (State.throw $ "somehow can't find ID for " ++ msg) return

find_rect :: (Int, Int) -> [Rect.Rect] -> Rect.Rect
find_rect (w, h) rects = Rect.xywh right bottom w h
    where
    right = maximum $ 0 : map Rect.rr rects
    bottom = 10

find_screen_rect :: (Int, Int) -> [Rect.Rect] -> Rect.Rect -> Rect.Rect
find_screen_rect (w, h) rects screen =
    Rect.intersection screen (Rect.xywh right bottom w h)
    where
    right = min (maximum (0 : map Rect.rr rects) ) (Rect.rr screen - w)
    bottom = 10
