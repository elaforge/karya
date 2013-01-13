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
import qualified Control.Monad.State.Strict as Monad.State
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

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
import qualified Cmd.Selection as Selection
import qualified Cmd.ViewConfig as ViewConfig

import qualified App.Config as Config
import Types


-- * global modifications

-- | Set the project to the given value and renamespace the old project to the
-- new one.  The 'State.state_project_dir' is not modified, so it will keep
-- saving to the old save file.
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

block_name :: (State.M m) => m String
block_name = do
    ns <- State.get_namespace
    id <- require "block id" . generate_block_id ns
        =<< State.gets State.state_blocks
    return $ Id.id_name id

block_from_template :: (State.M m) => BlockId -> m BlockId
block_from_template template_id =
    named_block_from_template template_id =<< block_name

named_block_from_template :: (State.M m) => BlockId -> String -> m BlockId
named_block_from_template template_id name = do
    ruler_id <- State.block_ruler template_id
    block_id <- named_block name ruler_id
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
    return block_id

-- | BlockIds look like \"ns/b0\", \"ns/b1\", etc.
block :: (State.M m) => RulerId -> m BlockId
block ruler_id = do
    ns <- State.get_namespace
    block_id <- require "block id"
        . generate_block_id ns =<< State.gets State.state_blocks
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

-- | Create a view with the default dimensions.
unfitted_view :: (State.M m) => BlockId -> m ViewId
unfitted_view block_id = do
    block <- State.get_block block_id
    view_id <- require "view id"
        . generate_view_id block_id =<< State.gets State.state_views
    rect <- State.gets $ find_rect Config.view_size . map Block.view_rect
        . Map.elems . State.state_views
    State.create_view view_id $ Block.view block block_id rect Config.zoom

-- | This is like 'unfitted_view', but tries to fit the view size to its
-- contents.
--
-- It's in Cmd.M since it needs the screen dimensions.
--
-- Views created during setup are likely to not have the correct height.
-- That's because I haven't received the screen resolution from fltk yet so
-- I make a guess in 'Cmd.get_screen'.
view :: (Cmd.M m) => BlockId -> m ViewId
view block_id = do
    view_id <- unfitted_view block_id
    ViewConfig.resize_to_fit False view_id
    return view_id

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
    -- I want to add a track to the right of the selected track.  Taking the
    -- maximum means I should splice after a merged pitch track, if there is
    -- one.
    (block_id, sel_tracknums, _, _, _) <- Selection.tracks
    let sel_tracknum = maximum (1 : sel_tracknums)
    block <- State.get_block block_id
    let tracknum = track_after block sel_tracknum
    track_id <- focused_track block_id tracknum
    State.splice_skeleton_below block_id tracknum sel_tracknum
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
    State.add_edges block_id (map ((,) insert_at . (+1)) ancestors)
    return track_id
    where
    ancestor tree tracknum = case List.find find (Tree.flat_paths tree) of
            Nothing -> Nothing
            Just (track, parents, _) ->
                Just $ State.track_tracknum $ last (track : parents)
        where find (track, _, _) = State.track_tracknum track == tracknum

append_tracks_from_template :: (Cmd.M m) => m ()
append_tracks_from_template = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    append_tracks_from_template_at block_id tracknum

-- | Insert tracks using the given one and its children as a template.
-- If the source track has a parent, the new tracks are spliced below its
-- rightmost child, otherwise they are appended.
append_tracks_from_template_at :: (Cmd.M m) => BlockId -> TrackNum -> m ()
append_tracks_from_template_at block_id source = do
    tree <- TrackTree.get_track_tree block_id
    case Tree.find_with_parents ((==source) . State.track_tracknum) tree of
        Nothing -> Cmd.throw $ "selected track doesn't exist: "
            ++ show (block_id, source)
        Just (track, parents)
            -- If there's a parent, find it's right most child.
            | Tree.Node parent siblings : _ <- parents -> do
                let at = rightmost siblings + 1
                append_below at track
                State.add_edges block_id [(State.track_tracknum parent, at)]
            -- Otherwise, take the rightmost of the block.
            | otherwise -> append_below (rightmost tree + 1) track
    where
    -- Starting at tracknum, insert track and its children.
    append_below tracknum track_node = do
        forM_ tracks $ \(n, title) -> track block_id n title mempty
        State.add_edges block_id skel
        where
        (tracks, skel) = make_tracks tracknum [track_node]
    rightmost :: TrackTree.TrackTree -> Int
    rightmost = List.foldl' max 0
        . map (Foldable.foldl' (\x -> max x . State.track_tracknum) 0)

make_tracks :: TrackNum -> TrackTree.TrackTree
    -> ([(TrackNum, String)], [(TrackNum, TrackNum)])
make_tracks tracknum tree =
    (concatMap Tree.flatten tracks, Tree.edges (map (fmap fst) tracks))
    where tracks = assign_tracknums tracknum tree

-- | Assign ascending tracknums to the given tree in depth-first order.  Return
-- (tracknum, title) pairs.
assign_tracknums :: TrackNum -> TrackTree.TrackTree
    -> [Tree.Tree (TrackNum, String)]
assign_tracknums tracknum tree =
    Monad.State.evalState (mapM assign tree) tracknum
    where
    assign (Tree.Node track children) = do
        tracknum <- next
        children <- mapM assign children
        return $ Tree.Node (tracknum, State.track_title track) children
        where next = Monad.State.get <* Monad.State.modify (+1)

-- | Insert a track after the selection, or just append one if there isn't one.
-- This is useful for empty blocks which of course have no selection.
insert_track :: (Cmd.M m) => m TrackId
insert_track = do
    sel <- Selection.lookup_any_insert
    case sel of
        Nothing -> append_track
        Just (_, (block_id, tracknum, _)) -> do
            block <- State.get_block block_id
            focused_track block_id (track_after block tracknum)

append_track :: (Cmd.M m) => m TrackId
append_track = do
    block_id <- Cmd.get_focused_block
    focused_track block_id 99999

-- | Add a new track, give keyboard focus to the title, and embiggen the view
-- to make sure it's visible.
focused_track :: (Cmd.M m) => BlockId -> TrackNum -> m TrackId
focused_track block_id tracknum = do
    -- This " " is a hack to tell fltk to set keyboard focus.
    track_id <- track block_id tracknum " " Events.empty
    view_id <- Cmd.get_focused_view
    view <- State.get_view view_id
    embiggened <- ViewConfig.contents_rect view
    let rect = Block.view_visible_rect view
    when (Rect.rw embiggened > Rect.rw rect) $
        State.set_view_rect view_id $ Block.set_visible_rect view $
            Rect.resize (Rect.rw embiggened) (Rect.rh rect) rect
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
    ruler_id <- State.ruler_track_at block_id (max 0 (tracknum-1))
    track_events block_id ruler_id tracknum Config.track_width
        (Track.track title events)

-- | Lowest level track creator.
track_events :: (State.M m) =>
    BlockId -> RulerId -> TrackNum -> Types.Width -> Track.Track -> m TrackId
track_events block_id ruler_id tracknum width track = do
    track_id <- require "track id"
        . generate_track_id block_id "t" =<< State.gets State.state_tracks
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

-- | Clip the tracknum to within the valid range.
clip_tracknum :: (State.M m) => BlockId -> TrackNum -> m TrackNum
clip_tracknum block_id tracknum = do
    tracks <- State.track_count block_id
    return $ max 0 (min tracks tracknum)

-- | Get the track to the right of the given tracknum.  This isn't just (+1)
-- because it skips collapsed tracks.
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

-- | Create a ruler with the given name.
ruler :: (State.M m) => String -> Ruler.Ruler -> m RulerId
ruler name ruler = do
    ident <- make_id name
    State.create_ruler ident ruler

-- | Set a block to a new ruler.
new_ruler :: (State.M m) => BlockId -> String -> Ruler.Ruler -> m RulerId
new_ruler block_id name r = do
    ruler_id <- ruler name r
    set_block_ruler ruler_id block_id
    return ruler_id

set_block_ruler :: (State.M m) => RulerId -> BlockId -> m ()
set_block_ruler ruler_id block_id =
    Transform.tracks block_id (Block.set_ruler_id ruler_id)

-- ** util

make_id :: (State.M m) => String -> m Id.Id
make_id name = do
    ns <- State.get_namespace
    State.require ("make_id: invalid name: " ++ show name) $ Id.id ns name

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
    [Id.unsafe_id ns (dotted parent ++ code ++ show n) | n <- [1..]]
    where dotted s = if null s then "" else s ++ "."

require :: (State.M m) => String -> Maybe a -> m a
require msg = maybe (State.throw $ "somehow can't find ID for " ++ msg) return

find_rect :: (Int, Int) -> [Rect.Rect] -> Rect.Rect
find_rect (w, h) rects = Rect.xywh right bottom w h
    where
    right = maximum $ 0 : map Rect.rr rects
    -- TODO This is an OSX specific hack: the main screen has a title bar and
    -- prevents you from creating a window on it.  If the view rect doesn't
    -- have the right y value then it can think its height is bigger than it
    -- will actually be, and winds up being too short.
    bottom = 44
