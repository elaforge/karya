-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import qualified Util.Ranges as Ranges
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Util.Trees as Trees

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection
import qualified Cmd.Views as Views

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.GenId as GenId
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Transform as Transform
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import           Global
import           Types


-- * global modifications

-- | Set the project to the given value and renamespace the old project to the
-- new one.  'Cmd.state_save_file' is not modified, so it will keep saving to
-- the old save file.
rename_project :: Ui.M m => Id.Namespace -> m ()
rename_project ns = do
    old_ns <- Ui.get_namespace
    renamespace old_ns ns
    Ui.set_namespace ns

-- | Rename all IDs in namespace @from@ to @to@.
renamespace :: Ui.M m => Id.Namespace -> Id.Namespace -> m ()
renamespace from to = Transform.map_namespace $ \ns ->
    if ns == from then to else ns

rename_ruler :: Ui.M m => RulerId -> RulerId -> m ()
rename_ruler ruler_id new_name = Transform.map_ruler_ids $ \id ->
    if Id.RulerId id == ruler_id then Id.unpack_id new_name else id

-- | Rename multiple RulerIds at once.  This can swap two IDs without
-- colliding.
rename_rulers :: Ui.M m => [(RulerId, Id.Id)] -> m ()
rename_rulers pairs = Transform.map_ruler_ids $ \id ->
    fromMaybe id $ lookup (Id.RulerId id) pairs

rename_block :: Ui.M m => BlockId -> Id.Id -> m ()
rename_block block_id new_name = Transform.map_block_ids $ \id ->
    if Id.BlockId id == block_id then new_name else id

copy_block :: Ui.M m => BlockId -> Id.Id -> m ()
copy_block block_id new_name = do
    from <- Ui.get_block block_id
    void $ Ui.create_config_block new_name from

-- | Find tracks which are not found in any block.  Probably used to pass them
-- to Ui.destroy_track for \"gc\".
orphan_tracks :: Ui.M m => m (Set TrackId)
orphan_tracks = do
    blocks <- Ui.gets (Map.elems . Ui.state_blocks)
    let ref_tracks = Set.fromList (concatMap Block.block_track_ids blocks)
    tracks <- Ui.gets (Set.fromAscList . Map.keys . Ui.state_tracks)
    return $ tracks `Set.difference` ref_tracks

-- | Like 'orphan_tracks' but more efficiently check if a single track is an
-- orphan.
orphan_track :: Ui.M m => TrackId -> m Bool
orphan_track track_id = do
    blocks <- Ui.gets (Map.elems . Ui.state_blocks)
    return $ any ((track_id `elem`) . Block.block_track_ids) blocks

-- | Find rulers which are not found in any block.
orphan_rulers :: Ui.M m => m [RulerId]
orphan_rulers = do
    blocks <- Ui.gets (Map.elems . Ui.state_blocks)
    let ref_rulers = Set.fromList (concatMap Block.block_ruler_ids blocks)
    rulers <- Ui.gets (Set.fromAscList . Map.keys . Ui.state_rulers)
    return $ Set.toList (rulers `Set.difference` ref_rulers)

-- | Find blocks with no associated views.
orphan_blocks :: Ui.M m => m [BlockId]
orphan_blocks = do
    views <- Ui.gets (Map.elems . Ui.state_views)
    let ref_blocks = Set.fromList (map Block.view_block views)
    blocks <- Ui.gets (Set.fromAscList . Map.keys . Ui.state_blocks)
    return $ Set.toList (blocks `Set.difference` ref_blocks)

-- | Modify track titles with a function.
--
-- TODO this is inadequate.  I need a function to get parsed inst and control
-- track titles separately.  Use TrackTree.track_tree_of to figure inst vs.
-- control.
map_track_titles :: Ui.M m => (Text -> Text) -> m ()
map_track_titles f = do
    tracks <- Map.assocs . Ui.state_tracks <$> Ui.get
    forM_ tracks $ \(track_id, track) ->
        Ui.set_track_title track_id (f (Track.track_title track))


-- * block

block_from_template :: Ui.M m => Bool -> BlockId -> m BlockId
block_from_template copy_events template_id =
    named_block_from_template copy_events template_id =<< GenId.block_id Nothing

-- | Create a block which is a copy of another.
named_block_from_template :: Ui.M m => Bool -- ^ copy the events
    -> BlockId -> Id.Id -> m BlockId
named_block_from_template copy_events template_id name = do
    ruler_id <- Ui.block_ruler template_id
    block_id <- named_block name ruler_id
    template <- Ui.get_block template_id
    Ui.set_block_title block_id (Block.block_title template)
    let tracks = drop 1 (Block.block_tracks template)
    new_tids <- mapMaybeM (add_track block_id) (zip [1..] tracks)
    -- Copy over the flags and merge status.  I have to merge the analogous
    -- tracks in the new block.
    let old_to_new = Map.fromList $ zip
            (Block.track_ids_of (map Block.tracklike_id tracks))
            new_tids
    forM_ (zip [1..] tracks) $ \(tracknum, btrack) -> do
        Ui.modify_track_flags block_id tracknum
            (const (Block.track_flags btrack))
        Ui.set_merged_tracks block_id tracknum $
            Set.fromList $ mapMaybe (`Map.lookup` old_to_new) $ Set.toList $
            Block.track_merged btrack
    whenM (Ui.has_explicit_skeleton block_id) $
        Ui.set_skeleton block_id =<< Ui.get_skeleton template_id
    return block_id

    where
    add_track block_id (tracknum, btrack) = case Block.tracklike_id btrack of
        Block.TId tid rid -> do
            track <- Ui.get_track tid
            Just <$> track_events block_id rid tracknum
                (Block.track_width btrack)
                (Track.track (Track.track_title track)
                    (if copy_events then Track.track_events track else mempty))
        _ -> do
            Ui.insert_track block_id tracknum btrack
            return Nothing

-- | BlockIds look like @ns\/b1@, @ns\/b2@, etc.
block :: Ui.M m => RulerId -> m BlockId
block = sub_block Nothing

-- | Create a block whose BlockId is prefixed by another: @ns/parent.b1@.
-- The relative block call mechanism supported by the default block call means
-- you can call it from the parent by just writing @.b1@.
sub_block :: Ui.M m => Maybe BlockId -> RulerId -> m BlockId
sub_block maybe_parent ruler_id = do
    block_id <- GenId.block_id maybe_parent
    Ui.create_block block_id ""
        [Block.track (Block.RId ruler_id) Config.ruler_width]

-- | Create a block with the given ID name.  Useful for blocks meant to be
-- sub-derived.  If the name doesn't contain a @\/@, it gets the current
-- namespace.
named_block :: Ui.M m => Id.Id -> RulerId -> m BlockId
named_block name ruler_id = Ui.create_block name ""
    [Block.track (Block.RId ruler_id) Config.ruler_width]

-- | Delete a block and any views it appears in.  Also delete any tracks
-- that only appeared in that block.
destroy_block :: Ui.M m => BlockId -> m ()
destroy_block block_id = do
    block <- Ui.get_block block_id
    let track_ids = Block.block_track_ids block
        ruler_ids = Block.block_ruler_ids block
    Ui.destroy_block block_id
    orphans <- orphan_tracks
    mapM_ Ui.destroy_track (filter (`Set.member` orphans) track_ids)
    orphans <- orphan_rulers
    mapM_ Ui.destroy_ruler (filter (`elem` orphans) ruler_ids)

-- * view

-- | Create a view with the default dimensions.
unfitted_view :: Ui.M m => BlockId -> m ViewId
unfitted_view block_id = do
    (x, y) <- Ui.gets $ find_rect Nothing Config.view_size
        . map Block.view_rect . Map.elems . Ui.state_views
    let (w, h) = Config.view_size
    sized_view block_id (Rect.xywh x y w h)

sized_view :: Ui.M m => BlockId -> Rect.Rect -> m ViewId
sized_view block_id rect = do
    view_id <- GenId.view_id block_id
    block <- Ui.get_block block_id
    view_id <- Ui.create_view view_id $
        Block.view block block_id rect Config.zoom
    -- Automatically set the selection on a new view, so it has focus.
    let maybe_tracknum = case Block.block_tracks block of
            [] -> Nothing
            [_] -> Just 0
            _ -> Just 1
    whenJust maybe_tracknum $ \tracknum ->
        Ui.set_selection view_id Config.insert_selnum $
            Just $ Sel.point tracknum 0 Sel.Positive
            -- TODO I should use the current orientation, but that would make
            -- this and all its callers dependent on Cmd, or need another arg.
    return view_id

-- | This is like 'unfitted_view', but tries to fit the view size to its
-- contents.
--
-- It's in Cmd.M since it needs the screen dimensions.
--
-- Views created during setup are likely to not have the correct height.
-- That's because I haven't received the screen resolution from fltk yet so
-- I make a guess in 'Cmd.get_screen'.
view :: Cmd.M m => BlockId -> m ViewId
view block_id = do
    view_id <- unfitted_view block_id
    Views.maximize_and_zoom view_id
    screen <- maybe (Cmd.get_screen Nothing) view_screen
        =<< Cmd.lookup_focused_view
    rect <- Block.view_rect <$> Ui.get_view view_id
    others <- Ui.gets $ filter (\r -> r /= rect && Rect.overlaps r screen)
        . map Block.view_rect . Map.elems . Ui.state_views
    let (x, y) = find_rect (Just screen) (Rect.w rect, Rect.h rect) others
    Ui.set_view_rect view_id (Rect.place x y rect)
    return view_id

-- | Create a view, or focus on it if it already exists.
view_or_focus :: Cmd.M m => BlockId -> m (Maybe ViewId)
    -- ^ Just ViewId if a new one was created.
view_or_focus block_id = do
    views <- Ui.views_of block_id
    maybe (Just <$> view block_id) (\vid -> Cmd.focus vid *> return Nothing)
        (Seq.head (Map.keys views))

view_screen :: Cmd.M m => ViewId -> m Rect.Rect
view_screen view_id =
    Cmd.get_screen . Just . Rect.upper_left . Block.view_rect
        =<< Ui.get_view view_id

block_view :: Cmd.M m => RulerId -> m ViewId
block_view ruler_id = block ruler_id >>= view

-- | Destroy a view, along with the underlying block if there were no other
-- views.
destroy_view :: Ui.M m => ViewId -> m ()
destroy_view view_id = do
    block_id <- Ui.block_id_of view_id
    Ui.destroy_view view_id
    orphans <- orphan_blocks
    when (block_id `elem` orphans) $
        Ui.destroy_block block_id

-- * track

-- | Create a track and splice it below the current one.  The track will
-- be inserted to the right of the selected track.
splice_below :: Cmd.M m => m TrackId
splice_below = do
    -- I want to add a track to the right of the selected track.  Taking the
    -- maximum means I should splice after a merged pitch track, if there is
    -- one.
    (block_id, sel_tracknums, _, _) <- Selection.tracks
    let sel_tracknum = maximum (1 : sel_tracknums)
    block <- Ui.get_block block_id
    let tracknum = track_after block sel_tracknum
    track_id <- focused_track tracknum
    whenM (Ui.has_explicit_skeleton block_id) $
        -- If you create from track 0, it'll be (1, 1) here.
        when (tracknum /= sel_tracknum) $
            Ui.splice_skeleton_below block_id tracknum sel_tracknum
    return track_id

splice_above :: Cmd.M m => m TrackId
splice_above = do
    -- This doesn't need to avoid a merged track like 'splice_below', because
    -- it inserts to the left.
    (block_id, tracknum, _, _) <- Selection.get_insert
    track_id <- focused_track tracknum
    whenM (Ui.has_explicit_skeleton block_id) $
        Ui.splice_skeleton_above block_id tracknum (tracknum+1)
    return track_id

{-
-- | Create a track and make it parent to the current one along with its
-- siblings.  If the selected track has no parent, the new track will become
-- parent to all toplevel tracks and be placed at tracknum 1.  Otherwise, it
-- will be inserted to the right of the parent.
splice_above_all :: Cmd.M m => m TrackId
splice_above_all = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    tree <- Ui.track_tree_of block_id
    (_, parents) <- Cmd.require
        ("splice_above: tracknum not in tree: " ++ show tracknum) $
        Trees.findWithParents ((==tracknum) . num) tree
    let new_tracknum = maybe 1 ((+1) . num . Tree.rootLabel) (Seq.head parents)
    let parent = bump . num . Tree.rootLabel <$> Seq.head parents
        bump n = if n >= new_tracknum then n + 1 else n
    track_id <- empty_track block_id new_tracknum
    -- Splice above means splice below the parent!
    case parent of
        Just parent ->
            Ui.splice_skeleton_below block_id new_tracknum parent
        Nothing -> do
            -- No parent?  Becomes the parent of all toplevel tracks.
            let toplevel = map ((+1) . num . Tree.rootLabel) tree
            Ui.add_edges block_id (map ((,) new_tracknum) toplevel)
    return track_id
    where num = Ui.track_tracknum
-}

-- | Get the ancestors (topmost parents) of the selected tracks and create
-- a parent track to them.  It will be inserted to the left of the leftmost
-- ancestor.
splice_above_ancestors :: Cmd.M m => m TrackId
splice_above_ancestors = do
    (block_id, tracknums, _, _) <- Selection.tracks
    tree <- TrackTree.track_tree_of block_id
    let ancestors = Seq.unique $ mapMaybe (ancestor tree) tracknums
    insert_at <- Cmd.require "no selected tracks" $ Seq.minimum ancestors
    track_id <- focused_track insert_at
    whenM (Ui.has_explicit_skeleton block_id) $
        Ui.add_edges block_id (map ((,) insert_at . (+1)) ancestors)
    return track_id
    where
    ancestor tree tracknum = case List.find find (Trees.flatPaths tree) of
            Nothing -> Nothing
            Just (track, parents, _) ->
                Just $ Ui.track_tracknum $ last (track : parents)
        where find (track, _, _) = Ui.track_tracknum track == tracknum

insert_branch :: Cmd.M m => m ()
insert_branch = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    insert_branch_from block_id tracknum
    widen =<< Cmd.get_focused_view

-- | Insert tracks using the given one and its children as a template.  The
-- effect is to copy the branch below the selection.
insert_branch_from :: Cmd.M m => BlockId -> TrackNum -> m ()
insert_branch_from block_id source = do
    tree <- TrackTree.track_tree_of block_id
    (track, parents) <- Cmd.require ("not found: "<>pretty (block_id, source)) $
        Trees.findWithParents ((==source) . Ui.track_tracknum) tree
    let right = maximum (Ui.track_tracknum <$> track) + 1
    merged <- Ui.track_merged block_id source
    append_below merged right track
    whenM (Ui.has_explicit_skeleton block_id) $
        whenJust (Seq.head parents) $ \(Tree.Node parent _) ->
            Ui.add_edges block_id [(Ui.track_tracknum parent, right)]
    where
    -- Starting at tracknum, insert track and its children.
    append_below merged tracknum track_node = do
        forM_ tracks $ \(n, title) -> track block_id n title mempty
        whenM (Ui.has_explicit_skeleton block_id) $
            Ui.add_edges block_id skel
        -- Technically it's not necessarily merged with its neighbor, but
        -- that's how the usual cmds work, so I'll assume it's true.
        when merged $ Ui.merge_track block_id tracknum (tracknum+1)
        where (tracks, skel) = make_tracks tracknum [track_node]

make_tracks :: TrackNum -> TrackTree.TrackTree
    -> ([(TrackNum, Text)], [(TrackNum, TrackNum)])
make_tracks tracknum tree =
    (concatMap Tree.flatten tracks, Trees.edges (map (fmap fst) tracks))
    where tracks = assign_tracknums tracknum tree

-- | Assign ascending tracknums to the given tree in depth-first order.  Return
-- (tracknum, title) pairs.
assign_tracknums :: TrackNum -> TrackTree.TrackTree
    -> [Tree.Tree (TrackNum, Text)]
assign_tracknums tracknum tree =
    Monad.State.evalState (mapM assign tree) tracknum
    where
    assign (Tree.Node track children) = do
        tracknum <- next
        children <- mapM assign children
        return $ Tree.Node (tracknum, Ui.track_title track) children
        where next = Monad.State.get <* Monad.State.modify (+1)

-- | Insert a track after the selection, or just append one if there isn't one.
-- This is useful for empty blocks which of course have no selection.
insert_track_right :: Cmd.M m => m TrackId
insert_track_right = do
    sel <- Selection.lookup_any_insert
    case sel of
        Nothing -> append_track
        Just (_, (block_id, tracknum, _)) -> do
            block <- Ui.get_block block_id
            focused_track (track_after block tracknum)

append_track :: Cmd.M m => m TrackId
append_track = focused_track 99999

-- | Add a new track, and widen the view to make sure it's visible.  Give
-- keyboard focus to the title.
focused_track :: Cmd.M m => TrackNum -> m TrackId
focused_track tracknum = do
    view_id <- Cmd.get_focused_view
    track_and_widen True view_id tracknum

track_and_widen :: Ui.M m => Bool -> ViewId -> TrackNum -> m TrackId
track_and_widen focus view_id tracknum = do
    block_id <- Ui.block_id_of view_id
    track_id <- track block_id tracknum "" Events.empty
    widen view_id
    tracknum <- clip_tracknum block_id tracknum
    when focus $ Ui.damage $
        mempty { Update._title_focus = Just (view_id, Just tracknum) }
    return track_id

-- | Expand the view horizontally to to fit all tracks.
widen :: Ui.M m => ViewId -> m ()
widen view_id = do
    view <- Ui.get_view view_id
    embiggened <- Views.contents_rect view
    let rect = Block.view_rect view
    when (Rect.w embiggened > Rect.w rect) $
        Ui.set_view_rect view_id $
            Rect.resize (Rect.w embiggened) (Rect.h rect) rect

empty_track :: Ui.M m => BlockId -> TrackNum -> m TrackId
empty_track block_id tracknum = track block_id tracknum "" Events.empty

-- | Like 'track_events', but copy the ruler from the track to the left.
track :: Ui.M m => BlockId -> TrackNum -> Text -> Events.Events
    -> m TrackId
track block_id tracknum title events = do
    -- Clip to valid range so callers can use an out of range tracknum.
    tracknum <- clip_tracknum block_id tracknum
    ruler_id <- find_ruler (tracknum-1)
    track_events block_id ruler_id tracknum Config.track_width
        (Track.track title events)
    where
    find_ruler tracknum
        | tracknum < 0 = return Ui.no_ruler
        | otherwise = maybe (find_ruler (tracknum-1)) return
            =<< Ui.ruler_track_at block_id tracknum

-- | Lowest level track creator.  The new TrackId will be in the same namespace
-- as the given BlockId.
track_events :: Ui.M m =>
    BlockId -> RulerId -> TrackNum -> Types.Width -> Track.Track -> m TrackId
track_events block_id ruler_id tracknum width track = do
    track_id <- GenId.track_id block_id
    tid <- Ui.create_track track_id track
    Ui.insert_track block_id tracknum
        (Block.track (Block.TId tid ruler_id) width)
    return tid

-- | Create a track with the given name, in the same namespace as the BlockId.
named_track :: Ui.M m =>
    BlockId -> RulerId -> TrackNum -> Text -> Track.Track -> m TrackId
named_track block_id ruler_id tracknum name track = do
    ident <- Ui.read_id (Id.ident_name block_id <> "." <> name)
    all_tracks <- Ui.gets Ui.state_tracks
    when (Id.TrackId ident `Map.member` all_tracks) $
        Ui.throw $ "track " <> showt ident <> " already exists"
    tid <- Ui.create_track ident track
    Ui.insert_track block_id tracknum
        (Block.track (Block.TId tid ruler_id) Config.track_width)
    return tid

remove_selected_tracks :: Cmd.M m => m ()
remove_selected_tracks = do
    (block_id, tracknums, _, _) <- Selection.tracks
    mapM_ (Ui.remove_track block_id) (reverse tracknums)

destroy_selected_tracks :: Cmd.M m => m ()
destroy_selected_tracks = do
    (block_id, tracknums, _, _) <- Selection.tracks
    -- Deleting each track will decrease the tracknum of the ones after it.
    mapM_ (destroy_track block_id) (zipWith (-) tracknums [0..])

-- | Remove a track from a block.  If that was the only block it appeared in,
-- delete the underlying track.  Rulers are never deleted automatically.
destroy_track :: Ui.M m => BlockId -> TrackNum -> m ()
destroy_track block_id tracknum = do
    tracklike <- Ui.require ("invalid tracknum: " <> showt tracknum)
        =<< Ui.track_at block_id tracknum
    Ui.remove_track block_id tracknum
    whenJust (Block.track_id_of tracklike) $ \track_id ->
        whenM (orphan_track track_id) $
            Ui.destroy_track track_id

-- | Swap the tracks at the given tracknums.  If one of the tracknums is out
-- of range, the track at the other tracknum will be moved to the beginning or
-- end, i.e. swapped with empty space.
swap_tracks :: Ui.M m => BlockId -> TrackNum -> TrackNum -> m ()
swap_tracks block_id num0 num1 = do
    track0 <- Ui.block_track_at block_id num0
    track1 <- Ui.block_track_at block_id num1
    case (track0, track1) of
        (Nothing, Nothing) -> return ()
        (Just t0, Nothing) -> remove num0 >> insert num1 t0
        (Nothing, Just t1) -> remove num1 >> insert num0 t1
        (Just t0, Just t1) -> do
            remove num0 >> insert num0 t1
            remove num1 >> insert num1 t0
    where
    remove = Ui.remove_track block_id
    insert = Ui.insert_track block_id

-- ** util

-- | Clip the tracknum to within the valid range.
clip_tracknum :: Ui.M m => BlockId -> TrackNum -> m TrackNum
clip_tracknum block_id tracknum = do
    tracks <- Ui.track_count block_id
    return $ max 0 (min tracks tracknum)

-- | Get the track to the right of the given tracknum.  This isn't just (+1)
-- because it skips collapsed tracks.
track_after :: Block.Block -> TrackNum -> TrackNum
track_after block tracknum
    -- It must already be the rightmost tracknum.
    | tracknum == next_tracknum = length (Block.block_tracks block)
    | otherwise = next_tracknum
    where next_tracknum = Ui.skip_unselectable_tracks block tracknum 1

-- * ruler

-- | Create a ruler with the given name.
ruler :: Ui.M m => Text -> Ruler.Ruler -> m RulerId
ruler name ruler = do
    ident <- Ui.read_id name
    Ui.create_ruler ident ruler

-- | Set a block to a new ruler.
new_ruler :: Ui.M m => BlockId -> Text -> Ruler.Ruler -> m RulerId
new_ruler block_id name r = do
    ruler_id <- ruler name r
    set_block_ruler ruler_id block_id
    return ruler_id

set_block_ruler :: Ui.M m => RulerId -> BlockId -> m ()
set_block_ruler ruler_id block_id =
    Transform.tracks block_id (Block.set_ruler_id ruler_id)

-- * general util

-- | Find a place to fit the given rect.  This is like a tiny window manager.
find_rect :: Maybe Rect.Rect -> (Int, Int) -> [Rect.Rect] -> (Int, Int)
find_rect maybe_screen (w, _h) rects =
    maybe (0, 0) Rect.upper_left $ Seq.minimum_on delta holes
    where
    -- First pick holes that fit, by increasing size, then pick the ones
    -- that don't fit, by decreasing size.
    delta rect = (if diff == 0 then -1 else negate (signum diff), abs diff)
        where diff = Rect.w rect - w
    holes = case maybe_screen of
        Nothing -> [Rect.xywh right 0 1 1]
        Just screen -> find_holes rects screen
    right = maximum $ 0 : map Rect.r rects

find_holes :: [Rect.Rect] -> Rect.Rect -> [Rect.Rect]
find_holes rects screen = case Ranges.extract ranges of
    Nothing -> [screen]
    Just rs -> [Rect.xywh x1 (Rect.y screen) (x2-x1) (Rect.h screen)
        | (x1, x2) <- rs]
    where
    extent r = (Rect.x r, Rect.r r)
    ranges = Ranges.invert (extent screen) $ Ranges.ranges $ map extent rects
