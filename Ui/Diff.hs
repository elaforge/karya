{- | Diff two states to produce a list of Updates, which must be sent to the UI
to make it display the second state.

This is unpleasantly complicated and subtle.  I wish I knew a better way!
-}
module Ui.Diff (diff, derive_diff) where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Writer as Writer

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Logger as Logger
import qualified Util.Map as Map
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import qualified Derive.Deriver.Monad as Derive


type DiffError = String

type DiffM a = Logger.LoggerT (Either Update.CmdUpdate Update.DisplayUpdate)
    (Error.ErrorT DiffError Identity.Identity) a

throw :: String -> DiffM a
throw = Error.throwError

change :: Update.CmdUpdate -> DiffM ()
change = Logger.logs . (:[]) . Left

change_display :: Update.DisplayUpdate -> DiffM ()
change_display = Logger.logs . (:[]) . Right

run :: DiffM ()
    -> Either DiffError ([Update.CmdUpdate], [Update.DisplayUpdate])
run = fmap Seq.partition_either . Identity.runIdentity . Error.runErrorT
    . Logger.exec

-- | Emit a list of the necessary 'Update's to turn @st1@ into @st2@.
diff :: [Update.CmdUpdate] -> State.State -> State.State
    -> Either DiffError ([Update.CmdUpdate], [Update.DisplayUpdate])
diff cmd_updates st1 st2 = fmap postproc $ run $ do
    -- View diff needs to happen first, because other updates may want to
    -- update the new view (technically these updates are redundant, but they
    -- don't hurt and filtering them would be complicated).
    diff_views st1 st2 (State.state_views st1) (State.state_views st2)
    mapM_ (uncurry3 diff_block) $
        Map.zip_intersection (State.state_blocks st1) (State.state_blocks st2)
    mapM_ (uncurry3 diff_track) $
        Map.zip_intersection (State.state_tracks st1) (State.state_tracks st2)
    mapM_ (uncurry3 diff_ruler) $
        Map.zip_intersection (State.state_rulers st1) (State.state_rulers st2)
    diff_state st1 st2
    where
    postproc (cs, ds) = (cupdates,
        ds ++ Maybe.mapMaybe Update.to_display (merge_updates st2 cupdates))
        where cupdates = cmd_updates ++ cs

-- | Given the track updates, figure out which other tracks have those tracks
-- merged and should also be updated.
--
-- The track diff doesn't generate event updates at all, they are expected to
-- be collected as a side-effect of the event insertion and deletion functions.
-- But that doesn't take into account merged tracks.
merge_updates :: State.State -> [Update.CmdUpdate] -> [Update.CmdUpdate]
merge_updates state updates = updates ++ concatMap propagate updates
    where
    propagate (Update.TrackUpdate track_id update)
        | is_event_update update =
            map (\tid -> Update.TrackUpdate tid update) merges_this
        | otherwise = []
        where merges_this = Map.get [] track_id merged_to_track
    propagate _ = []
    -- For each track update, find tracks that have it in merged
    track_to_merged = Maybe.mapMaybe merged_ids_of
        (concatMap Block.block_tracks (Map.elems (State.state_blocks state)))
    merged_ids_of track = case Block.tracklike_id track of
        Block.TId track_id _ -> Just (track_id, Block.track_merged track)
        _ -> Nothing
    -- Map from a track to all tracks that merge it.
    merged_to_track = Map.multimap [(merged_id, track_id)
        | (track_id, merged_ids) <- track_to_merged, merged_id <- merged_ids]
    is_event_update (Update.TrackEvents {}) = True
    is_event_update Update.TrackAllEvents {} = True
    is_event_update _ = False

-- ** view

diff_views :: State.State -> State.State -> Map.Map ViewId Block.View
    -> Map.Map ViewId Block.View -> DiffM ()
diff_views st1 st2 views1 views2 =
    forM_ (Map.pairs views1 views2) $ \(view_id, v1, v2) -> case (v1, v2) of
        (Nothing, Just view) ->
            change $ Update.ViewUpdate view_id (Update.CreateView view)
        (Just _, Nothing) ->
            change $ Update.ViewUpdate view_id Update.DestroyView
        (Just view1, Just view2) -> diff_view st1 st2 view_id view1 view2
        _ -> return ()

diff_view :: State.State -> State.State -> ViewId -> Block.View -> Block.View
    -> DiffM ()
diff_view st1 st2 view_id view1 view2 = do
    let emit = change . Update.ViewUpdate view_id
    let unequal f = unequal_on f view1 view2
    when (unequal Block.view_block) $
        throw $ show view_id ++ " changed from "
            ++ show (Block.view_block view1) ++ " to "
            ++ show (Block.view_block view2)
    when (unequal Block.view_rect) $
        emit $ Update.ViewSize (Block.view_rect view2)
    when (unequal Block.view_config) $
        emit $ Update.ViewConfig (Block.view_config view2)
    when (unequal Block.view_status) $
        emit $ Update.Status (Block.view_status view2)
    when (unequal Block.view_track_scroll) $
        emit $ Update.TrackScroll (Block.view_track_scroll view2)
    when (unequal Block.view_zoom) $
        emit $ Update.Zoom (Block.view_zoom view2)

    -- If the view doesn't have a block I should have failed long before here.
    let Just colors1 = view_selection_colors st1 view1
        Just colors2 = view_selection_colors st2 view2
    mapM_ (uncurry3 (diff_selection emit colors1 colors2))
        (Map.pairs (Block.view_selections view1) (Block.view_selections view2))

view_selection_colors :: State.State -> Block.View -> Maybe [Color.Color]
view_selection_colors state view = do
    block <- Map.lookup (Block.view_block view) (State.state_blocks state)
    return $ Block.config_selection_colors (Block.block_config block)

diff_selection :: (Update.ViewUpdate -> DiffM ())
    -> [Color.Color] -> [Color.Color] -> Types.SelNum
    -> Maybe Types.Selection -> Maybe Types.Selection
    -> DiffM ()
diff_selection emit colors1 colors2 selnum sel1 sel2 =
    -- Also update the selections if the selection color config has changed,
    -- because this isn't covered by Update.BlockConfig, because selection
    -- colors aren't stored seperately at the c++ level.
    when (sel1 /= sel2 || Seq.at colors1 selnum /= Seq.at colors2 selnum) $
        emit $ Update.Selection selnum sel2

-- ** block / track / ruler

diff_block :: BlockId -> Block.Block -> Block.Block -> DiffM ()
diff_block block_id block1 block2 = do
    let emit = change . Update.BlockUpdate block_id
    let unequal f = unequal_on f block1 block2
    when (unequal Block.block_title) $
        emit $ Update.BlockTitle (Block.block_title block2)
    when (unequal Block.block_config) $
        emit $ Update.BlockConfig (Block.block_config block2)
    when (unequal Block.block_skeleton) $
        emit $ Update.BlockSkeleton (Block.block_skeleton block2)

    let btracks1 = Block.block_tracks block1
        btracks2 = Block.block_tracks block2
    let bpairs = Seq.indexed_pairs_on Block.tracklike_id btracks1 btracks2
    forM_ bpairs $ \(i2, track1, track2) -> case (track1, track2) of
        (Just _, Nothing) -> emit $ Update.RemoveTrack i2
        (Nothing, Just track) -> emit $ Update.InsertTrack i2 track
        (Just track1, Just track2) | track1 /= track2 ->
            emit $ Update.BlockTrack i2 track2
        _ -> return ()

    let dtracks1 = map Block.display_track (Block.block_tracks block1)
        dtracks2 = map Block.display_track (Block.block_tracks block2)
    let dpairs = Seq.indexed_pairs_on Block.dtracklike_id dtracks1 dtracks2
    forM_ dpairs $ \(i2, track1, track2) -> case (track1, track2) of
        -- Insert and remove are emitted for cmd updates above, but
        -- the Update.to_display conversion filters them out.
        (Just _, Nothing) -> change_display $
            Update.BlockUpdate block_id (Update.RemoveTrack i2)
        (Nothing, Just dtrack) -> change_display $
            Update.BlockUpdate block_id (Update.InsertTrack i2 dtrack)
        (Just dtrack1, Just dtrack2) | dtrack1 /= dtrack2 -> change_display $
            Update.BlockUpdate block_id (Update.BlockTrack i2 dtrack2)
        _ -> return ()

diff_track :: TrackId -> Track.Track -> Track.Track -> DiffM ()
diff_track track_id track1 track2 = do
    -- Track events updates are collected directly by the State.State functions
    -- as they happen.
    let emit = change . Update.TrackUpdate track_id
    let unequal f = unequal_on f track1 track2
    when (unequal Track.track_title) $
        emit $ Update.TrackTitle (Track.track_title track2)
    when (unequal Track.track_bg) $
        emit $ Update.TrackBg (Track.track_bg track2)
    when (unequal Track.track_render) $
        emit $ Update.TrackRender (Track.track_render track2)

diff_ruler :: RulerId -> Ruler.Ruler -> Ruler.Ruler -> DiffM ()
diff_ruler ruler_id ruler1 ruler2 = do
    -- This does a complete compare of all the marks in all the rulers after
    -- each msg receive.  There shouldn't ever be that many rulers, but if this
    -- gets slow I can do something like insist marklist contents are immutable
    -- and only check names.
    when (ruler1 /= ruler2) $
        change $ Update.RulerUpdate ruler_id ruler2

-- ** state

diff_state :: State.State -> State.State -> DiffM ()
diff_state st1 st2 = do
    let emit = change . Update.StateUpdate
    let pairs f = Map.pairs (f st1) (f st2)
    when (State.state_config st1 /= State.state_config st2) $
        emit $ Update.Config (State.state_config st2)
    forM_ (pairs State.state_blocks) $ \(block_id, b1, b2) -> case (b1, b2) of
        (Nothing, Just block) -> emit $ Update.CreateBlock block_id block
        (Just _, Nothing) -> emit $ Update.DestroyBlock block_id
        _ -> return ()
    forM_ (pairs State.state_tracks) $ \(track_id, t1, t2) -> case (t1, t2) of
        (Nothing, Just track) -> emit $ Update.CreateTrack track_id track
        (Just _, Nothing) -> emit $ Update.DestroyTrack track_id
        _ -> return ()
    forM_ (pairs State.state_rulers) $ \(ruler_id, r1, r2) -> case (r1, r2) of
        (Nothing, Just ruler) -> emit $ Update.CreateRuler ruler_id ruler
        (Just _, Nothing) -> emit $ Update.DestroyRuler ruler_id
        _ -> return ()

-- * derive diff

type DeriveDiffM a = Writer.WriterT Derive.ScoreDamage Identity.Identity a

run_derive_diff :: DeriveDiffM () -> Derive.ScoreDamage
run_derive_diff = snd . Identity.runIdentity . Writer.runWriterT

-- | This diff is meant to determine score damage for the block, which
-- determines what will have to be rederived, if anything.
--
-- This is repeating some work done in 'diff', but is cleaner than reusing
-- 'diff' output because derive cares about specific things like mute, solo,
-- or track title changes.
derive_diff :: State.State -> State.State -> [Update.CmdUpdate]
    -> Derive.ScoreDamage
derive_diff st1 st2 updates = postproc $ run_derive_diff $ do
    mapM_ (uncurry3 derive_diff_block) $
        Map.zip_intersection (State.state_blocks st1) (State.state_blocks st2)
    mapM_ (uncurry3 derive_diff_track) $
        Map.zip_intersection (State.state_tracks st1) (State.state_tracks st2)
    where postproc = postproc_damage st2 . (updates_damage updates <>)

-- | Fill in 'Derive.sdamage_track_blocks'.
postproc_damage :: State.State -> Derive.ScoreDamage -> Derive.ScoreDamage
postproc_damage state (Derive.ScoreDamage tracks _ blocks) =
    Derive.ScoreDamage tracks track_blocks blocks
    where
    track_blocks = Set.fromList $ map fst $ State.find_tracks track_of_block
        (State.state_blocks state)
    track_of_block (Block.TId tid _) = Map.member tid tracks
    track_of_block _ = False

updates_damage :: [Update.CmdUpdate] -> Derive.ScoreDamage
updates_damage updates = mempty { Derive.sdamage_tracks = tracks }
    where
    tracks = Map.fromListWith (<>) $
        Maybe.mapMaybe Update.track_changed updates

derive_diff_block :: BlockId -> Block.Block -> Block.Block -> DeriveDiffM ()
derive_diff_block block_id block1 block2 = do
    let unequal f = unequal_on f block1 block2
    when (unequal Block.block_title || unequal Block.block_skeleton)
        block_damage

    let (ts1, ts2) = (Block.block_tracks block1, Block.block_tracks block2)
    let tpairs = Seq.indexed_pairs_on Block.tracklike_id ts1 ts2
    forM_ tpairs $ \(_, t1, t2) -> case (t1, t2) of
        (Just track1, Just track2)
            | flags_differ track1 track2 -> block_damage
            | otherwise -> return ()
        _ -> block_damage
    where
    block_damage =
        Writer.tell $ mempty { Derive.sdamage_blocks = Set.singleton block_id }

-- | True if the tracks flags differ in an a way that will require
-- rederivation.
flags_differ :: Block.Track -> Block.Track -> Bool
flags_differ track1 track2 = relevant track1 /= relevant track2
    where
    relevant = filter flag . Block.track_flags
    flag Block.Collapse = False
    flag Block.Mute = True
    flag Block.Solo = True

derive_diff_track :: TrackId -> Track.Track -> Track.Track -> DeriveDiffM ()
derive_diff_track track_id track1 track2 =
    when (unequal_on Track.track_title track1 track2) $
        Writer.tell $ mempty { Derive.sdamage_tracks =
            Map.singleton track_id Ranges.everything }


-- * util

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

unequal_on :: (Eq eq) => (a -> eq) -> a -> a -> Bool
unequal_on key a b = key a /= key b
