-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Diff two states to produce a list of Updates, which must be sent to the
    UI to make it display the second state.

    This is unpleasantly complicated and subtle.  I wish I knew a better way!
-}
module Ui.Diff (
    run
    , diff, derive_diff, track_diff
    , score_changed
    , diff_views
) where
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Writer as Writer
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Logger as Logger
import qualified Util.Map as Map
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Derive.Deriver.Monad as Derive
import qualified App.Config as Config
import Global
import Types


type DiffM a = Logger.LoggerT (Either Update.UiUpdate Update.DisplayUpdate)
    Identity.Identity a

change :: Update.UiUpdate -> DiffM ()
change = Logger.logs . (:[]) . Left

changes :: [Update.UiUpdate] -> DiffM ()
changes = Logger.logs . map Left

change_display :: Update.DisplayUpdate -> DiffM ()
change_display = Logger.logs . (:[]) . Right

run :: DiffM () -> ([Update.UiUpdate], [Update.DisplayUpdate])
run = Either.partitionEithers . Identity.runIdentity . Logger.exec

-- | Emit a list of the necessary 'Update's to turn @st1@ into @st2@.
diff :: [Update.CmdUpdate] -> State.State -> State.State
    -> ([Update.UiUpdate], [Update.DisplayUpdate])
diff cmd_updates st1 st2 = postproc cmd_updates st2 $ run $ do
    -- View diff needs to happen first, because other updates may want to
    -- update the new view (technically these updates are redundant, but they
    -- don't hurt and filtering them would be complicated).
    diff_views st1 st2 (State.state_views st1) (State.state_views st2)
    mapM_ (uncurry3 diff_block) $
        Map.zip_intersection (State.state_blocks st1) (State.state_blocks st2)
    mapM_ (uncurry3 (diff_track st2)) $
        Map.zip_intersection (State.state_tracks st1) (State.state_tracks st2)
    -- I don't diff rulers, since they have lots of things in them and rarely
    -- change.  But that means I need the CmdUpdate hack, and modifications
    -- must be done through State.modify_ruler.
    diff_state st1 st2

-- | Here's where the three different kinds of updates come together.
-- CmdUpdates are converted into UiUpdates, and then all of them converted
-- to DisplayUpdates.
postproc :: [Update.CmdUpdate] -> State.State
    -> ([Update.UiUpdate], [Update.DisplayUpdate])
    -> ([Update.UiUpdate], [Update.DisplayUpdate])
postproc cmd_updates to_state (ui_updates, display_updates) =
    (cancel_updates ui, display ++ refresh_selections to_state display)
    where
    ui = map Update.to_ui cmd_updates ++ ui_updates
    display = display_updates ++ to_display (merge_updates to_state ui)
    to_display = mapMaybe Update.to_display

-- | If the updates have InsertTrack or RemoveTrack the selections may have
-- been moved or deleted.  Emit updates for all selections for all views of
-- blocks with added or removed tracks.
refresh_selections :: State.State -> [Update.DisplayUpdate]
    -> [Update.DisplayUpdate]
refresh_selections state updates = concatMap selections view_ids
    where
    view_ids =
        [ view_id | (view_id, view) <- Map.toList (State.state_views state)
        , Set.member (Block.view_block view) block_ids
        ]
    block_ids = Set.fromList [block_id |
        Update.Block block_id update <-updates, is_track update]
    is_track (Update.RemoveTrack {}) = True
    is_track (Update.InsertTrack {}) = True
    is_track _ = False

    selections view_id = fromMaybe [] $ do
        view <- Map.lookup view_id (State.state_views state)
        let update selnum = Update.View view_id $
                Update.Selection selnum
                    (Map.lookup selnum (Block.view_selections view))
        return $ map update [0 .. Config.max_selnums - 1]

-- | DestroyView, DestroyBlock, DestroyTrack, and DestroyRuler cancel out
-- previous updates.
--
-- This isn't technically necessary since callers should be robust against that,
-- but cancelling means less work for them and their warnings are more likely
-- to indicate a real problem.
cancel_updates :: [Update.UiUpdate] -> [Update.UiUpdate]
cancel_updates updates = map fst $ filter (not . destroyed) $
    zip updates (drop 1 (List.tails updates))
    where
    destroyed (update, future) = case update of
        Update.View vid view -> case view of
            Update.DestroyView -> False
            _ -> any (== Update.View vid Update.DestroyView) future
        Update.Block bid _ -> future_has (Update.DestroyBlock bid)
        Update.Track tid _ -> future_has (Update.DestroyTrack tid)
        Update.Ruler rid -> future_has (Update.DestroyRuler rid)
        Update.State update -> case update of
            Update.CreateBlock bid _ -> future_has (Update.DestroyBlock bid)
            Update.CreateTrack tid _ -> future_has (Update.DestroyTrack tid)
            Update.CreateRuler rid _ -> future_has (Update.DestroyRuler rid)
            _ -> False
        where future_has destroy = any (== Update.State destroy) future

-- | Given the track updates, figure out which other tracks have those tracks
-- merged and should also be updated.
--
-- The track diff doesn't generate event updates at all, they are expected to
-- be collected as a side-effect of the event insertion and deletion functions.
-- But that doesn't take into account merged tracks.
merge_updates :: State.State -> [Update.UiUpdate] -> [Update.UiUpdate]
merge_updates state updates = updates ++ concatMap propagate updates
    where
    propagate (Update.Track track_id update)
        | is_event_update update =
            map (\tid -> Update.Track tid update) merges_this
        | otherwise = []
        where merges_this = Map.get [] track_id merged_to_track
    propagate _ = []
    -- For each track update, find tracks that have it in merged
    track_to_merged = mapMaybe merged_ids_of
        (concatMap Block.block_tracks (Map.elems (State.state_blocks state)))
    merged_ids_of track = case Block.tracklike_id track of
        Block.TId track_id _ -> Just (track_id, Block.track_merged track)
        _ -> Nothing
    -- Map from a track to all tracks that merge it.
    merged_to_track = Map.multimap
        [ (merged_id, track_id)
        | (track_id, merged_ids) <- track_to_merged
        , merged_id <- Set.toList merged_ids
        ]
    is_event_update (Update.TrackEvents {}) = True
    is_event_update Update.TrackAllEvents {} = True
    is_event_update _ = False

-- ** view

diff_views :: State.State -> State.State -> Map.Map ViewId Block.View
    -> Map.Map ViewId Block.View -> DiffM ()
diff_views st1 st2 views1 views2 =
    forM_ (Map.pairs views1 views2) $ \(view_id, paired) -> case paired of
        Seq.Second _ -> change $ Update.View view_id Update.CreateView
        Seq.First _ -> change $ Update.View view_id Update.DestroyView
        Seq.Both view1 view2
            | Block.view_block view1 /= Block.view_block view2 -> do
                change $ Update.View view_id Update.DestroyView
                change $ Update.View view_id Update.CreateView
            | otherwise -> diff_view st1 st2 view_id view1 view2

diff_view :: State.State -> State.State -> ViewId -> Block.View -> Block.View
    -> DiffM ()
diff_view st1 st2 view_id view1 view2 = do
    let emit = change . Update.View view_id
    let unequal f = unequal_on f view1 view2
    when (unequal Block.view_rect) $
        emit $ Update.ViewSize (Block.view_rect view2)
    let color = status_color st2 view2
    when (unequal Block.view_status || status_color st1 view1 /= color) $
        emit $ Update.Status (Block.view_status view2) color
    when (unequal Block.view_track_scroll) $
        emit $ Update.TrackScroll (Block.view_track_scroll view2)
    when (unequal Block.view_zoom) $
        emit $ Update.Zoom (Block.view_zoom view2)

    -- If the view doesn't have a block I should have failed long before here.
    mapM_ (uncurry (diff_selection emit))
        (Map.pairs (Block.view_selections view1) (Block.view_selections view2))

status_color :: State.State -> Block.View -> Color.Color
status_color state view =
    case Map.lookup block_id (State.state_blocks state) of
        Just block -> Block.status_color block_id block
            (State.config_root (State.state_config state))
        Nothing -> Config.status_default
    where block_id = Block.view_block view

diff_selection :: (Update.View -> DiffM ())
    -> Sel.Num -> Seq.Paired Sel.Selection Sel.Selection -> DiffM ()
diff_selection _ _ (Seq.Both sel1 sel2) | sel1 == sel2 = return ()
diff_selection emit selnum paired = case paired of
    Seq.Both sel1 sel2
        | sel1 /= sel2 -> emit $ Update.Selection selnum (Just sel2)
    Seq.Second sel2 -> emit $ Update.Selection selnum (Just sel2)
    Seq.First _ -> emit $ Update.Selection selnum Nothing
    _ -> return ()

-- ** block / track / ruler

diff_block :: BlockId -> Block.Block -> Block.Block -> DiffM ()
diff_block block_id block1 block2 = do
    let emit = change . Update.Block block_id
    let unequal f = unequal_on f block1 block2
    when (unequal Block.block_title) $
        emit $ Update.BlockTitle (Block.block_title block2)
    when (unequal Block.block_config) $
        emit $ Update.BlockConfig (Block.block_config block2)

    let dtracks1 = map Block.display_track (Block.block_tracks block1)
        dtracks2 = map Block.display_track (Block.block_tracks block2)
    let int_skel1 = Block.integrate_skeleton block1
        int_skel2 = Block.integrate_skeleton block2
    when (unequal Block.block_skeleton || int_skel1 /= int_skel2) $ do
        emit $ Update.BlockSkeleton (Block.block_skeleton block2) int_skel2
            (map Block.dtrack_status dtracks2)
        -- Changing the skeleton may change event styles.
        changes [Update.Track track_id Update.TrackAllEvents
            | track_id <- Block.block_track_ids block2]

    let btracks1 = Block.block_tracks block1
        btracks2 = Block.block_tracks block2
    let bpairs = Seq.indexed_pairs_on Block.tracklike_id btracks1 btracks2
    forM_ bpairs $ \(i2, paired) -> case paired of
        Seq.First _ -> emit $ Update.RemoveTrack i2
        Seq.Second track -> emit $ Update.InsertTrack i2 track
        Seq.Both track1 track2 | track1 /= track2 ->
            emit $ Update.BlockTrack i2 track2
        _ -> return ()

    let dpairs = Seq.indexed_pairs_on Block.dtracklike_id dtracks1 dtracks2
    forM_ dpairs $ \(i2, paired) -> case paired of
        -- Insert and remove are emitted for cmd updates above, but
        -- the Update.to_display conversion filters them out.
        Seq.First _ -> change_display $
            Update.Block block_id (Update.RemoveTrack i2)
        Seq.Second dtrack -> change_display $
            Update.Block block_id (Update.InsertTrack i2 dtrack)
        Seq.Both dtrack1 dtrack2 | dtrack1 /= dtrack2 -> change_display $
            Update.Block block_id (Update.BlockTrack i2 dtrack2)
        _ -> return ()

diff_track :: State.State -> TrackId -> Track.Track -> Track.Track -> DiffM ()
diff_track state track_id track1 track2 = do
    -- Track events updates are collected directly by the State.State functions
    -- as they happen.
    let emit = change . Update.Track track_id
    let unequal f = unequal_on f track1 track2
    when (unequal Track.track_title) $ do
        emit $ Update.TrackTitle (Track.track_title track2)
        -- Changing the title may change the type of the track, which may
        -- change event styles.  If it's going from non-note to note track, it
        -- can also change style of siblings since has_note_children becomes
        -- true.
        changes [Update.Track tid Update.TrackAllEvents
            | tid <- sibling_tracks state track_id]

    when (unequal Track.track_bg) $
        emit $ Update.TrackBg (Track.track_bg track2)
    when (unequal Track.track_render) $
        emit $ Update.TrackRender (Track.track_render track2)

sibling_tracks :: State.State -> TrackId -> [TrackId]
sibling_tracks state track_id = either (const []) id $ State.eval state $ do
    blocks <- State.blocks_with_track_id track_id
    return [tid | (_, tracks) <- blocks, (_, Block.TId tid _) <- tracks]

-- ** state

diff_state :: State.State -> State.State -> DiffM ()
diff_state st1 st2 = do
    let emit = change . Update.State
    let pairs f = Map.pairs (f st1) (f st2)
    when (State.state_config st1 /= State.state_config st2) $
        emit $ Update.Config (State.state_config st2)
    forM_ (pairs State.state_blocks) $ \(block_id, paired) -> case paired of
        Seq.Second block -> emit $ Update.CreateBlock block_id block
        Seq.First _ -> emit $ Update.DestroyBlock block_id
        _ -> return ()
    forM_ (pairs State.state_tracks) $ \(track_id, paired) -> case paired of
        Seq.Second track -> emit $ Update.CreateTrack track_id track
        Seq.First _ -> emit $ Update.DestroyTrack track_id
        _ -> return ()
    forM_ (pairs State.state_rulers) $ \(ruler_id, paired) -> case paired of
        Seq.Second ruler -> emit $ Update.CreateRuler ruler_id ruler
        Seq.First _ -> emit $ Update.DestroyRuler ruler_id
        _ -> return ()

-- * derive diff

type DeriveDiffM a = Writer.WriterT Derive.ScoreDamage Identity.Identity a

run_derive_diff :: DeriveDiffM () -> Derive.ScoreDamage
run_derive_diff = snd . Identity.runIdentity . Writer.runWriterT

-- | This diff is meant to determine score damage for the block, which
-- determines what will have to be rederived, if anything.
--
-- This is repeating some work done in 'diff', but is fundamentally different
-- because it cares about nonvisible changes, e.g. track title change on
-- a block without a view.
derive_diff :: State.State -> State.State -> [Update.UiUpdate]
    -> Derive.ScoreDamage
derive_diff st1 st2 updates = postproc $ run_derive_diff $
    -- If the config has changed, then everything is damaged.
    if unequal_on State.state_config st1 st2
    then Writer.tell $ mempty
        { Derive.sdamage_blocks = Map.keysSet (State.state_blocks st1)
            <> Map.keysSet (State.state_blocks st2)
        }
    else do
        mapM_ (uncurry derive_diff_block) $
            Map.pairs (State.state_blocks st1) (State.state_blocks st2)
        -- This doesn't check for added or removed tracks, because for them to
        -- have any effect they must be added to or removed from a block, which
        -- 'derive_diff_block' will catch.
        mapM_ (uncurry3 derive_diff_track) $
            Map.zip_intersection (State.state_tracks st1)
                (State.state_tracks st2)
    where
    postproc = postproc_damage st2 . (updates_damage block_rulers updates <>)
    block_rulers = Map.multimap
        [ (ruler_id, block_id)
        | (block_id, block) <- Map.toList (State.state_blocks st2)
        , ruler_id <- Block.block_ruler_ids block
        ]

-- | Fill in 'Derive.sdamage_track_blocks'.
postproc_damage :: State.State -> Derive.ScoreDamage -> Derive.ScoreDamage
postproc_damage state (Derive.ScoreDamage tracks _ blocks) =
    Derive.ScoreDamage tracks track_blocks blocks
    where
    track_blocks = Set.fromList $ map fst $ State.find_tracks track_of_block
        (State.state_blocks state)
    track_of_block (Block.TId tid _) = Map.member tid tracks
    track_of_block _ = False

-- | Derive damage from UiUpdates.
updates_damage :: Map.Map RulerId [BlockId] -> [Update.UiUpdate]
    -> Derive.ScoreDamage
updates_damage block_rulers updates = mempty
    { Derive.sdamage_tracks = tracks
    , Derive.sdamage_blocks = blocks
    }
    where
    tracks = Map.fromListWith (<>) $ mapMaybe Update.track_changed updates
    blocks = Set.fromList
        [ block_id
        | Update.Ruler ruler_id <- updates
        , block_id <- Map.findWithDefault [] ruler_id block_rulers
        ]

derive_diff_block :: BlockId -> Seq.Paired Block.Block Block.Block
    -> DeriveDiffM ()
derive_diff_block block_id pair = case pair of
    Seq.Both block1 block2 -> do
        let unequal f = unequal_on f block1 block2
        when (unequal (Text.strip . Block.block_title)
                || unequal Block.block_skeleton)
            block_damage
        let (ts1, ts2) = (Block.block_tracks block1, Block.block_tracks block2)
        let tpairs = Seq.indexed_pairs_on Block.tracklike_id ts1 ts2
        forM_ tpairs $ \(_, pair) -> case pair of
            Seq.Both track1 track2
                | flags_differ track1 track2 -> block_damage
                | otherwise -> return ()
            _ -> block_damage
    -- This means I wind up with damage on a block that is gone.  I think this
    -- is correct, since the cache will still have recorded dependencies on
    -- that block, which will cause its dependents to be rederived, as
    -- expected.
    Seq.First _ -> block_damage
    Seq.Second _ -> block_damage
    where
    block_damage =
        Writer.tell $ mempty { Derive.sdamage_blocks = Set.singleton block_id }

-- | True if the tracks flags differ in an a way that will require
-- rederivation.
flags_differ :: Block.Track -> Block.Track -> Bool
flags_differ track1 track2 = relevant track1 /= relevant track2
    where relevant = Set.filter (==Block.Disable) . Block.track_flags

derive_diff_track :: TrackId -> Track.Track -> Track.Track -> DeriveDiffM ()
derive_diff_track track_id track1 track2 =
    when (unequal (Text.strip . Track.track_title)
            || unequal Track.track_render) $
        Writer.tell $ mempty { Derive.sdamage_tracks =
            Map.singleton track_id Ranges.everything }
    where
    unequal f = unequal_on f track1 track2

-- * score_changed

-- | This is like 'derive_diff', but it only needs to return a Bool.  It's also
-- more sensitive in that it's looking for any change that you might want to
-- save to disk, not just changes that could require rederivation.
score_changed :: State.State -> State.State -> [Update.CmdUpdate] -> Bool
score_changed st1 st2 updates = or
    [ any Update.is_score_update updates
    , unequal_on (Map.keys . State.state_blocks) st1 st2
    , unequal_on (Map.keys . State.state_tracks) st1 st2
    , any (\(_, b1, b2) -> strip b1 /= strip b2) $
        Map.zip_intersection (State.state_blocks st1) (State.state_blocks st2)
    , any (\(_, t1, t2) -> t1 /= t2) $
        Map.zip_intersection (State.state_tracks st1) (State.state_tracks st2)
    , State.state_config st1 /= State.state_config st2
    ]
    where strip b = b { Block.block_config = Block.default_config }

-- * events diff

-- | Diff the events on one track.  This will only emit 'CmdTrackEvents',
-- and won't emit anything if the track title changed, or was created or
-- deleted.  Those diffs should be picked up by the main 'diff'.
track_diff :: State.State -> State.State -> TrackId -> [Update.CmdUpdate]
track_diff st1 st2 tid = case (Map.lookup tid t1, Map.lookup tid t2) of
    (Just _, Nothing) -> []
    (Nothing, Just _) -> []
    (Just t1, Just t2)
        | Track.track_title t1 == Track.track_title t2 ->
            diff_track_events tid (Track.track_events t1)
                (Track.track_events t2)
        | otherwise -> []
    (Nothing, Nothing) -> []
    where
    t1 = State.state_tracks st1
    t2 = State.state_tracks st2

diff_track_events :: TrackId -> Events.Events -> Events.Events
    -> [Update.CmdUpdate]
diff_track_events tid e1 e2 =
    map update $ Ranges.merge_sorted $ mapMaybe diff $
        Seq.pair_sorted_on Event.start
            (Events.ascending e1) (Events.ascending e2)
    where
    diff (Seq.First e) = Just (Event.range e)
    diff (Seq.Second e) = Just (Event.range e)
    diff (Seq.Both e1 e2)
        | e1 == e2 = Nothing
        | otherwise = Just (Event.start e1, max (Event.end e1) (Event.end e2))
    update (s, e) = Update.CmdTrackEvents tid s e


-- * util

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

unequal_on :: Eq eq => (a -> eq) -> a -> a -> Bool
unequal_on key a b = key a /= key b
