{- | Diff two states to produce a list of Updates, which must be sent to the UI
to make it display the second state.

This is unpleasantly complicated and subtle.  I wish I knew a better way!
-}
module Ui.Diff (diff, track_diff) where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Util.Seq as Seq
import qualified Util.Logger as Logger
import qualified Util.Map as Map

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update


type DiffError = String

type DiffM a = Logger.LoggerT Update.Update
    (Error.ErrorT DiffError Identity.Identity) a

throw :: String -> DiffM a
throw = Error.throwError
change :: [Update.Update] -> DiffM ()
change = Logger.logs

run :: DiffM () -> Either DiffError [Update.Update]
run = Identity.runIdentity . Error.runErrorT . Logger.exec

-- | Emit a list of the necessary 'Update's to turn @st1@ into @st2@.
diff :: State.State -> State.State -> Either DiffError [Update.Update]
diff st1 st2 = fmap (munge_updates st2) $ run $ do
    -- View diff needs to happen first, because other updates may want to
    -- update the new view (technically these updates are redundant, but they
    -- don't hurt and filtering them would be complicated).
    diff_views st1 st2 (State.state_views st1) (State.state_views st2)

    -- Only emit updates for blocks that are actually in a displayed view.
    let visible_ids = (List.nub . map Block.view_block . Map.elems)
            (State.state_views st2)
        visible_blocks = Map.filterWithKey (\k _v -> k `elem` visible_ids)
            (State.state_blocks st2)
    mapM_ (uncurry3 diff_block)
        (Map.zip_intersection (State.state_blocks st1) visible_blocks)
    mapM_ (uncurry3 diff_track)
        (Map.zip_intersection (State.state_tracks st1) (State.state_tracks st2))
    mapM_ (uncurry3 diff_ruler)
        (Map.zip_intersection (State.state_rulers st1) (State.state_rulers st2))

-- | Find only the TrackUpdates between two states.
track_diff :: State.State -> State.State -> [Update.Update]
track_diff old new = either (const []) id $ run $ mapM_ (uncurry3 diff_track)
    (Map.zip_intersection (State.state_tracks old) (State.state_tracks new))

-- | This is a nasty little case that falls out of how I'm doing diffs:
-- First the view diff runs, which detects changed track widths.
-- Then the block diff runs, which detects changed tracks.  Replaced
-- tracks (remove old track, insert new one with default width, which as a new
-- track should get the default width in all views) are not distinguishable
-- from a merely altered track (which can look like remove old track, insert
-- new one with the same width, and should keep its width in each view).  So
-- what I do is assume that if there's an InsertTrack and a corresponding
-- TrackView in view_tracks of the new state, it should get the width given in
-- the view.
--
-- This is yet more crap to support view-local track width...
munge_updates :: State.State -> [Update.Update] -> [Update.Update]
munge_updates state updates = updates ++ munged
    where
    width_info = [(width, block_id, tracknum)
        | Update.BlockUpdate block_id (Update.InsertTrack tracknum width _)
            <- updates]
    munged = concatMap set_width width_info
    -- TODO instead of adding a TrackWidth, modify the InsertTrack?
    set_width (old_width, block_id, tracknum) = do
        (view_id, view) <- filter ((==block_id) . Block.view_block . snd)
            (Map.assocs (State.state_views state))
        let new_width = maybe old_width Block.track_view_width $
                Seq.at (Block.view_tracks view) tracknum
        return $
            Update.ViewUpdate view_id (Update.TrackWidth tracknum new_width)

-- ** view

diff_views st1 st2 views1 views2 = do
    change $ map (flip Update.ViewUpdate Update.DestroyView) $
        Map.keys (Map.difference views1 views2)
    let new_views = Map.difference views2 views1
    change $ map (flip Update.ViewUpdate Update.CreateView) (Map.keys new_views)
    mapM_ (uncurry3 (diff_view st1 st2))
        (Map.zip_intersection views1 views2)

diff_view st1 st2 view_id view1 view2 = do
    let view_update = Update.ViewUpdate view_id
    let unequal f = unequal_on f view1 view2
    when (unequal Block.view_block) $
        throw $ show view_id ++ " changed from "
            ++ show (Block.view_block view1) ++ " to "
            ++ show (Block.view_block view2)
    when (unequal Block.view_rect) $
        change [view_update $ Update.ViewSize (Block.view_rect view2)]
    when (unequal Block.view_config) $
        change [view_update $ Update.ViewConfig (Block.view_config view2)]
    when (unequal Block.view_status) $
        change [view_update $ Update.Status (Block.show_status view2)]
    when (unequal Block.view_track_scroll) $
        change [view_update $
            Update.TrackScroll (Block.view_track_scroll view2)]
    when (unequal Block.view_zoom) $
        change [view_update $ Update.Zoom (Block.view_zoom view2)]

    -- The track view info (widths) is in the View, while the track data itself
    -- (Tracklikes) is in the Block.  Since one track may have been added or
    -- deleted while another's width was changed, I have to run 'indexed_pairs'
    -- here with the Blocks' Tracklikes to pair up the the same Tracklikes
    -- before comparing their widths.  'i' will be the TrackNum index for the
    -- tracks pre insertion/deletion, which is correct since the view is diffed
    -- and its Updates run before the Block updates.  This also means it
    -- actually matters that updates are run in order.  This is a lot of
    -- subtlety just to detect width changes!
    --
    -- 'indexed_pairs' is run again on the Blocks to actually delete or insert
    -- tracks.

    tracks1 <- track_info view_id view1 st1
    tracks2 <- track_info view_id view2 st2
    let pairs = indexed_pairs_on fst tracks1 tracks2
    forM_ pairs $ \(i2, track1, track2) -> case (track1, track2) of
        (Just (_, tview1), Just (_, tview2)) ->
            diff_track_view view_id i2 tview1 tview2
        _ -> return ()

    -- If the view doesn't have a block I should have failed long before here.
    let Just colors1 = view_selection_colors st1 view1
        Just colors2 = view_selection_colors st2 view2
    mapM_ (uncurry3 (diff_selection view_update colors1 colors2))
        (Map.pairs (Block.view_selections view1) (Block.view_selections view2))

view_selection_colors state view = do
    block <- Map.lookup (Block.view_block view) (State.state_blocks state)
    return $ Block.config_selection_colors (Block.block_config block)

diff_selection view_update colors1 colors2 selnum sel1 sel2 =
    -- Also update the selections if the selection color config has changed,
    -- because this isn't covered by Update.BlockConfig, because selection
    -- colors aren't stored seperately at the c++ level.
    when (sel1 /= sel2 || Seq.at colors1 selnum /= Seq.at colors2 selnum) $
        change [view_update $ Update.Selection selnum sel2]

diff_track_view view_id tracknum tview1 tview2 = do
    when (unequal_on Block.track_view_width tview1 tview2) $
        change [Update.ViewUpdate view_id
            (Update.TrackWidth tracknum (Block.track_view_width tview2))]

-- | Pair the Tracklikes from the Block with the TrackViews from the View.
track_info :: ViewId ->  Block.View -> State.State
    -> DiffM [(Block.TracklikeId, Block.TrackView)]
track_info view_id view st =
    case Map.lookup block_id (State.state_blocks st) of
        Nothing -> throw $ show block_id ++ " of " ++ show view_id
            ++ " has no referent"
        Just block -> return $ zip
            (Block.block_tracklike_ids block) (Block.view_tracks view)
    where block_id = Block.view_block view

-- ** block / track / ruler

diff_block block_id block1 block2 = do
    let block_update = Update.BlockUpdate block_id
    let unequal f = unequal_on f block1 block2
    when (unequal Block.block_title) $
        change [block_update $ Update.BlockTitle (Block.block_title block2)]
    when (unequal Block.block_config) $
        change [block_update $ Update.BlockConfig (Block.block_config block2)]
    when (unequal Block.block_skeleton) $
        change [block_update $
            Update.BlockSkeleton (Block.block_skeleton block2)]

    let (dtracks1, dtracks2) = (Block.block_display_tracks block1,
            Block.block_display_tracks block2)
    let pairs = indexed_pairs_on (Block.dtrack_tracklike_id . fst)
            dtracks1 dtracks2
    forM_ pairs $ \(i2, track1, track2) -> case (track1, track2) of
        (Just _, Nothing) -> change [block_update $ Update.RemoveTrack i2]
        -- I only need the default creation width when a new track is being
        -- created, oddly enough.
        (Nothing, Just (dtrack, width)) ->
            change [block_update $ Update.InsertTrack i2 width dtrack]
        (Just (dtrack1, _), Just (dtrack2, _)) | dtrack1 /= dtrack2 -> change
            [block_update $ Update.DisplayTrack i2 dtrack2]
        _ -> return ()

diff_track track_id track1 track2 = do
    let track_update = Update.TrackUpdate track_id
    let unequal f = unequal_on f track1 track2
    when (unequal Track.track_title) $
        change [track_update $ Update.TrackTitle (Track.track_title track2)]
    when (unequal Track.track_bg) $
        change [track_update $ Update.TrackBg]
    when (unequal Track.track_render) $
        change [track_update $ Update.TrackRender]

diff_ruler ruler_id ruler1 ruler2 = do
    -- This does a complete compare of all the marks in all the rulers after
    -- each msg receive.  There shouldn't ever be that many rulers, but if this
    -- gets slow I can do something like insist marklist contents are immutable
    -- and only check names.
    when (ruler1 /= ruler2) $
        change [Update.RulerUpdate ruler_id]

-- * util

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

unequal_on :: (Eq eq) => (a -> eq) -> a -> a -> Bool
unequal_on key a b = key a /= key b

-- | This is just like 'Seq.diff', except that the index of each pair in
-- the /right/ list is included.  In other words, given @(i, Nothing, Just y)@,
-- @i@ is the position of @y@ in the @b@ list.  Given @(i, Just x, Nothing)@,
-- @i@ is where @x@ was deleted from the @b@ list.
indexed_pairs :: (a -> b -> Bool) -> [a] -> [b] -> [(Int, Maybe a, Maybe b)]
indexed_pairs eq xs ys = zip3 (indexed pairs) (map fst pairs) (map snd pairs)
    where pairs = Seq.diff eq xs ys

indexed_pairs_on :: (Eq eq) => (a -> eq) -> [a] -> [a]
    -> [(Int, Maybe a, Maybe a)]
indexed_pairs_on key xs ys = indexed_pairs (\a b -> key a == key b) xs ys

indexed pairs = scanl f 0 pairs
    where
    f i (_, Nothing) = i
    f i _ = i+1
