{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Ui.Diff where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Writer as Writer
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Util.Seq as Seq
import qualified Util.Data

import qualified Ui.Block as Block
import qualified Ui.Track as Track

import qualified Ui.Update as Update
import qualified Ui.State as State


type DiffError = String

type DiffM a = Writer.WriterT [Update.Update]
    (Error.ErrorT DiffError Identity.Identity) a

throw :: String -> DiffM a
throw = Error.throwError
change :: Monad m => [Update.Update] -> Writer.WriterT [Update.Update] m ()
change = Writer.tell

run = Identity.runIdentity . Error.runErrorT . Writer.execWriterT

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
        (Util.Data.zip_intersection
            (State.state_blocks st1) visible_blocks)
    mapM_ (uncurry3 diff_track)
        (Util.Data.zip_intersection
            (State.state_tracks st1) (State.state_tracks st2))
    mapM_ (uncurry3 diff_ruler)
        (Util.Data.zip_intersection
            (State.state_rulers st1) (State.state_rulers st2))


-- | This is a nasty little case that falls out of how I'm doing diffs:
-- First the view diff runs, which detects changed track widths.
-- Then the block diff runs, which detects changed tracks.  Totally changed
-- tracks (remove old track, insert new one with default width) are not
-- distinguishable from minorly changed tracks (remove old track, insert new
-- one with the same width).  So what I do is assume that if there's an
-- InsertTrack and a corresponding TrackView in view_tracks of the new state,
-- it should get the width given in the view.
munge_updates :: State.State -> [Update.Update] -> [Update.Update]
munge_updates state updates = updates ++ munged
    where
    width_info = [(width, block_id, tracknum)
        | Update.BlockUpdate block_id (Update.InsertTrack tracknum _ width)
            <- updates]
    munged = concatMap set_width width_info
    set_width (old_width, block_id, tracknum) = do
        (view_id, view) <- filter ((==block_id) . Block.view_block . snd)
            (Map.assocs (State.state_views state))
        new_width <- maybe [old_width] ((:[]) . Block.track_view_width) $
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
        (Util.Data.zip_intersection views1 views2)

diff_view st1 st2 view_id view1 view2 = do
    let view_update = Update.ViewUpdate view_id
    when (Block.view_block view1 /= Block.view_block view2) $
        throw $ show view_id ++ " changed from "
            ++ show (Block.view_block view1) ++ " to "
            ++ show (Block.view_block view2)
    when (Block.view_rect view1 /= Block.view_rect view2) $
        change [view_update $ Update.ViewSize (Block.view_rect view2)]
    when (Block.view_config view1 /= Block.view_config view2) $
        change [view_update $ Update.ViewConfig (Block.view_config view2)]
    when (Block.view_status view1 /= Block.view_status view2) $
        change [view_update $ Update.Status (Block.show_status view2)]
    when (Block.view_track_scroll view1 /= Block.view_track_scroll view2) $
        change [view_update $
            Update.TrackScroll (Block.view_track_scroll view2)]
    when (Block.view_zoom view1 /= Block.view_zoom view2) $
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
    let pairs = indexed_pairs (\a b -> fst a == fst b) tracks1 tracks2
    forM_ pairs $ \(i2, track1, track2) -> case (track1, track2) of
        (Just (_, tview1), Just (_, tview2)) ->
            diff_track_view view_id i2 tview1 tview2
        _ -> return ()

    -- If the view doesn't have a block I should have failed long before here.
    let Just colors1 = view_selection_colors st1 view1
        Just colors2 = view_selection_colors st2 view2
    mapM_ (uncurry3 (diff_selection view_update colors1 colors2))
        (pair_maps (Block.view_selections view1) (Block.view_selections view2))

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
    let width = Block.track_view_width
    when (width tview1 /= width tview2) $
        change [Update.ViewUpdate view_id
            (Update.TrackWidth tracknum (width tview2))]

-- | Pair the Tracklikes from the Block with the TrackViews from the View.
track_info :: Block.ViewId ->  Block.View -> State.State
    -> DiffM [(Block.TracklikeId, Block.TrackView)]
track_info view_id view st =
    case Map.lookup block_id (State.state_blocks st) of
        Nothing -> throw $ show block_id ++ " of " ++ show view_id
            ++ " has no referent"
        Just block -> return $ zip
            (Block.block_tracks block) (Block.view_tracks view)
    where block_id = Block.view_block view

-- ** block / track / ruler

diff_block block_id block1 block2 = do
    let block_update = Update.BlockUpdate block_id
    when (Block.block_title block1 /= Block.block_title block2) $
        change [block_update $ Update.BlockTitle (Block.block_title block2)]
    when (Block.block_config block1 /= Block.block_config block2) $
        change [block_update $ Update.BlockConfig (Block.block_config block2)]

    let pairs = indexed_pairs (\a b -> fst a == fst b)
            (Block.block_track_widths block1) (Block.block_track_widths block2)
    forM_ pairs $ \(i2, track1, track2) -> case (track1, track2) of
        (Just _, Nothing) -> change [block_update $ Update.RemoveTrack i2]
        (Nothing, Just (track, width)) ->
            change [block_update $ Update.InsertTrack i2 track width]
        _ -> return ()

diff_track track_id track1 track2 = do
    let track_update = Update.TrackUpdate track_id
    when (Track.track_title track1 /= Track.track_title track2) $
        change [track_update $ Update.TrackTitle (Track.track_title track2)]
    when (Track.track_bg track1 /= Track.track_bg track2) $
        change [track_update $ Update.TrackBg]
    when (Track.track_render track1 /= Track.track_render track2) $
        change [track_update $ Update.TrackRender]

diff_ruler ruler_id ruler1 ruler2 = do
    -- This does a complete compare of all the marks in all the rulers after
    -- each msg receive.  There shouldn't ever be that many rulers, but if this
    -- gets slow I can do something like insist marklist contents are immutable
    -- and only check names.
    when (ruler1 /= ruler2) $
        change [Update.RulerUpdate ruler_id]

-- * util

uncurry3 f (a, b, c) = f a b c

pair_maps :: (Ord k) => Map.Map k v -> Map.Map k v -> [(k, Maybe v, Maybe v)]
pair_maps map1 map2 = map (\k -> (k, Map.lookup k map1, Map.lookup k map2))
    (Map.keys (Map.union map1 map2))

-- | Pair @a@ elements up with @b@ elements.  If they are equal according to
-- @eq@, they'll both be Just in the result.  If an @a@ is deleted going from
-- @a@ to @b@, it will be Nothing, and vice versa for @b@.
--
-- Kind of like an edit distance.
pair_lists :: (a -> b -> Bool) -> [a] -> [b] -> [(Maybe a, Maybe b)]
pair_lists _ [] ys = [(Nothing, Just y) | y <- ys]
pair_lists _ xs [] = [(Just x, Nothing) | x <- xs]
pair_lists eq (x:xs) (y:ys)
    | x `eq` y = (Just x, Just y) : pair_lists eq xs ys
    | any (eq x) ys = (Nothing, Just y) : pair_lists eq (x:xs) ys
    | otherwise = (Just x, Nothing) : pair_lists eq xs (y:ys)

-- | This is just like 'pair_lists', except that the index of each pair in
-- the /right/ list is included.  In other words, given @(i, Nothing, Just y)@,
-- @i@ is the position of @y@ in the @b@ list.  Given @(i, Just x, Nothing)@,
-- @i@ is where @x@ was deleted from the @b@ list.
indexed_pairs :: (a -> b -> Bool) -> [a] -> [b] -> [(Int, Maybe a, Maybe b)]
indexed_pairs eq xs ys = zip3 (indexed pairs) (map fst pairs) (map snd pairs)
    where pairs = pair_lists eq xs ys
indexed pairs = scanl f 0 pairs
    where
    f i (_, Nothing) = i
    f i _ = i+1
