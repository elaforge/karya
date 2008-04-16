module Ui.Diff where
import Control.Monad
import qualified Control.Monad.Writer as Writer
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Util.Seq as Seq

import qualified Ui.Block as Block

import qualified Ui.Update as Update
import qualified Ui.State as State


-- | Emit a list of the necessary 'Update's to turn @st1@ into @st2@.
diff :: State.State -> State.State -> [Update.Update]
diff st1 st2 = Writer.execWriter $ do
    diff_views st1 st2 (State.state_views st1) (State.state_views st2)

    -- Only emit updates for blocks that are actually in a displayed view.
    let visible_ids = (List.nub . map Block.view_block . Map.elems)
            (State.state_views st2)
        visible_blocks = Map.filterWithKey (\k _a -> k `elem` visible_ids)
            (State.state_blocks st2)
    mapM_ (uncurry3 diff_block)
        (zip_maps (State.state_blocks st1) visible_blocks)

diff_views st1 st2 views1 views2 = do
    change $ map (flip Update.ViewUpdate Update.DestroyView) $
        Map.keys (Map.difference views1 views2)
    change $ map (flip Update.ViewUpdate Update.CreateView) $
        Map.keys (Map.difference views2 views1)
    mapM_ (uncurry3 (diff_view st1 st2)) (zip_maps views1 views2)

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

    -- The track view info (widths) is in the View, while the track data itself
    -- (Tracklikes) is in the Block.  Since one track may have been added or
    -- deleted while another's width was changed, I have to run 'edit_distance'
    -- here with the Blocks' Tracklikes to pair up the the same Tracklikes
    -- before comparing their widths.  'i' will be the TrackNum index for the
    -- tracks pre insertion/deletion, which is correct since the view is diffed
    -- and its Updates run before the Block updates.  This also means it
    -- actually matters that updates are run in order.  This is a lot of
    -- subtlety just to detect width changes!
    --
    -- 'edit_distance' is run again on the Blocks to actually delete or insert
    -- tracks.

    tracks1 <- track_info view_id view1 st1
    tracks2 <- track_info view_id view2 st2
    let edits = edit_distance (\a b -> fst a == fst b) tracks1 tracks2
    forM_ (Seq.enumerate edits) $ \(i, edit) -> case edit of
        Same -> when (snd (tracks1 !! i) /= snd (tracks2 !! i)) $
            change [view_update $ Update.SetTrackWidth i (snd (tracks2 !! i))]
        _ -> return ()

-- | Pair the Tracklikes from the Block with the widths from the View.
track_info view_id view st =
    case Map.lookup block_id (State.state_blocks st) of
        Nothing -> throw $ show block_id ++ " of " ++ show view_id
            ++ " has no referent"
        Just block -> return $ zip
            (map fst (Block.block_tracks block)) (Block.view_track_widths view)
    where block_id = Block.view_block view

-- TODO should be ErrorT
throw = error

diff_block block_id block1 block2 = do
    let block_update = Update.BlockUpdate block_id
    when (Block.block_title block1 /= Block.block_title block2) $
        change [block_update $ Update.BlockTitle (Block.block_title block2)]
    when (Block.block_config block1 /= Block.block_config block2) $
        change [block_update $ Update.BlockConfig (Block.block_config block2)]

    when (Block.block_ruler_track block1 /= Block.block_ruler_track block2) $
        change [block_update $ Update.InsertTrack Block.ruler_tracknum
            (Block.block_ruler_track block2) 0]

    let tracks1 = Block.block_tracks block1
        tracks2 = Block.block_tracks block2
        edits = edit_distance (\a b -> fst a == fst b) tracks1 tracks2
    forM_ (Seq.enumerate edits) $ \(i, edit) -> case edit of
        Same -> return ()
        Delete -> change [block_update $ Update.RemoveTrack i]
        Insert (track, width) ->
            change [block_update $ Update.InsertTrack i track width]

change :: [Update.Update] -> Writer.Writer [Update.Update] ()
change = Writer.tell

-- * util

uncurry3 f (a, b, c) = f a b c
-- Given two maps, pair up the elements in @map1@ with a samed-keyed element
-- in @map2@, if there is one.  Elements that are only in @map1@ or @map2@ will
-- not be included in the output.
zip_maps map1 map2 =
    [(k, v1, v2) | (k, v1) <- Map.assocs map1, v2 <- Map.lookup k map2]

data Show a => Edit a = Insert a | Delete | Same deriving (Eq, Show)
-- | Cheap and simple edit distance between two lists.
edit_distance :: Show a => (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
edit_distance _ [] ys = map Insert ys
edit_distance _ xs [] = replicate (length xs) Delete
edit_distance eq (x:xs) (y:ys)
    | x `eq` y = Same : edit_distance eq xs ys
    | any (eq x) ys = Insert y : edit_distance eq (x:xs) ys
    | otherwise = Delete : edit_distance eq xs (y:ys)
