module Ui.Diff where
import Control.Monad
import qualified Control.Monad.Writer as Writer
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Ui.Block as Block

import qualified Ui.Update as Update
import qualified Ui.State as State


-- | Emit a list of the necessary 'Update's to turn @st1@ into @st2@.
diff :: State.State -> State.State -> [Update.Update]
diff st1 st2 = (snd . Writer.runWriter) $ do
    diff_views (State.state_views st1) (State.state_views st2)

    -- Only bother to emit updates for blocks that are actually in a displayed
    -- view.
    let visible_ids = (List.nub . map Block.view_block . map fst . Map.elems)
            (State.state_views st2)
        visible_blocks = Map.filterWithKey (\k a -> k `elem` visible_ids)
            (State.state_blocks st2)
    mapM_ (uncurry3 diff_block)
        (pair_maps (State.state_blocks st1) visible_blocks)

diff_views views1 views2 = do
    change $ map (flip Update.ViewUpdate Update.DestroyView) $
        Map.keys (Map.difference views1 views2)
    change $ map (flip Update.ViewUpdate Update.CreateView) $
        Map.keys (Map.difference views2 views1)
    mapM_ (uncurry3 diff_view) (pair_maps views1 views2)

diff_view view_id (view1, _) (view2, _) = do
    let view_update = Update.ViewUpdate view_id
    when (Block.view_block view1 /= Block.view_block view2) $
        change [Update.Error $ show view_id ++ " changed from "
            ++ show (Block.view_block view1) ++ " to "
            ++ show (Block.view_block view2)]
    when (Block.view_rect view1 /= Block.view_rect view2) $
        change [view_update $ Update.ViewSize (Block.view_rect view2)]
    when (Block.view_config view1 /= Block.view_config view2) $
        change [view_update $ Update.ViewConfig (Block.view_config view2)]

diff_block block_id block1 block2 = do
    let block_update = Update.BlockUpdate block_id
    when (Block.block_title block1 /= Block.block_title block2) $
        change [block_update $ Update.BlockTitle (Block.block_title block2)]
    when (Block.block_config block1 /= Block.block_config block2) $
        change [block_update $ Update.BlockConfig (Block.block_config block2)]
    when (Block.block_ruler block1 /= Block.block_ruler block2) $
        change [block_update $ Update.BlockRuler (Block.block_ruler block2)]
    let edits = edit_distance (\a b -> fst a == fst b)
            (Block.block_tracks block1) (Block.block_tracks block2)
    forM_ (zip [0..] edits) $ \(i, edit) -> case edit of
        Same -> return ()
        Delete -> change [block_update $ Update.RemoveTrack i]
        Insert (track, width) ->
            change [block_update $ Update.InsertTrack i track width]

change :: [Update.Update] -> Writer.Writer [Update.Update] ()
change = Writer.tell

-- * util

uncurry3 f (a, b, c) = f a b c
pair_maps map1 map2 =
    [(k, v1, v2) | (k, v1) <- Map.assocs map1, v2 <- Map.lookup k map2]

data Show a => Edit a = Insert a | Delete | Same deriving Show
edit_distance :: Show a => (a -> a -> Bool) -> [a] -> [a] -> [Edit a]
edit_distance _ [] ys = map Insert ys
edit_distance _ xs [] = replicate (length xs) Delete
edit_distance eq (x:xs) (y:ys)
    | x `eq` y = Same : edit_distance eq xs ys
    | any (eq x) ys = Insert y : edit_distance eq (x:xs) ys
    | otherwise = Delete : edit_distance eq xs (y:ys)


{-
test = do
    let dist = edit_distance (==)
    let d1 = dist "abc" "abc"
        d2 = dist "abc" "axbc"
        d3 = dist "abc" "axxbc"
        d4 = dist "abc" "bc"
        d5 = dist "abc" "xyz"
        d6 = dist "abc" "axxxbc"
        d7 = dist "axxxbc" "abc"
-}
