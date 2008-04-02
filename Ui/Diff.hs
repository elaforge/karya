module Ui.Diff where
import qualified Control.Monad.Writer as Writer
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Ui.Update as Update
import qualified Ui.State as State


-- Emit creates for new windows, destroys for gone ones.
-- Emit inserts for new tracks, deletes for gone ones
-- Emit track redraws for tracks whose events changed.
diff :: State.State -> State.State -> [Update.Update]
diff st1 st2 = (snd . Writer.runWriter) $ do
    change $ map Update.DestroyView $ Map.keys (Map.difference views1 views2)
    mapM_ (uncurry3 diff_view) (pair_maps views1 views2)

    let visible_ids = (List.nub . map view_block . Map.elems) views2
    let visible_blocks = Map.filterWithKey (\k a -> k `elem` visible_ids)
            (State.state_blocks st2)
    mapM_ (uncurry3 diff_block)
        (pair_maps (State.state_blocks st1) visible_blocks)
    where
    views1 = State.state_views st1
    views2 = State.state_views st2

pair_maps map1 map2 =
    [(k, v1, v2) | (k, v1) <- Map.assocs map1, v2 <- Map.lookup k map2]

change :: [Sync.Update] -> Writer.Writer [Sync.Update] ()
change = Writer.tell

uncurry3 f (a, b, c) = f a b c

diff_view window view1 view2 = do
    when (view_ruler view1 /= view_ruler view2) $
        change [Update.SetRuler window (view_ruler view2)]

diff_block block_id block1 block2 = do
    when (block_title block1 /= block_title block2) $
        change [SetTitle block_id (block_title block2)]
    let dist = edit_distance (block_tracks block1) (block_tracks block2)
    forM_ (zip [0..] dist) $ \(i, edit) -> case edit of
        Same -> return ()
        Delete -> change [Update.RemoveTrack block_id i]
        Insert track -> change [Update.InsertTrack block_id i track]

-- * edit distance

data Show a => Edit a = Insert a | Delete | Same deriving Show
edit_distance [] ys = map Insert ys
edit_distance xs [] = replicate (length xs) Delete
edit_distance (x:xs) (y:ys)
    | x == y = Same : edit_distance xs ys
    | x `elem` ys = Insert y : edit_distance (x:xs) ys
    | otherwise = Delete : edit_distance xs (y:ys)
    where eq_at lst i v = length lst > i && lst!!i == v

d1 = edit_distance "abc" "abc"
d2 = edit_distance "abc" "axbc"
d3 = edit_distance "abc" "axxbc"
d4 = edit_distance "abc" "bc"
d5 = edit_distance "abc" "xyz"
d6 = edit_distance "abc" "axxxbc"
d7 = edit_distance "axxxbc" "abc"
