-- | Cmds for track level operations.
module Cmd.Lang.LTrack where
import qualified Util.Seq as Seq
import qualified Util.Tree as Tree

import Ui
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd


-- | Merge all adjacent note/pitch pairs.
merge_all :: BlockId -> Cmd.CmdL ()
merge_all block_id = do
    tree <- State.get_track_tree block_id
    let collapse = Seq.map_maybe collapsable (Tree.paths tree)
    mapM_ (uncurry (State.merge_track block_id)) collapse
    where
    collapsable (track, parent : _, [])
        | num parent == num track + 1 = Just (num track, num parent)
    collapsable _ = Nothing
    num = State.track_tracknum
