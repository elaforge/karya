-- | Cmds for track level operations.
module Cmd.Lang.LTrack where
import qualified Util.Seq as Seq
import qualified Util.Tree as Tree

import Ui
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd

import qualified Derive.Schema as Schema
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch


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

info :: BlockId -> TrackNum
    -> Cmd.CmdL (Schema.TrackType, Maybe Score.Instrument, Pitch.ScaleId)
info block_id tracknum = do
    track_tree <- State.get_track_tree block_id
    proj_scale <- State.get_project_scale
    case Schema.get_track_info proj_scale track_tree (Just tracknum) of
        (Nothing, _, _) -> Cmd.throw $ "can't get track type for "
            ++ show block_id ++ " at " ++ show tracknum
        (Just typ, inst, scale) -> return (typ, inst, scale)
