-- | Cmds for track level operations.
module Cmd.Lang.LTrack where
import Ui
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd

import qualified Derive.Schema as Schema


get_type :: BlockId -> TrackNum -> Cmd.CmdL Schema.TrackType
get_type block_id tracknum = do
    track_tree <- State.get_track_tree block_id
    maybe err return (Schema.get_track_type track_tree (Just tracknum))
    where
    err = Cmd.throw $ "can't get track type for " ++ show block_id
        ++ " at " ++ show tracknum
