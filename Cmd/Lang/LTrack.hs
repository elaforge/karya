-- | Cmds for track level operations.
module Cmd.Lang.LTrack where
import Ui
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Track as Track


get_type :: BlockId -> TrackNum -> Cmd.CmdL Track.TrackType
get_type block_id tracknum = do
    track_tree <- State.get_track_tree block_id
    maybe err return (Track.get_track_type track_tree tracknum)
    where
    err = Cmd.throw $ "can't get track type for " ++ show block_id
        ++ " at " ++ show tracknum
