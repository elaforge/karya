module Perform.Warning where
import qualified Control.Monad.Error as Error

import qualified Ui.Track as Track
import Ui.Types
import qualified Ui.Block as Block
import qualified Perform.Timestamp as Timestamp


data Warning = Warning {
    warn_msg :: String
    , warn_event :: [StackPos]
    -- | Range that the warning covers.  It should be within the event's
    -- range.  It's in global time, so it needs to be converted back to
    -- local time, and it's (start, end) rather than (start, dur).
    -- TODO: convert these back to TrackPos with the tempo map
    , warn_pos :: Maybe (TrackPos, TrackPos)
    } deriving (Eq, Show)
warning = Warning

instance Error.Error Warning where
    strMsg msg = Warning msg [] Nothing

type StackPos = (Block.BlockId, Maybe Track.TrackId, Maybe TrackPos)
