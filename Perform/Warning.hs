module Perform.Warning where
import qualified Control.Monad.Error as Error

import qualified Ui.Track as Track
import Ui.Types
import qualified Perform.Timestamp as Timestamp

data Warning = Warning {
    warn_msg :: String
    , warn_event :: [CallPos]
    -- | Range that the warning covers.  It should be within the event's
    -- range.
    -- TODO: convert these back to TrackPos with the tempo map
    , warn_pos :: Maybe (Timestamp.Timestamp, Timestamp.Timestamp)
    } deriving (Eq, Show)
warning = Warning

instance Error.Error Warning where
    strMsg msg = Warning msg [] Nothing

type CallPos = (Track.TrackId, TrackPos)
