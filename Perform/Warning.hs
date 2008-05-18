module Perform.Warning where
import qualified Control.Monad.Error as Error

import qualified Ui.Track as Track
import Ui.Types

data Warning = Warning {
    warn_msg :: String
    , warn_stack :: [CallPos]
    } deriving (Eq, Show)
warning = Warning

instance Error.Error Warning where
    strMsg msg = Warning msg []

type CallPos = (Track.TrackId, TrackPos)
