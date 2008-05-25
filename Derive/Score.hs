{- | A Score is the unit at which derivers operate, and is just a list of
Events.  The events here are the derivation-level view of an Event.  They are
generated from UI Events, and will eventually be transformed into Perform
Events, which are specific to the performance backend.
-}
module Derive.Score where
import qualified Data.Map as Map

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Warning as Warning


-- actually, this is just one track, but they eventually get merged
-- data Score = Score { score_events :: [Event] } deriving (Show)

data Event = Event {
    -- | These are the core attributes that define an event.  For the moment,
    -- UI display attributes like font style are not preserved here.
    event_start :: TrackPos
    , event_duration :: TrackPos
    , event_text :: String
    , event_controls :: ControllerMap

    -- | Keep track of this event's display in various tracks (it may appear
    -- in more than one if it appears in a merged track).  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: [Warning.CallPos]

    -- | These are optional parameters that may or may not be required by the
    -- performer.
    , event_instrument :: Maybe Instrument
    , event_pitch :: Maybe Pitch.Pitch
    } deriving (Show)
-- instance Show Event where
--     show evt = "<" ++ event_text evt ++ ">"

-- event :: TrackPos -> TrackPos -> String -> Warning.CallPos -> Event
-- event start dur text call_pos =
--     Event start dur text Nothing Nothing Map.empty [call_pos]

from_track_event :: ControllerMap -> Track.TrackId -> (TrackPos, Event.Event)
    -> Event
from_track_event controls track_id (pos, evt) =
    Event pos (Event.event_duration evt) (Event.event_text evt)
        controls [(track_id, pos)]
        Nothing Nothing

type ControllerMap = Map.Map Controller Signal.Signal

-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as the
-- backend itself, but things at the Derive layer and above don't care about
-- all that.
data Instrument = Instrument String deriving (Eq, Ord, Show)

newtype Controller = Controller String deriving (Eq, Ord, Show)
