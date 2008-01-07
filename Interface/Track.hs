{-
A Track is a container for Events.  A track goes from TrackPos 0 until
the end of the last Event.
-}

module Interface.Track where
import Interface.Types
import qualified Interface.Event as Event

data Track = Track deriving (Show) {- opaque
    events :: [(TrackPos, Event)]
-}
type T = Track
{- data TrackView =
    { state :: Track
    }
-}

-- | create with given bg color
create :: Color -> UI Track
create bg = undefined

add_event :: Track -> (TrackPos, Event.T) -> UI ()
add_event track (pos, evt) = undefined

remove_event :: Track -> (TrackPos, Event.T) -> UI ()
remove_event track (pos, evt) = undefined

get_events :: Track -> TrackPos -> TrackPos -> UI [(TrackPos, Event.T)]
get_events track begin end = undefined

get_attrs :: Track -> Attrs
get_attrs track = undefined
