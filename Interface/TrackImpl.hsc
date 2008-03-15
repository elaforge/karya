{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-
A Track is a container for Events.  A track goes from TrackPos 0 until
the end of the last Event.
-}

module Interface.TrackImpl where
import Foreign

import Interface.Types
-- import qualified Interface.Color as Color
import qualified Interface.Event as Event

data CEventTrackModel
data Track = Track
    { track_p :: ForeignPtr CEventTrackModel
    , track_attrs :: Attrs
    , track_events :: EventList
    } deriving (Show)
type EventList = [(TrackPos, Event.Event)]

create :: Color.Color -> IO Track
create color = do
    trackp <- with color $ \colorp -> c_event_track_model_new colorp
    trackfp <- newForeignPtr c_event_track_model_destroy trackp
    return $ Track trackfp [] []

foreign import ccall unsafe "event_track_model_new"
    c_event_track_model_new :: Ptr Color.Color -> IO (Ptr CEventTrackModel)
foreign import ccall unsafe "&event_track_model_destroy"
    c_event_track_model_destroy :: FunPtr (Ptr CEventTrackModel -> IO ())

{-
add_event :: Track -> (TrackPos, Event.T) -> IO ()
add_event track (pos, evt) = undefined

remove_event :: Track -> (TrackPos, Event.T) -> IO ()
remove_event track (pos, evt) = undefined

get_events :: Track -> TrackPos -> TrackPos -> IO [(TrackPos, Event.T)]
get_events track begin end = undefined
-}

get_attrs :: Track -> Attrs
get_attrs = track_attrs

set_attrs :: Track -> Attrs -> Track
set_attrs track attrs = track { track_attrs = attrs }


-- * view

data TrackView = TrackView deriving (Show)
