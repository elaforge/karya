{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
{-
A Track is a container for Events.  A track goes from TrackPos 0 until
the end of the last Event.

Should be able to lazily fetch events starting at any TrackPos, going forwards
or backwards.

TrackAttrs:

cached derivation and realization (depending on deriver scope)
modified event map, for derivation (old trackpos -> new trackpos)

-}

module Ui.TrackImpl where
import qualified Control.Concurrent.MVar as MVar
import Foreign
import Foreign.C

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Event as Event

data CEventTrackModel
data Track = Track
    { track_p :: ForeignPtr CEventTrackModel
    , track_attrs :: MVar.MVar Attrs
    } deriving (Eq)
instance Show Track where
    show track = "<Track.Track " ++ show (track_p track) ++ ">"

create :: Color.Color -> UI Track
create color = do
    trackp <- with color $ \colorp -> c_event_track_model_new colorp
    trackfp <- newForeignPtr c_event_track_model_destroy trackp
    attrs <- MVar.newMVar []
    return $ Track trackfp attrs

foreign import ccall unsafe "event_track_model_new"
    c_event_track_model_new :: Ptr Color.Color -> IO (Ptr CEventTrackModel)
foreign import ccall unsafe "&event_track_model_destroy"
    c_event_track_model_destroy :: FunPtr (Ptr CEventTrackModel -> IO ())

-- Return False if the event couldn't be inserted because it overlaps with
-- the previous one.
insert_event :: Track -> TrackPos -> Event.Event -> UI Bool
insert_event track pos event = do
    ok <- withForeignPtr (track_p track) $ \trackp -> with pos $ \posp ->
        with event $ \eventp ->
            c_event_track_model_insert_event trackp posp eventp
    return (toBool ok)

remove_event :: Track -> TrackPos -> UI Bool
remove_event track pos = do
    ok <- withForeignPtr (track_p track) $ \trackp -> with pos $ \posp ->
        c_event_track_model_remove_event trackp posp
    return (toBool ok)

foreign import ccall unsafe "event_track_model_insert_event"
    c_event_track_model_insert_event :: Ptr CEventTrackModel -> Ptr TrackPos
        -> Ptr Event.Event -> IO CInt
foreign import ccall unsafe "event_track_model_remove_event"
    c_event_track_model_remove_event :: Ptr CEventTrackModel -> Ptr TrackPos
        -> IO CInt
-- Longer than call-with-current-continuation!

-- | Lazy list of events starting at 'pos' to the end of the track.
get_events_forward :: Track -> TrackPos -> UI [(TrackPos, Event.Event)]
get_events_forward track pos = undefined

-- | Like 'get_events_forward' except from 'pos' to the beginning of the track.
get_events_backward :: Track -> TrackPos -> UI [(TrackPos, Event.Event)]
get_events_backward track pos = undefined

get_attrs :: Track -> IO Attrs
get_attrs track = MVar.readMVar (track_attrs track)
set_attrs :: Track -> Attrs -> IO ()
set_attrs track attrs = MVar.swapMVar (track_attrs track) attrs >> return ()
