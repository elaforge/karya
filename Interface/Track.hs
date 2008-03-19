{-# OPTIONS_GHC -XBangPatterns #-}
module Interface.Track (
    -- * Track model
    Track, create
    -- ** Model modification
    , get_attrs, set_attrs

    -- * Events
    , insert_event, remove_event
    , get_events_forward, get_events_backward
) where

import Interface.Ui (send_action)
import qualified Interface.TrackImpl as T
import Interface.TrackImpl (Track)

force = id

create = T.create
get_attrs = T.get_attrs
set_attrs = T.set_attrs

insert_event track pos event = send_action (T.insert_event track pos event)
remove_event track pos = send_action (T.remove_event track pos)

get_events_forward track pos = send_action (T.get_events_forward track pos)
get_events_backward track pos = send_action (T.get_events_backward track pos)
