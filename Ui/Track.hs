{-# OPTIONS_GHC -XBangPatterns #-}
module Ui.Track (
    -- * Track model
    Track, create
    -- * Model modification
    , get_attrs, set_attrs

    -- ** Events
    , insert_event, remove_event
    -- , event_at, advance, rewind
) where

import Ui.Ui (send_action)
import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Event as Event

import qualified Ui.TrackImpl as T
import Ui.TrackImpl (Track)


-- | Create a new empty event track.
create :: Color.Color -> UI Track
create = T.create

-- ** Model modification

get_attrs :: Track -> UI Attrs
get_attrs = T.get_attrs
set_attrs = T.set_attrs

insert_event track pos event = send_action (T.insert_event track pos event)
remove_event track pos = send_action (T.remove_event track pos)
