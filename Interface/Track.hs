{-# OPTIONS_GHC -XBangPatterns #-}
module Interface.Track
where

import Interface.Ui (send_action)
import qualified Interface.TrackImpl as T
import Interface.TrackImpl (Track, EventList, TrackView)

force = id

create = T.create
