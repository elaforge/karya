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

module Ui.TrackC where
import Control.Monad
import Foreign

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.Track as Track


#include "c_interface.h"

instance Storable Track.Track where
    sizeOf _ = #size EventTrackConfig
    alignment _ = undefined
    poke = poke_track

poke_track trackp (Track.Track
        { Track.track_events = events
        , Track.track_bg_color = bg
        })
    = do
        find_events <- make_find_events events
        last_track_pos <- make_last_track_pos events
        (#poke EventTrackConfig, bg_color) trackp bg
        (#poke EventTrackConfig, find_events) trackp find_events
        (#poke EventTrackConfig, last_track_pos) trackp last_track_pos

make_find_events events = c_make_find_events (cb_find_events events)
make_last_track_pos events = c_make_last_track_pos (cb_last_track_pos events)

-- typedef int (*FindEvents)(TrackPos *start_pos, TrackPos *end_pos,
--         TrackPos **ret_tps, Event **ret_events);
-- typedef int (*LastTrackPos)(TrackPos *last);
type FindEvents = Ptr TrackPos -> Ptr TrackPos -> Ptr (Ptr TrackPos)
    -> Ptr (Ptr Event.Event) -> IO Int
type LastTrackPos = Ptr TrackPos -> IO Int

cb_find_events :: Track.TrackEvents -> FindEvents
cb_find_events events startp endp ret_tps ret_events = do
    start <- peek startp
    end <- peek endp
    let (bwd, fwd) = Track.events_at start events
        (until_end, _rest) = break ((>= end) . fst) fwd
        found_events = take 1 bwd ++ until_end
    when (not (null found_events)) $ do
        -- Calling c++ is responsible for freeing this.
        tp_array <- newArray (map fst found_events)
        event_array <- newArray (map snd found_events)
        poke ret_tps tp_array
        poke ret_events event_array
    return (length found_events)

cb_last_track_pos :: Track.TrackEvents -> LastTrackPos
cb_last_track_pos events posp = case Track.last_event events of
    Nothing -> return 0
    Just (pos, event) -> poke posp (pos + Event.event_duration event)
        >> return 1

foreign import ccall "wrapper"
    c_make_find_events :: FindEvents -> IO (FunPtr FindEvents)
foreign import ccall "wrapper"
    c_make_last_track_pos :: LastTrackPos -> IO (FunPtr LastTrackPos)
