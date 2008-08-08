{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -XEmptyDataDecls #-}
{- | A Track is a container for Events.  A track goes from TrackPos 0 until
    the end of the last Event.

    TODO
    cached derivation and realization (depending on deriver scope)
    modified event map, for derivation (old trackpos -> new trackpos)
-}
module Ui.TrackC where
import Control.Monad
import Data.Array.IArray ((!))
import qualified Data.Array.IArray as IArray
import Foreign
import Foreign.C

import qualified Util.Data

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.Track as Track
import qualified Ui.Util as Util


#include "c_interface.h"

instance Storable Track.Track where
    sizeOf _ = #size EventTrackConfig
    alignment _ = undefined
    poke = poke_track

poke_track trackp (Track.Track
        { Track.track_events = events
        , Track.track_bg = bg
        , Track.track_render = render
        })
    = do
        find_events <- make_find_events events
        let time_end = Track.time_end events
        (#poke EventTrackConfig, bg_color) trackp bg
        (#poke EventTrackConfig, find_events) trackp find_events
        (#poke EventTrackConfig, time_end) trackp time_end
        (#poke EventTrackConfig, render) trackp render

insert_render_samples :: Ptr Track.Track -> Track.Samples -> IO ()
insert_render_samples trackp samples = do
    find_samples <- make_find_samples samples
    let renderp = (#ptr EventTrackConfig, render) trackp
    (#poke RenderConfig, find_samples) renderp find_samples

make_find_events events = c_make_find_events (cb_find_events events)
make_find_samples samples = c_make_find_samples (cb_find_samples samples)

instance Storable Track.RenderConfig where
    sizeOf _ = #size RenderConfig
    alignment _ = undefined
    poke = poke_render_config

poke_render_config configp (Track.RenderConfig style color) = do
    (#poke RenderConfig, style) configp (encode_style style)
    (#poke RenderConfig, color) configp color

encode_style :: Track.RenderStyle -> (#type RenderConfig::RenderStyle)
encode_style style = case style of
    Track.NoRender -> (#const RenderConfig::render_none)
    Track.Line -> (#const RenderConfig::render_line)
    Track.Filled -> (#const RenderConfig::render_filled)

-- typedef int (*FindEvents)(TrackPos *start_pos, TrackPos *end_pos,
--         TrackPos **ret_tps, Event **ret_events);
type FindEvents = Ptr TrackPos -> Ptr TrackPos -> Ptr (Ptr TrackPos)
    -> Ptr (Ptr Event.Event) -> IO Int

-- typedef int (*FindSamples)(TrackPos *start_pos, TrackPos *end_pos,
--         TrackPos **ret_tps, double **ret_samples);
type FindSamples = Ptr TrackPos -> Ptr TrackPos -> Ptr (Ptr TrackPos)
    -> Ptr (Ptr CDouble) -> IO Int

cb_find_events :: Track.TrackEvents -> FindEvents
cb_find_events events startp endp ret_tps ret_events = do
    start <- peek startp
    end <- peek endp
    let (bwd, fwd) = Track.events_at start events
        (until_end, _rest) = break ((>= end) . fst) fwd
        found_events = take 1 bwd ++ until_end
    unless (null found_events) $ do
        -- Calling c++ is responsible for freeing this.
        tp_array <- newArray (map fst found_events)
        event_array <- newArray (map snd found_events)
        poke ret_tps tp_array
        poke ret_events event_array
    return (length found_events)

cb_find_samples :: Track.Samples -> FindSamples
cb_find_samples (Track.Samples samples) startp endp ret_tps ret_samples = do
    start <- peek startp
    end <- peek endp
    -- From one before start to one after end.
    let start_i = max 0 (Util.Data.bsearch_on fst samples start - 1)
        max_i = snd (IArray.bounds samples)
        (elts, rest) = break ((>=end) . fst) (map (samples!) [start_i..max_i])
        found = elts ++ take 1 rest
    -- putStrLn $ "go find " ++ show start_i ++ "--" ++ show max_i
    unless (null found) $ do
        tp_array <- newArray (map fst found)
        sample_array <- newArray (map (Util.c_double . snd) found)
        poke ret_tps tp_array
        poke ret_samples sample_array
    -- putStrLn $ "find samples " ++ show start ++ "--" ++ show end
    --     ++ ": " ++ show (length found)
    --     ++ "\n" ++ show found
    return (length found)

foreign import ccall "wrapper"
    c_make_find_events :: FindEvents -> IO (FunPtr FindEvents)
foreign import ccall "wrapper"
    c_make_find_samples :: FindSamples -> IO (FunPtr FindSamples)
