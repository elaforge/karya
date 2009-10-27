{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
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

import qualified Util.Array as Array
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.Track as Track
import qualified Ui.Util as Util


#include "c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- Since converting a Track requires both a track and merged events, poke needs
-- two args.  So keep it out of Storable to prevent accidental use of 'with'.
-- instance Storable Track.Track where
--     sizeOf _ = #size EventTrackConfig
--     alignment _ = #{alignment EventTrackConfig}

poke_track trackp (Track.Track _ _ bg render) = do
    (#poke EventTrackConfig, bg_color) trackp bg
    (#poke EventTrackConfig, render) trackp render

poke_find_events :: Ptr Track.Track -> [Track.TrackEvents] -> IO ()
poke_find_events trackp event_lists = do
    let time_end = maximum (0 : map Track.time_end event_lists)
    find_events <- make_find_events event_lists
    (#poke EventTrackConfig, find_events) trackp find_events
        -- =<< make_find_events event_lists
    (#poke EventTrackConfig, time_end) trackp time_end

with_track :: Track.Track -> [Track.TrackEvents] -> (Ptr Track.Track -> IO a)
    -> IO a
with_track track event_lists f = allocaBytes size $ \trackp -> do
    poke_track trackp track
    poke_find_events trackp (Track.track_events track : event_lists)
    f trackp
    where
    size = #size EventTrackConfig
    -- allocaBytesAligned is not exported from Foreign.Marshal.Alloc
    -- align = #{alignment EventTrackConfig}

insert_render_samples :: Ptr Track.Track -> Track.Samples -> IO ()
insert_render_samples trackp samples = do
    find_samples <- make_find_samples samples
    let renderp = (#ptr EventTrackConfig, render) trackp
    (#poke RenderConfig, find_samples) renderp find_samples

make_find_events :: [Track.TrackEvents] -> IO (FunPtr FindEvents)
make_find_events events = Util.make_fun_ptr "find_events" $
    c_make_find_events (cb_find_events events)

make_find_samples :: Track.Samples -> IO (FunPtr FindSamples)
make_find_samples samples = Util.make_fun_ptr "find_samples" $
    c_make_find_samples (cb_find_samples samples)

instance Storable Track.RenderConfig where
    sizeOf _ = #size RenderConfig
    alignment _ = #{alignment RenderConfig}
    peek = error "RenderConfig peek unimplemented"
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
--         TrackPos **ret_tps, Event **ret_events, int **ret_ranks);
type FindEvents = Ptr TrackPos -> Ptr TrackPos -> Ptr (Ptr TrackPos)
    -> Ptr (Ptr Event.Event) -> Ptr (Ptr CInt) -> IO Int

cb_find_events :: [Track.TrackEvents] -> FindEvents
cb_find_events event_lists startp endp ret_tps ret_events ret_ranks = do
    start <- peek startp
    end <- peek endp
    let key (pos, _, rank) = (pos, rank)
    let (tps, evts, ranks) = unzip3 $ Seq.sort_on key [ (pos, evt, rank)
            | (rank, events) <- zip [0..] event_lists
            , (pos, evt) <- find_events start end events ]
    unless (null evts) $ do
        -- Calling c++ is responsible for freeing this.
        poke ret_tps =<< newArray tps
        poke ret_events =<< newArray evts
        poke ret_ranks =<< newArray ranks
    return (length evts)

find_events :: TrackPos -> TrackPos -> Track.TrackEvents -> [Track.PosEvent]
find_events start end events = take 1 bwd ++ until_end
    where
    -- Take 1 from bwd to get the event overlapping the beginning of the
    -- damaged area.
    (bwd, fwd) = Track.events_at start events
    until_end = takeWhile ((<end) . fst) fwd

-- typedef int (*FindSamples)(TrackPos *start_pos, TrackPos *end_pos,
--         TrackPos **ret_tps, double **ret_samples);
type FindSamples = Ptr TrackPos -> Ptr TrackPos -> Ptr (Ptr TrackPos)
    -> Ptr (Ptr CDouble) -> IO Int

cb_find_samples :: Track.Samples -> FindSamples
cb_find_samples (Track.Samples samples) startp endp ret_tps ret_samples = do
    start <- peek startp
    end <- peek endp
    -- From one before start to one after end.
    let start_i = max 0 (Array.bsearch_on fst samples start - 1)
        max_i = snd (IArray.bounds samples)
        (elts, rest) = break ((>=end) . fst) (map (samples!) [start_i..max_i])
        -- Get one sample past the cutoff so it can draw the slope properly.
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
