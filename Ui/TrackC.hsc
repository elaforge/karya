-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | A Track is a container for Events.  A track goes from ScoreTime 0 until
    the end of the last Event.
-}
module Ui.TrackC (with_track) where
import Util.ForeignC
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Style as Style
import qualified Ui.Track as Track
import qualified Ui.Util as Util

import qualified Perform.Signal as Signal
import Types
import Global


#include "Ui/c_interface.h"

-- | Since converting a Track requires both a track and merged events, poke
-- needs two args.  So keep it out of Storable to prevent accidental use of
-- 'with'.
with_track :: Track.Track -> Track.SetStyle -> [Events.Events]
    -> (Ptr Track.Track -> IO a) -> IO a
with_track track (track_bg, event_style) merged_events f =
    allocaBytesAligned size align $ \trackp -> do
        (#poke EventTrackConfig, bg_color) trackp (track_bg track)
        poke_find_events trackp (event_style (Track.track_title track))
            (Track.track_events track : merged_events)
        (#poke EventTrackConfig, render) trackp (Track.track_render track)
        initialize_track_signal ((#ptr EventTrackConfig, track_signal) trackp)
        f trackp
    where
    size = #size EventTrackConfig
    align = alignment (0 :: CDouble)

type EventStyle = Event.Event -> Style.StyleId

poke_find_events :: Ptr Track.Track -> EventStyle -> [Events.Events] -> IO ()
poke_find_events trackp event_style event_lists = do
    let time_end = maximum (0 : map Events.time_end event_lists)
    find_events <- make_find_events event_style event_lists
    (#poke EventTrackConfig, find_events) trackp find_events
    (#poke EventTrackConfig, time_end) trackp time_end

make_find_events :: EventStyle -> [Events.Events] -> IO (FunPtr FindEvents)
make_find_events event_style event_lists = Util.make_fun_ptr "find_events" $
    c_make_find_events (cb_find_events event_style event_lists)

instance CStorable Track.RenderConfig where
    sizeOf _ = #size RenderConfig
    alignment _ = alignment (0 :: CDouble)
    peek _ = error "RenderConfig peek unimplemented"
    poke = poke_render_config

poke_render_config :: Ptr Track.RenderConfig -> Track.RenderConfig -> IO ()
poke_render_config configp (Track.RenderConfig style color) = do
    (#poke RenderConfig, style) configp (encode_style style)
    (#poke RenderConfig, color) configp color

instance CStorable Track.TrackSignal where
    sizeOf _ = #size TrackSignal
    alignment _ = alignment (0 :: CDouble)
    peek _ = error "TrackSignal peek unimplemented"
    poke = poke_track_signal

-- | This does a memcpy to marshal the signal for c++.  I could pass the
-- pointer directly, but then I would have to arrange for haskell and c++
-- to coordinate its lifespan.  I believe I could hold the ForeignPtr in
-- a FunPtr which is then manually deleted from c++ via the usual finalizer.
-- If that failed, I could use a StablePtr with a little more work.
--
-- However, memcpy is quite fast.  At 0.01s sampling rate, one minute of
-- a control track is 8 bytes/Double * 600 = 4.6kb.  So * 60 minutes * 8 tracks
-- = 2.25mb.  Since I tested 0.01s to copy 32mb, this should totally be fast
-- enough.
poke_track_signal :: Ptr Track.TrackSignal -> Track.TrackSignal -> IO ()
poke_track_signal tsigp (Track.TrackSignal sig shift stretch) = do
    (#poke TrackSignal, shift) tsigp shift
    (#poke TrackSignal, stretch) tsigp stretch
    initialize_track_signal tsigp
    (destp, len) <- if Signal.null sig then return (nullPtr, 0)
        else Signal.with_ptr sig $ \sigp len -> do
            destp <- mallocArray len
            copyArray destp sigp len
            return (destp, len)
    (#poke TrackSignal, signal) tsigp destp
    (#poke TrackSignal, length) tsigp (Util.c_int len)

    -- Calculated by c++, in c_interface.cc.  I'd rather do it here,
    -- but I'm worried all those peeks will generate garbage.
    (#poke TrackSignal, val_min) tsigp (-1 :: CDouble)
    (#poke TrackSignal, val_max) tsigp (-1 :: CDouble)

-- | Objects constructed from haskell don't have their constructors run,
-- so make sure it doesn't have garbage.
initialize_track_signal :: Ptr Track.TrackSignal -> IO ()
initialize_track_signal tsigp = do
    (#poke TrackSignal, signal) tsigp nullPtr
    (#poke TrackSignal, length) tsigp (0 :: CInt)

encode_style :: Track.RenderStyle -> (#type RenderConfig::RenderStyle)
encode_style style = case style of
    Track.NoRender -> (#const RenderConfig::render_none)
    Track.Line {} -> (#const RenderConfig::render_line)
    Track.Filled {} -> (#const RenderConfig::render_filled)

-- typedef int (*FindEvents)(ScoreTime *start_pos, ScoreTime *end_pos,
--         Event **ret_events, int **ret_ranks);
type FindEvents = Ptr ScoreTime -> Ptr ScoreTime
    -> Ptr (Ptr Event.Event) -> Ptr (Ptr CInt) -> IO Int

cb_find_events :: EventStyle -> [Events.Events] -> FindEvents
cb_find_events event_style event_lists startp endp ret_events ret_ranks = do
    start <- peek startp
    end <- peek endp
    let (events, ranks) = unzip $ Seq.merge_lists key $
            zipWith (\rank -> map (, rank)) [0..] $
            map (map set_style . in_range start end) event_lists
        key (event, rank) = (Event.start event, rank)
        set_style event = Event.modify_style (const (event_style event)) event
    unless (null events) $ do
        -- Calling c++ is responsible for freeing these.
        poke ret_events =<< newArray events
        poke ret_ranks =<< newArray ranks
    return (length events)
    where
    -- Get everything in the inclusive range, plus one event before and after.
    -- The drawing code needs to know if the previous event text would overlap
    -- the first one.  The same goes for the last event, in case it has
    -- negative duration and the text goes above.
    --
    -- Almost, but not quite the same as 'Events.in_range_around'.
    in_range start end events =
        take 1 pre ++ Then.takeWhile1 ((<=end) . Event.start) post
        where (pre, post) = Events.split_lists start events

foreign import ccall "wrapper"
    c_make_find_events :: FindEvents -> IO (FunPtr FindEvents)
