-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | A Track is a container for Events.  A track goes from ScoreTime 0 until
    the end of the last Event.
-}
module Ui.TrackC (withTrack) where
import ForeignC
import qualified Util.FFI as FFI
import qualified Util.Lists as Lists
import qualified Util.Then as Then

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Style as Style
import qualified Ui.Track as Track

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types
import Global


#include "Ui/c_interface.h"

-- | Since converting a Track requires both a track and merged events, poke
-- needs two args.  So keep it out of Storable to prevent accidental use of
-- 'with'.
withTrack :: Track.Track -> Track.SetStyle -> [Events.Events]
    -> (Ptr Track.Track -> IO a) -> IO a
withTrack track (trackBg, eventStyle) mergedEvents f =
    allocaBytesAligned size align $ \trackp -> do
        (#poke EventTrackConfig, bg_color) trackp (trackBg track)
        pokeFindEvents trackp (eventStyle (Track.trackTitle track))
            (Track.trackEvents track : mergedEvents)
        (#poke EventTrackConfig, render) trackp (Track.trackRender track)
        initializeTrackSignal ((#ptr EventTrackConfig, track_signal) trackp)
        f trackp
    where
    size = #size EventTrackConfig
    align = alignment (0 :: CDouble)

type EventStyle = Event.Event -> Style.StyleId

pokeFindEvents :: Ptr Track.Track -> EventStyle -> [Events.Events] -> IO ()
pokeFindEvents trackp eventStyle eventLists = do
    let timeEnd = maximum (0 : map Events.timeEnd eventLists)
    findEvents <- makeFindEvents eventStyle eventLists
    (#poke EventTrackConfig, find_events) trackp findEvents
    (#poke EventTrackConfig, time_end) trackp timeEnd

makeFindEvents :: EventStyle -> [Events.Events] -> IO (FunPtr FindEvents)
makeFindEvents eventStyle eventLists = FFI.makeFunPtr "find_events" $
    cMakeFindEvents (cbFindEvents eventStyle eventLists)

instance CStorable Track.RenderConfig where
    sizeOf _ = #size RenderConfig
    alignment _ = alignment (0 :: CDouble)
    peek _ = error "RenderConfig peek unimplemented"
    poke = pokeRenderConfig

pokeRenderConfig :: Ptr Track.RenderConfig -> Track.RenderConfig -> IO ()
pokeRenderConfig configp (Track.RenderConfig style color) = do
    (#poke RenderConfig, style) configp (encodeStyle style)
    (#poke RenderConfig, color) configp color

instance CStorable Track.TrackSignal where
    sizeOf _ = #size TrackSignal
    alignment _ = alignment (0 :: CDouble)
    peek _ = error "TrackSignal peek unimplemented"
    poke = pokeTrackSignal

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
pokeTrackSignal :: Ptr Track.TrackSignal -> Track.TrackSignal -> IO ()
pokeTrackSignal tsigp (Track.TrackSignal sig shift stretch) = do
    initializeTrackSignal tsigp
    (offset, destp, len) <- if Signal.null sig then return (0, nullPtr, 0)
        else Signal.withPtr sig $ \offset sigp len -> do
            destp <- mallocArray len
            copyArray destp sigp len
            return (offset, destp, len)
    -- This TrackSignal's signal is actually in ScoreTime.
    (#poke TrackSignal, shift) tsigp (shift + RealTime.toScore offset)
    (#poke TrackSignal, stretch) tsigp stretch
    (#poke TrackSignal, signal) tsigp destp
    (#poke TrackSignal, length) tsigp (FFI.cInt len)

    -- Calculated by c++, in c_interface.cc.  I'd rather do it here,
    -- but I'm worried all those peeks will generate garbage.
    (#poke TrackSignal, val_min) tsigp (-1 :: CDouble)
    (#poke TrackSignal, val_max) tsigp (-1 :: CDouble)

-- | Objects constructed from haskell don't have their constructors run,
-- so make sure it doesn't have garbage.
initializeTrackSignal :: Ptr Track.TrackSignal -> IO ()
initializeTrackSignal tsigp = do
    (#poke TrackSignal, signal) tsigp nullPtr
    (#poke TrackSignal, length) tsigp (0 :: CInt)

encodeStyle :: Track.RenderStyle -> (#type RenderConfig::RenderStyle)
encodeStyle style = case style of
    Track.NoRender -> (#const RenderConfig::render_none)
    Track.Line {} -> (#const RenderConfig::render_line)
    Track.Filled {} -> (#const RenderConfig::render_filled)

-- typedef int (*FindEvents)(
--     const ScoreTime *start_pos, const ScoreTime *end_pos,
--     Event **ret_events, int **ret_ranks);
type FindEvents = Ptr ScoreTime -> Ptr ScoreTime
    -> Ptr (Ptr Event.Event) -> Ptr (Ptr CInt) -> IO Int

cbFindEvents :: EventStyle -> [Events.Events] -> FindEvents
cbFindEvents eventStyle eventLists startp endp retEvents retRanks = do
    start <- peek startp
    end <- peek endp
    let (events, ranks) = unzip $ Lists.mergeLists key $
            zipWith (\rank -> map (, rank)) [0..] $
            map (map setStyle . inRange start end) eventLists
        key (event, rank) = (Event.start event, rank)
        setStyle event = Event.style_ #= eventStyle event $ event
    unless (null events) $ do
        -- Calling c++ is responsible for freeing these.
        poke retEvents =<< newArray events
        poke retRanks =<< newArray ranks
    return (length events)
    where
    -- Get everything in the inclusive range, plus one event before and after.
    -- The drawing code needs to know if the previous event text would overlap
    -- the first one.  The same goes for the last event, in case it has
    -- negative duration and the text goes above.
    --
    -- Almost, but not quite the same as 'Events.in_range_around'.
    inRange start end events =
        take 1 pre ++ Then.takeWhile1 ((<=end) . Event.start) post
        where (pre, post) = Events.splitLists start events

foreign import ccall "wrapper"
    cMakeFindEvents :: FindEvents -> IO (FunPtr FindEvents)
