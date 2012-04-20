{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{- | A Track is a container for Events.  A track goes from ScoreTime 0 until
    the end of the last Event.

    TODO
    cached derivation and realization (depending on deriver scope)
    modified event map, for derivation (old trackpos -> new trackpos)
-}
module Ui.TrackC (with_track) where
import Foreign
import Foreign.C

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.Then as Then

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Style as Style
import qualified Ui.Track as Track
import qualified Ui.Util as Util

import qualified Perform.Signal as Signal
import Types


#include "Ui/c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | Since converting a Track requires both a track and merged events, poke
-- needs two args.  So keep it out of Storable to prevent accidental use of
-- 'with'.
with_track :: Track.Track -> Event.SetStyle -> [Events.Events]
    -> (Ptr Track.Track -> IO a) -> IO a
with_track track set_style event_lists f = allocaBytes size $ \trackp -> do
    -- Wrap style is customizable per track, but I'll hardcode it for now.
    (#poke EventTrackConfig, text_wrap) trackp
        ((#const EventTrackConfig::wrap) :: CInt)
    (#poke EventTrackConfig, bg_color) trackp (Track.track_bg track)
    poke_find_events trackp (set_style (Track.track_title track))
        (Track.track_events track : event_lists)
    (#poke EventTrackConfig, render) trackp (Track.track_render track)
    initialize_track_signal ((#ptr EventTrackConfig, track_signal) trackp)
    f trackp
    where
    size = #size EventTrackConfig
    -- allocaBytesAligned is not exported from Foreign.Marshal.Alloc
    -- align = #{alignment EventTrackConfig}

type SetStyle = ScoreTime -> Event.Event -> Style.StyleId

poke_find_events :: Ptr Track.Track -> SetStyle -> [Events.Events]
    -> IO ()
poke_find_events trackp set_style event_lists = do
    let time_end = maximum (0 : map Events.time_end event_lists)
    find_events <- make_find_events set_style event_lists
    (#poke EventTrackConfig, find_events) trackp find_events
    (#poke EventTrackConfig, time_end) trackp time_end

make_find_events :: SetStyle -> [Events.Events] -> IO (FunPtr FindEvents)
make_find_events set_style events = Util.make_fun_ptr "find_events" $
    c_make_find_events (cb_find_events set_style events)

instance Storable Track.RenderConfig where
    sizeOf _ = #size RenderConfig
    alignment _ = #{alignment RenderConfig}
    peek _ = error "RenderConfig peek unimplemented"
    poke = poke_render_config

poke_render_config :: Ptr Track.RenderConfig -> Track.RenderConfig -> IO ()
poke_render_config configp (Track.RenderConfig style color) = do
    (#poke RenderConfig, style) configp (encode_style style)
    (#poke RenderConfig, color) configp color

instance Storable Track.TrackSignal where
    sizeOf _ = #size TrackSignal
    alignment _ = #{alignment TrackSignal}
    peek _ = error "TrackSignal peek unimplemented"
    poke = poke_track_signal

-- | This does a memcpy to marshal the signal for c++.  I could pass the
-- pointer directly, but then I would have to arrange for haskell and c++
-- to coordinate its lifespan.  I believe I could hold the ForeignPtr in
-- a FunPtr which is then manually deleted from c++ via the usual finalizer.
-- If that failed, I could use a StablePtr with a little more work.
--
-- However, memcpy is quite fast.  I tested 0.01s for 32mb, which is a good
-- upper bound.  It's 87m of 0.1s pitch signal * 8 tracks * 4 controls, which
-- is a lot.
--
-- Copying over the valname list when it probably never changes galls a little.
-- However, as with the signal above, a copy is probably fast enough and is
-- much simpler wrt storage, especially because there are variable length
-- strings involved.  I shouldn't use static storage because customizing pitch
-- sig rendering by messing with ValNames seems like a useful thing to do.
poke_track_signal :: Ptr Track.TrackSignal -> Track.TrackSignal -> IO ()
poke_track_signal tsigp (Track.TrackSignal sig shift stretch scale_map) = do
    (#poke TrackSignal, shift) tsigp shift
    (#poke TrackSignal, stretch) tsigp stretch
    poke_sig sig
    poke_scale_map scale_map
    where
    poke_sig sig = do
        Signal.with_ptr sig $ \sigp len -> do
            -- TODO copy an empty signal as a null ptr
            destp <- mallocArray len
            copyArray destp sigp len
            (#poke TrackSignal, signal) tsigp destp
            (#poke TrackSignal, length) tsigp len
        -- Calculated by c++, in c_interface.cc.  I'd rather do it here,
        -- but I'm worried all those peeks will generate garbage.
        (#poke TrackSignal, val_min) tsigp (-1 :: CDouble)
        (#poke TrackSignal, val_max) tsigp (-1 :: CDouble)
    poke_scale_map (Just (Track.ScaleMap val_names@(_:_))) = do
        -- As with the char * inside, c++ is expected to free this.
        val_namesp <- newArray val_names
        (#poke TrackSignal, val_names) tsigp val_namesp
        (#poke TrackSignal, val_names_length) tsigp (length val_names)
    poke_scale_map _ = do
        (#poke TrackSignal, val_names) tsigp nullPtr
        (#poke TrackSignal, val_names_length) tsigp (0 :: CInt)

-- | Objects constructed from haskell don't have their constructors run,
-- so make sure it doesn't have garbage.
initialize_track_signal :: Ptr Track.TrackSignal -> IO ()
initialize_track_signal tsigp = do
    (#poke TrackSignal, signal) tsigp nullPtr
    (#poke TrackSignal, length) tsigp (0 :: CInt)
    (#poke TrackSignal, val_names) tsigp nullPtr
    (#poke TrackSignal, val_names_length) tsigp (0 :: CInt)


encode_style :: Track.RenderStyle -> (#type RenderConfig::RenderStyle)
encode_style style = case style of
    Track.NoRender -> (#const RenderConfig::render_none)
    Track.Line -> (#const RenderConfig::render_line)
    Track.Filled -> (#const RenderConfig::render_filled)

-- typedef int (*FindEvents)(ScoreTime *start_pos, ScoreTime *end_pos,
--         ScoreTime **ret_tps, Event **ret_events, int **ret_ranks);
type FindEvents = Ptr ScoreTime -> Ptr ScoreTime -> Ptr (Ptr ScoreTime)
    -> Ptr (Ptr Event.Event) -> Ptr (Ptr CInt) -> IO Int

cb_find_events :: SetStyle -> [Events.Events] -> FindEvents
cb_find_events set_style event_lists startp endp ret_tps ret_events
        ret_ranks = do
    start <- peek startp
    end <- peek endp
    let (tps, evts, ranks) = unzip3 $ Seq.sort_on key
            [ (pos, style pos evt, rank)
            | (rank, events) <- zip [0..] event_lists
            , (pos, evt) <- in_range start end events
            ]
        key (pos, _, rank) = (pos, rank)
        style pos evt = evt { Event.event_style = set_style pos evt }
    unless (null evts) $ do
        -- Calling c++ is responsible for freeing this.
        poke ret_tps =<< newArray tps
        poke ret_events =<< newArray evts
        poke ret_ranks =<< newArray ranks
    return (length evts)
    where
    -- Get everything in the half-open range, plus one event before and after.
    -- The drawing code needs to know if the previous event text would overlap
    -- the first one.  The same goes for the last event, in case it has
    -- negative duration and the text goes above.
    --
    -- Almost, but not quite the same as 'Events.in_range_around'.
    in_range start end events =
        take 1 pre ++ Then.takeWhile1 ((<=end) . fst) post
        where (pre, post) = Events.split start events

foreign import ccall "wrapper"
    c_make_find_events :: FindEvents -> IO (FunPtr FindEvents)

instance Storable Track.ValName where
    sizeOf _ = #size ValName
    alignment _ = #{alignment ValName}
    peek _ = error "ValName peek unimplemented"
    poke dp (Track.ValName (val, name)) = do
        -- C is expected to free this!
        namep <- newCString name
        (#poke ValName, val) dp val
        (#poke ValName, name) dp namep
