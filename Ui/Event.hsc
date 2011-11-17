{- | The main attributes of an event are its duration and text.  The starting
    time is actually in the Track.

    Events can display a signal, which will be rendered in the background in
    one of a few ways: a color, or a graph.

    The text in events never changes size, even when you zoom in or out.  If
    an Event gets too small for its text, it collapses into a blue chunk,
    which is the standard color for some data that didn't fit.

    The beginning of the Event is marked with a red line.  The text will begin
    slightly below the line, but still try to fit within the event.  If the
    Event end doesn't give room for the text, the text will overlap the line
    so that its bottom touches the bottom of the Event.  If there is no room
    for the text at all, because of other text or the top of the Event, the
    text will disappear and the line will be blue, to mark hidden text.

    No Event may overlap another Event on the same Track.

    TODO
    The Event has attributes which store arbitrary key-value pairs.  This can
    be used to store the "parent event" of a derivation, for instance.
-}
module Ui.Event where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.ByteString.UTF8 as UTF8
import Foreign
import Foreign.C

import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Style as Style

import qualified App.Config as Config
import Types


data Event = Event {
    event_bs :: B.ByteString
    , event_duration :: ScoreTime
    , event_style :: Style.StyleId
    } deriving (Eq, Show, Read)

instance DeepSeq.NFData Event where
    rnf (Event bs dur style) = bs `seq` dur `seq` style `seq` ()

-- | Manual event constructor.
event :: String -> ScoreTime -> Event
event text dur = Event (UTF8.fromString text) dur Config.default_style

event_string :: Event -> String
event_string = UTF8.toString . event_bs

set_string :: String -> Event -> Event
set_string s evt = evt { event_bs = UTF8.fromString s }

set_duration :: ScoreTime -> Event -> Event
set_duration dur event = event { event_duration = dur }

modify_duration :: (ScoreTime -> ScoreTime) -> Event -> Event
modify_duration f evt = set_duration (f (event_duration evt)) evt

-- | 0 is considered both positive and negative because they're ambiguous.
-- For example, Track._split_range which includes them in both ends.
is_positive, is_negative :: Event -> Bool
is_positive = not . is_negative
is_negative = is_negative_duration . event_duration

is_negative_duration, is_positive_duration :: ScoreTime -> Bool
is_negative_duration d = d < 0 || isNegativeZero (ScoreTime.to_double d)
is_positive_duration = not . is_negative_duration


-- * storable

#include "Ui/c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Event where
    sizeOf _ = #size Event
    alignment _ = #{alignment Event}
    poke = poke_event
    peek = error "Event peek unimplemented"

poke_event eventp (Event text dur (Style.StyleId style_id)) = do
    -- Must be freed by the caller, EventTrackView::draw_area.
    textp <- if B.null text then return nullPtr
        else unpackCString0 text
    (#poke Event, text) eventp textp
    (#poke Event, duration) eventp dur
    (#poke Event, style_id) eventp style_id

-- | Unpack the bytestring to a null-terminated cstring, in malloc'd space.
-- ByteString only has an alloca version of this.
unpackCString0 :: B.ByteString -> IO CString
unpackCString0 bs = Unsafe.unsafeUseAsCStringLen bs $ \(str, len) -> do
    new <- mallocBytes (len+1)
    copyBytes new str len
    poke (new `plusPtr` len) (0 :: Word8)
    return new
