{- | The main attributes of an event are its duration and text.  The starting
    time is actually in the Track.

    Events can display a signal, which will be rendered in the background in
    one of a few ways: a color, or a graph.  TODO The text in the event is
    unicode so you can load and use special glyphs.

    TODO
    The Event has attributes which store arbitrary key-value pairs.  This can
    be used to store the "parent event" of a derivation, for instance.

    The text in events never changes size, even when you zoom in or out.  If an
    Event gets too small for its text, it collapses into a blue chunk, which is
    the standard color for some data that didn't fit.

    The beginning of the Event is marked with a red line.  The text will begin
    slightly below the line, but still try to fit within the event.  If the
    Event end doesn't give room for the text, the text will overlap the line so
    that its bottom touches the bottom of the Event.  If there is no room for
    the text at all, because of other text or the top of the Event, the text
    will disappear and the line will be blue, to mark hidden text.

    No Event may overlap another Event on the same Track.
-}

module Ui.Event where
import qualified Data.Array.IArray as IArray
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as Internal
import Foreign
import Foreign.C

import Ui
import qualified Ui.Color as Color
import qualified Ui.Font as Font


data Event = Event {
    -- This is encoded as UTF8.  Data.Text would probably be more logical, but
    -- I expected it to be sent to the UI much more frequently than modified
    -- from haskell.  Don't access this directly, use 'event_text'.
    -- TODO: or is it?  derivation could put the lie to that.
    event_text :: Text.Text
    , event_duration :: TrackPos
    , event_style :: StyleId
    } deriving (Eq, Show, Read)

-- | Manual event constructor.
event :: String -> TrackPos -> Event
event text dur = Event (Text.pack text) (max (TrackPos 0) dur) default_style

event_string :: Event -> String
event_string = Text.unpack . event_text

set_string :: String -> Event -> Event
set_string s evt = evt { event_text = Text.pack s }

set_duration :: TrackPos -> Event -> Event
set_duration dur event = event { event_duration = max 0 dur }

default_style :: StyleId
default_style = StyleId 0

-- | Static table of styles.  I'd rather store this in App.Config or something,
-- but that would lead to circular imports.  If I want to include it in the
-- static config, I'll have to figure out a way to pass the table down to poke
-- anyway.
style_table :: [(StyleId, Style)]
style_table =
    [ (default_style, Style evt_color default_font)
    ]
    where
    evt_color = Color.rgb 0.9 0.9 0.7

default_font = Font.TextStyle Font.Helvetica [] 9 Color.black

-- | To save space, event styles are explicitly shared by storing them in
-- a table.
newtype StyleId = StyleId Word8
    deriving (Eq, Show, Read)

data Style = Style {
    style_color :: Color
    , style_text :: Font.TextStyle
    } deriving (Eq, Show, Read)

-- * storable

style_array :: IArray.Array Word8 Style
style_array = IArray.array (0, maxBound) ([(i, no_style) | i <- [0..maxBound]])
    IArray.// [(sid, style) | (StyleId sid, style) <- style_table]
    where no_style = Style Color.black default_font

lookup_style :: StyleId -> Style
lookup_style (StyleId style) = style_array IArray.! style

#include "c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Event where
    sizeOf _ = #size Event
    alignment _ = #{alignment Event}
    poke = poke_event
    peek = error "Event peek unimplemented"

poke_event eventp (Event text dur style_id) = do
    let (Style color text_style) = lookup_style style_id
    -- Must be freed by the caller, EventTrackView::draw_area.
    textp <- if Text.null text then return nullPtr
        else unpackCString0 (Encoding.encodeUtf8 text)
    (#poke Event, text) eventp textp
    (#poke Event, duration) eventp dur
    (#poke Event, color) eventp color
    (#poke Event, style) eventp text_style

-- | Unpack the bytestring to a null-terminated cstring, in malloc'd space.
-- ByteString only has an alloca version of this.
unpackCString0 :: ByteString.ByteString -> IO CString
unpackCString0 bs = do
    let (fptr, offset, len) = Internal.toForeignPtr bs
    stringp <- mallocBytes (len + 1)
    withForeignPtr fptr $ \ptr ->
        Internal.memcpy stringp (ptr `plusPtr` offset) (fromIntegral len)
    poke (stringp `plusPtr` len) (0 :: Word8)
    return (castPtr stringp)
