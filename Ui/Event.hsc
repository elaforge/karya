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
import Foreign
import Foreign.C

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Font as Font


data Event = Event {
    event_text :: String
    , event_duration :: TrackPos
    , event_style :: StyleId
    } deriving (Eq, Show, Read)
event text dur = Event text (max (TrackPos 0) dur) default_style

default_style :: StyleId
default_style = StyleId 0

-- | Static table of styles.  I'd rather store this in App.Config or something,
-- but that would lead to circular imports.  If I want to include it in the
-- static config, I'll have to figure out a way to pass the table down to poke
-- anyway.
style_table :: [(StyleId, Style)]
style_table =
    [ (default_style, Style (Color.rgb 0.9 0.9 0.7) default_font False)
    ]
default_font = Font.TextStyle Font.Helvetica [] 9 Color.black

-- | To save space, event styles are explicitly shared by storing them in
-- a table.
newtype StyleId = StyleId Word8
    deriving (Eq, Show, Read)

data Style = Style {
    style_color :: Color
    , style_text :: Font.TextStyle
    , style_align_to_bottom :: Bool
    } deriving (Eq, Show, Read)

-- * storable

style_array :: IArray.Array Word8 Style
style_array = IArray.array (0, maxBound) ([(i, no_style) | i <- [0..maxBound]])
    IArray.// [(sid, style) | (StyleId sid, style) <- style_table]
    where no_style = Style Color.black default_font False

lookup_style :: StyleId -> Style
lookup_style (StyleId style) = style_array IArray.! style

#include "c_interface.h"

instance Storable Event where
    sizeOf _ = #size Event
    alignment _ = undefined
    poke = poke_event

poke_event eventp (Event text dur style_id) = do
    let (Style color text_style align) = lookup_style style_id
    -- Must be freed by the caller, EventTrackView::draw_area.
    textp <- newCString text
    (#poke Event, text) eventp textp
    (#poke Event, duration) eventp dur
    (#poke Event, color) eventp color
    (#poke Event, style) eventp text_style
    (#poke Event, align_to_bottom) eventp align
