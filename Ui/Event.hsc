{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{-
An Event has a duration.  The starting time is actually in the Track.  It has
a title and various subevents.  A subevent is a bit of text at a TrackPos
offset.  An Event is displayed as a block with lines indicating start and end
points.  Each sub also has a line.

Events can display a signal, which will be rendered in the background in one of
a few ways: a color, or a graph.  The text in the event is unicode so you can
load and use special glyphs.  It can be edited via keyboard and mouse, and can
have its own selections (independent of the block-level selections).  The
beginning and duration as well as sub position should be draggable with the
mouse, with optional snapping to ruler points.

The Event has attributes which store arbitrary key-value pairs.  This can be
used to store the "parent event" of a derivation, for instance.

The text in events never changes size, even when you zoom in or out.  If an
Event gets too small for its text, it collapses into a blue chunk, the standard symbol for some data that didn't fit.

The beginning of the Event and each Sub's position is marked with a red line.
The text will begin slightly below the line, but still try to fit within the
event.  If the Event end doesn't give room for the text, the text will overlap
the line so that its bottom touches the bottom of the Event.  If there is no
room for the text at all, because of other text or the top of the Event, the
text will disappear and the line will be blue, to mark hidden text.

No Event may overlap another Event on the same Track.

-}

module Ui.Event where
import qualified Data.Generics as Generics
import Foreign
import Foreign.C

import Ui.Types
import qualified Ui.Font as Font


data Event = Event {
    event_text :: String
    , event_duration :: TrackPos
    , event_color :: Color
    , event_style :: Font.TextStyle
    , event_align_to_bottom :: Bool
    } deriving (Eq, Show, Read, Generics.Data, Generics.Typeable)


-- * storable

#include "c_interface.h"

instance Storable Event where
    sizeOf _ = #size Event
    alignment _ = undefined
    poke = poke_event

poke_event eventp (Event text dur color style align_to_bottom) = do
        -- Must be freed by the caller, EventTrackView::draw_area.
        textp <- if null text then return nullPtr else newCString text
        (#poke Event, text) eventp textp
        (#poke Event, duration) eventp dur
        (#poke Event, color) eventp color
        (#poke Event, style) eventp style
        (#poke Event, align_to_bottom) eventp align_to_bottom
