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
Event gets too small for its text, 

The beginning of the Event and each Sub's position is marked with a red line.
The text will begin slightly below the line, but still try to fit within the
event.  If the Event end doesn't give room for the text, the text will overlap
the line so that its bottom touches the bottom of the Event.  If there is no
room for the text at all, because of other text or the top of the Event, the
text will disappear and the line will be blue, to mark hidden text.

No Event may overlap another Event on the same Track.  The only reason for that
is to simplify Event layout.  If I ever want overlapping Events it shouldn't be
hard to add.


-}

module Interface.Event where
import qualified Interface.Color as Color
import Interface.Types

data Event = Event
    { event_title :: String
    , event_style :: TextStyle
    , event_duration :: TrackPos
    , event_subs :: [Sub]
    , event_color :: Color.Color
    , event_signal :: Signal
    , event_render_style :: RenderStyle
    , event_attrs :: Attrs
    }

-- | A sub event.
data Sub = Sub TrackPos String

-- | How to display the signal under an Event.
data RenderStyle = ColorMap [(Double, Color.Color)] | Graph deriving (Show)
data Signal = Signal [Double] -- probably an array actually

type TextStyle = (Font, FontStyle, Color.Color)
type Font = String
type FontStyle = String
