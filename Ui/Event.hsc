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

import qualified Util.Pretty as Pretty
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Style as Style

import qualified Derive.Stack as Stack
import qualified App.Config as Config
import Types


data Event = Event {
    -- | UTF8 encoded.
    event_bs :: !Text
    , event_duration :: !ScoreTime
    , event_style :: !Style.StyleId
    -- | If this event was integrated from another event as by
    -- "Derive.Call.Integrate", this will have the stack of the source event.
    , event_stack :: !(Maybe Stack)
    } deriving (Eq, Read, Show)

type Text = B.ByteString

to_text :: String -> Text
to_text = UTF8.fromString

-- | A Stack annotated with a tag and a serial number.  Events that come from
-- the same score event will all have the same stack, so some more information
-- is needed to differentiate them.
data Stack = Stack {
    stack_stack :: !Stack.Stack
    -- | One score event will emit separate ui events for note, pitch, and
    -- controls.  A string tag is used to differentiate them.
    , stack_tag :: !String
    -- | One score call can emit multiple score events, so each one is
    -- numbered.
    , stack_serial :: !Int
    } deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData Event where
    rnf (Event bs dur style stack) =
        bs `seq` dur `seq` style `seq` stack `seq` ()

instance Pretty.Pretty Event where
    format (Event bs dur _style stack) = Pretty.format
        (Pretty.format bs, Pretty.format dur, Pretty.format stack)

instance Pretty.Pretty Stack where
    format (Stack stack tag serial) = Pretty.format
        (Pretty.format stack, Pretty.text tag, Pretty.format serial)

-- | Manual event constructor.
event :: String -> ScoreTime -> Event
event text dur = Event (to_text text) dur Config.default_style Nothing

event_string :: Event -> String
event_string = UTF8.toString . event_bs

set_string :: String -> Event -> Event
set_string s evt = evt { event_bs = to_text s }

modify_string :: (String -> String) -> Event -> Event
modify_string f evt = evt { event_bs = modify (event_bs evt) }
    where modify = UTF8.fromString . f . UTF8.toString

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

-- | This is called on events before they go to the UI, to be used for "syntax
-- highlighting", i.e. it can set the style depending on the event, but the
-- change in style won't be saved in the event itself.
type SetStyle = String -> ScoreTime -> Event -> Style.StyleId

-- * storable

#include "Ui/c_interface.h"
-- See comment in BlockC.hsc.
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Event where
    sizeOf _ = #size Event
    alignment _ = #{alignment Event}
    poke = poke_event
    peek = error "Event peek unimplemented"

poke_event :: Ptr Event -> Event -> IO ()
poke_event eventp (Event text dur (Style.StyleId style_id) _) = do
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
