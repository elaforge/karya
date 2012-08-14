{-# LANGUAGE ScopedTypeVariables #-}
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
module Ui.Event (
    Event, start, duration, style, stack, event_bytestring
    , Text, Stack(..), IndexKey, event
    -- * text
    , event_string, set_string, modify_string
    , modify_bytestring
    , intern_event
    -- * start, duration
    , end, min, max, range, overlaps
    , move, place, set_duration, modify_duration
    , positive, negative
    -- * stack
    , set_stack, strip_stack
    -- * style
    , modify_style, modified, SetStyle
    -- * serialize
    , Event0, Event1, Event2, convert0, convert1, convert2
) where
import Prelude hiding (min, max)
import qualified Prelude
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import Foreign

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put)
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Style as Style
import qualified Ui.Util as Util

import qualified Derive.Stack as Stack
import qualified App.Config as Config
import Types


data Event = Event {
    start :: !ScoreTime
    , duration :: !ScoreTime
    -- | UTF8 encoded.
    , event_bytestring :: !Text
    , style :: !Style.StyleId
    -- | If this event was integrated from another event as by
    -- "Derive.Call.Integrate", this will have the stack of the source event.
    , stack :: !(Maybe Stack)
    } deriving (Eq, Read, Show)

type Text = B.ByteString

to_text :: String -> Text
to_text = UTF8.fromString

data Stack = Stack {
    -- | The stack is used so the event retains a reference to its generating
    -- event.
    stack_stack :: !Stack.Stack
    , stack_key :: !IndexKey
    } deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData Stack where
    rnf = DeepSeq.rnf . stack_stack

-- | This is the original position of the event on its integrated track.  I can
-- use this to find the (hopefully) equivalent event from the next integration
-- to apply it even if the event has been moved or altered.
--
-- Keying on just the position has the effect that moving an event means to
-- move any event that is generated at that position.  This might be perfectly
-- reasonable or even desirable since it's easier to understand.  I'll have to
-- see what kinds of edits and what kinds of reintegrations are likely in
-- practice.
type IndexKey = ScoreTime

instance DeepSeq.NFData Event where
    rnf = DeepSeq.rnf . stack

instance Pretty.Pretty Event where
    format (Event start dur bs _style stack) =
        Pretty.format (Pretty.format start, Pretty.format dur,
            Pretty.format bs, dur, Pretty.format stack)

instance Pretty.Pretty Stack where
    format (Stack stack key) =
        Pretty.format (Pretty.format stack, Pretty.format key)

-- | Manual event constructor.
event :: ScoreTime -> ScoreTime -> String -> Event
event start dur text = Event
    { start = start
    , duration = dur
    , event_bytestring = to_text text
    , style = Config.default_style
    , stack = Nothing
    }

-- * text

event_string :: Event -> String
event_string = UTF8.toString . event_bytestring

set_string :: String -> Event -> Event
set_string s event = modified $ event { event_bytestring = to_text s }

modify_string :: (String -> String) -> Event -> Event
modify_string f event =
    modified $ event { event_bytestring = modify (event_bytestring event) }
    where modify = UTF8.fromString . f . UTF8.toString

modify_bytestring :: (Text -> Text) -> Event -> Event
modify_bytestring f event =
    event { event_bytestring = f (event_bytestring event) }

intern_event :: Map.Map Text (Text, Int) -> Event
    -> (Map.Map Text (Text, Int), Event)
intern_event table event = case Map.lookup text table of
    Nothing -> (Map.insert text (text, 1) table, event)
    Just (interned, count) ->
        (Map.insert interned (interned, count+1) table,
            event { event_bytestring = interned })
    where text = event_bytestring event

-- * start, duration

-- | Return the position at the end of the event.  Could be before @pos@ if
-- the event has a negative duration.
end :: Event -> ScoreTime
end e = start e + duration e

min, max :: Event -> ScoreTime
min e = Prelude.min (start e) (end e)
max e = Prelude.max (start e) (end e)

range :: Event -> (ScoreTime, ScoreTime)
range e = (min e, max e)

overlaps :: ScoreTime -> Event -> Bool
overlaps p event
    | positive event = p == start event || p >= start event && p < end event
    | otherwise = p == start event || p <= start event && p > end event

move :: (ScoreTime -> ScoreTime) -> Event -> Event
move f event = modified $ event { start = f (start event) }

place :: ScoreTime -> ScoreTime -> Event -> Event
place pos dur event = modified $ event { start = pos, duration = dur }

set_duration :: ScoreTime -> Event -> Event
set_duration dur event = modified $ event { duration = dur }

modify_duration :: (ScoreTime -> ScoreTime) -> Event -> Event
modify_duration f evt = set_duration (f (duration evt)) evt

-- | 0 is considered both positive and negative because they're ambiguous.
-- For example, Track._split_range which includes them in both ends.
positive, negative :: Event -> Bool
positive = not . negative
negative = negative_duration . duration

negative_duration :: ScoreTime -> Bool
negative_duration d = d < 0 || isNegativeZero (ScoreTime.to_double d)

-- * stack

set_stack :: Stack -> Event -> Event
set_stack stack event = event { stack = Just stack }

strip_stack :: Event -> Event
strip_stack event = modified $ event { stack = Nothing }

-- * style

modify_style :: (Style.StyleId -> Style.StyleId) -> Event -> Event
modify_style f event = event { style = f (style event) }

-- | If this was an integrated event, it might have the unmodified style.
-- Set it to modified now so I don't have to wait for the next integration.
modified :: Event -> Event
modified event = event { style = Config.modified_style (style event) }

-- | This is called on events before they go to the UI, to be used for "syntax
-- highlighting", i.e. it can set the style depending on the event, but the
-- change in style won't be saved in the event itself.
type SetStyle = String -> ScoreTime -> Event -> Style.StyleId

-- * serialize

instance Serialize.Serialize Event where
    put (Event a b c d e) = put a >> put b >> put c >> put d >> put e
    get = do
        start :: ScoreTime <- get
        dur :: ScoreTime <- get
        text :: B.ByteString <- get
        style :: Style.StyleId <- get
        stack :: Maybe Stack <- get
        return $ Event start dur text style stack

data Event0 = Event0 !Text !ScoreTime !Style.StyleId
instance Serialize.Serialize Event0 where
    put (Event0 a b c) = put a >> put b >> put c
    get = do
        text :: B.ByteString <- get
        dur :: ScoreTime <- get
        style :: Style.StyleId <- get
        return $ Event0 text dur style

data Event1 = Event1 !Text !ScoreTime !Style.StyleId !(Maybe Stack.Stack)
instance Serialize.Serialize Event1 where
    put (Event1 a b c d) = put a >> put b >> put c >> put d
    get = do
        text :: B.ByteString <- get
        dur :: ScoreTime <- get
        style :: Style.StyleId <- get
        stack :: Maybe Stack.Stack <- get
        return $ Event1 text dur style stack

data Event2 = Event2 !Text !ScoreTime !Style.StyleId !(Maybe Stack)
instance Serialize.Serialize Event2 where
    put (Event2 a b c d) = put a >> put b >> put c >> put d
    get = do
        text :: B.ByteString <- get
        dur :: ScoreTime <- get
        style :: Style.StyleId <- get
        stack :: Maybe Stack <- get
        return $ Event2 text dur style stack

instance Serialize.Serialize Stack where
    put (Stack a b) = put a >> put b
    get = do
        stack :: Stack.Stack <- get
        key :: IndexKey <- get
        return $ Stack stack key

convert0 :: ScoreTime -> Event0 -> Event
convert0 start (Event0 bs dur style) = Event start dur bs style Nothing

convert1 :: ScoreTime -> Event1 -> Event
convert1 start (Event1 bs dur style _stack) = Event start dur bs style Nothing

convert2 :: ScoreTime -> Event2 -> Event
convert2 start (Event2 bs dur style stack) = Event start dur bs style stack

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
poke_event eventp (Event start dur text (Style.StyleId style_id) _) = do
    -- Must be freed by the caller, EventTrackView::draw_area.
    textp <- if B.null text then return nullPtr
        else Util.unpackCString0 text
    (#poke Event, start) eventp start
    (#poke Event, duration) eventp dur
    (#poke Event, text) eventp textp
    (#poke Event, style_id) eventp style_id
