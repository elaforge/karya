-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    Event, start, duration, orientation, orientation_of, style, stack
    , Orientation(..), invert
    , Text
    , Stack(..), IndexKey, event
    -- * text
    , text, set_text
    , modify_text
    , intern_event
    -- * start, duration
    , end, range, overlaps
    , min, max
    , move, move_to, set_start, set_end
    , place, round, set_duration, modify_duration, modify_end
    , is_negative, is_positive
    -- * stack
    , set_stack, strip_stack
    -- * style
    , modify_style, modified, EventStyle
) where
import Prelude hiding (round, min, max)
import qualified Prelude
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Text as Text
import Util.ForeignC

import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put)
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Style as Style
import qualified Ui.Util as Util

import qualified Derive.Stack as Stack
import qualified App.Config as Config
import Types
import Global hiding (Text)


data Event = Event {
    _start :: !TrackTime
    , _duration :: !TrackTime
    , _text :: !Text
    -- | Each event can have its own style.  However, in practice, because I
    -- want events to use styles consistently I set them automatically via
    -- 'EventStyle'.  This way is less flexible, but it's one less bit of state
    -- to get out of sync.
    , _style :: !Style.StyleId
    -- | If this event was integrated from another event as by
    -- "Derive.Call.Integrate", this will have the stack of the source event.
    , _stack :: !(Maybe Stack)
    } deriving (Eq, Read, Show)

-- Don't allow direct modification.

start :: Event -> TrackTime
start = _start

duration :: Event -> TrackTime
duration = _duration

orientation :: Event -> Orientation
orientation = orientation_of . duration

orientation_of :: TrackTime -> Orientation
orientation_of t
    | ScoreTime.is_negative t = Negative
    | otherwise = Positive

text :: Event -> Text
text = _text

style :: Event -> Style.StyleId
style = _style

stack :: Event -> Maybe Stack
stack = _stack

-- | Whether the event is front-weighted or back-weighted.  In the event this
-- is represented with positive or negative duration.
data Orientation = Negative | Positive
    deriving (Eq, Ord, Read, Show, Enum, Bounded)
    -- The Negative to Positive order is important, because that affects the
    -- EventMap's sort order, which functions in "Ui.Events" rely on.
instance Pretty.Pretty Orientation where pretty = showt

invert :: Orientation -> Orientation
invert Positive = Negative
invert Negative = Positive

type Text = Text.Text

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
type IndexKey = TrackTime

instance DeepSeq.NFData Event where
    rnf = DeepSeq.rnf . stack

instance Pretty.Pretty Event where
    format (Event start dur bs _style stack) =
        "Event" <> Pretty.format (start, dur, bs, stack)

instance Pretty.Pretty Stack where
    format (Stack stack key) =
        Pretty.format (Pretty.format stack, Pretty.format key)

-- | Event constructor.
event :: ScoreTime -> ScoreTime -> Text -> Event
event start dur text = Event
    { _start = start
    , _duration = dur
    , _text = text
    , _style = Config.default_style
    , _stack = Nothing
    }

-- * text

set_text :: Text.Text -> Event -> Event
set_text s event = modified $ event { _text = s }

modify_text :: (Text -> Text) -> Event -> Event
modify_text f event = event { _text = f (text event) }

intern_event :: Map.Map Text (Text, Int) -> Event
    -> (Map.Map Text (Text, Int), Event)
intern_event table event = case Map.lookup (text event) table of
    Nothing -> (Map.insert (text event) (text event, 1) table, event)
    Just (interned, count) ->
        (Map.insert interned (interned, count+1) table,
            event { _text = interned })

-- * start, duration

-- | Return the position at the end of the event.  If it has a negative
-- duration, this will be before the 'start'.
end :: Event -> ScoreTime
end e = start e + duration e

min, max :: Event -> ScoreTime
min e = Prelude.min (start e) (end e)
max e = Prelude.max (start e) (end e)

range :: Event -> (ScoreTime, ScoreTime)
range e = (min e, max e)

overlaps :: ScoreTime -> Event -> Bool
overlaps p event
    | is_positive event = start event <= p && p < end event
    | otherwise = end event < p && p <= start event

-- | Move both start and end, so the duration remains the same.
move :: (ScoreTime -> ScoreTime) -> Event -> Event
move f event = modified $ event { _start = f (start event) }

move_to :: ScoreTime -> Event -> Event
move_to p = move (const p)

-- | Move the start only.  This could invert the duration if the start moves
-- past the end.
set_start :: ScoreTime -> Event -> Event
set_start p event = place p (end event - p) event

set_end :: ScoreTime -> Event -> Event
set_end p event = set_duration (p - start event) event

place :: ScoreTime -> ScoreTime -> Event -> Event
place start dur event = modified $ event { _start = start, _duration = dur }

-- | Round event times as described in 'ScoreTime.round'.
-- TODO used by Events, do I really need this?
round :: Event -> Event
round event = event
    { _start = ScoreTime.round (start event)
    , _duration = ScoreTime.round (duration event)
    }

set_duration :: ScoreTime -> Event -> Event
set_duration dur event
    | dur /= duration event
        || ScoreTime.is_negative dur /= ScoreTime.is_negative (duration event) =
            modified $ event { _duration = dur }
    | otherwise = event

modify_duration :: (ScoreTime -> ScoreTime) -> Event -> Event
modify_duration f evt = set_duration (f (duration evt)) evt

modify_end :: (ScoreTime -> ScoreTime) -> Event -> Event
modify_end f evt = modify_duration (\dur -> f (start evt + dur) - start evt) evt

is_negative :: Event -> Bool
is_negative = ScoreTime.is_negative . duration

is_positive :: Event -> Bool
is_positive = not . is_negative

-- * stack

set_stack :: Stack -> Event -> Event
set_stack stack event = event { _stack = Just stack }

strip_stack :: Event -> Event
strip_stack event = modified $ event { _stack = Nothing }

-- * style

modify_style :: (Style.StyleId -> Style.StyleId) -> Event -> Event
modify_style modify event
    | new_style == style event = event
    | otherwise = event { _style = new_style }
    where new_style = modify (style event)

-- | If this was an integrated event, it might have the unmodified style.
-- Set it to modified now so I don't have to wait for the next integration.
modified :: Event -> Event
modified event = event { _style = Config.modified_style (style event) }

-- | This is called on events before they go to the UI, to be used for "syntax
-- highlighting", i.e. it can set the style depending on the event, but the
-- change in style won't be saved in the event itself.
type EventStyle = Text.Text -- ^ track title
    -> Event -> Style.StyleId

-- * serialize

instance Serialize.Serialize Event where
    put (Event start dur text style stack) =
        put start >> put dur >> put text >> put style >> put stack
    get = do
        start :: ScoreTime <- get
        dur :: ScoreTime <- get
        text :: Text <- get
        style :: Style.StyleId <- get
        stack :: Maybe Stack <- get
        return $ Event start dur text style stack

instance Serialize.Serialize Stack where
    put (Stack a b) = put a >> put b
    get = do
        stack :: Stack.Stack <- get
        key :: IndexKey <- get
        return $ Stack stack key

instance Serialize.Serialize Orientation where
    put = Serialize.put_enum
    get = Serialize.get_enum

-- * storable

#include "Ui/c_interface.h"

instance CStorable Event where
    sizeOf _ = #size Event
    alignment _ = alignment (0 :: CDouble)
    poke = poke_event
    peek = error "Event peek unimplemented"

poke_event :: Ptr Event -> Event -> IO ()
poke_event eventp (Event start dur text (Style.StyleId style_id) _) = do
    -- Must be freed by the caller, EventTrack::draw_area.
    textp <- if Text.null text then return nullPtr else Util.textToCString0 text
    (#poke Event, start) eventp start
    (#poke Event, duration) eventp dur
    (#poke Event, text) eventp textp
    (#poke Event, style_id) eventp style_id
