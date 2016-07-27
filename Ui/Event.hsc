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
    Event, start, duration, orientation, style, stack
    , Orientation(..), invert
    , Text
    , Stack(..), IndexKey, event
    -- * text
    , text, set_text
    , modify_text
    , intern_event
    -- * start, duration
    , end, trigger, range, overlaps
    , min, max
    , move, set_start, set_end
    , place, round, set_duration, modify_duration, modify_end
    , set_orientation, is_negative, is_positive
    , orientation_as_duration
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
    , _orientation :: !Orientation
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
orientation = _orientation

text :: Event -> Text
text = _text

style :: Event -> Style.StyleId
style = _style

stack :: Event -> Maybe Stack
stack = _stack

{- | Whether the event is front-weighted or back-weighted.

    Originally this was represented with positive and negative duration, but it
    turns out I frequently want a front-weighted note which starts where
    a back-weighted one ends, which doesn't work if both of them are indexed in
    the EventMap by the same ScoreTime.  I could continue to encode the
    orientation in the duration, but that would break the rule that you
    can look an Event up by it's 'start' and seems confusing.

    I tried a number of pairs: Start|End, Front|Back, but all of them seemed
    awkward, so I'm back to Positive|Negative even if it's no longer encoded
    that way.
-}
data Orientation = Positive | Negative deriving (Eq, Read, Show)

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
    format (Event start dur orient bs _style stack) =
        -- Event(0t, 1t, "text", Nothing)
        -- Event-(0t, 1t, "text", Nothing)
        "Event" <> o <> Pretty.format (start, dur, bs, stack)
        where o = if orient == Positive then "" else "-"

instance Pretty.Pretty Stack where
    format (Stack stack key) =
        Pretty.format (Pretty.format stack, Pretty.format key)

-- | Event constructor.
event :: ScoreTime -> ScoreTime -> Text -> Event
event start dur text = Event
    { _start = s
    , _duration = d
    , _orientation = orient
    , _text = text
    , _style = Config.default_style
    , _stack = Nothing
    }
    where
    (s, d, orient) = if dur < 0 || isNegativeZero (ScoreTime.to_double dur)
        then (start + dur, -dur, Negative)
        else (start, dur, Positive)

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

-- | Return the position at the end of the event.
end :: Event -> ScoreTime
end e = start e + duration e

min, max :: Event -> ScoreTime
min = start
max = end

trigger :: Event -> ScoreTime
trigger e = case orientation e of
    Positive -> start e
    Negative -> end e

range :: Event -> (ScoreTime, ScoreTime)
range e = (start e, end e)

overlaps :: ScoreTime -> Event -> Bool
overlaps p event
    | is_positive event = start event <= p && p < end event
    | otherwise = start event < p && p <= end event

-- | Move both start and end, so the duration remains the same.
move :: (ScoreTime -> ScoreTime) -> Event -> Event
move f event = modified $ event { _start = f (start event) }

-- | Move the start only.  The duration can't go past 0, so the start can't
-- move past the end.
set_start :: ScoreTime -> Event -> Event
set_start p event = place s (end event - s) event
    where s = Prelude.min (end event) p

set_end :: ScoreTime -> Event -> Event
set_end p event = set_duration (Prelude.max 0 (p - start event)) event

place :: ScoreTime -> ScoreTime -> Event -> Event
place start dur event = modified $ event { _start = start, _duration = dur }

-- | Round event times as described in 'ScoreTime.round'.
-- TODO used by Events, do I really need this?
round :: Event -> Event
round event = event
    { _start = ScoreTime.round (start event)
    , _duration = ScoreTime.round (duration event)
    }

-- TODO if it's negative, take abs and set orientation=Negative?
-- Then I would have to move the start, which this function shouldn't do,
-- because callers rely on it not being able to move the event.
set_duration :: ScoreTime -> Event -> Event
set_duration dur_ event
    | dur /= duration event = modified $ event { _duration = dur }
    | otherwise = event
    where dur = Prelude.max 0 dur_

modify_duration :: (ScoreTime -> ScoreTime) -> Event -> Event
modify_duration f evt = set_duration (f (duration evt)) evt

modify_end :: (ScoreTime -> ScoreTime) -> Event -> Event
modify_end f evt =
    modify_duration (\dur -> f (start evt + dur) - start evt) evt

set_orientation :: Orientation -> Event -> Event
set_orientation orient event = event { _orientation = orient }

is_negative :: Event -> Bool
is_negative = (==Negative) . orientation

is_positive :: Event -> Bool
is_positive = (==Positive) . orientation

orientation_as_duration :: Event -> (TrackTime, TrackTime)
orientation_as_duration e = decode_orient (orientation e) (start e) (duration e)

-- * stack

set_stack :: Stack -> Event -> Event
set_stack stack event = event { _stack = Just stack }

strip_stack :: Event -> Event
strip_stack event = modified $ event { _stack = Nothing }

-- * style

modify_style :: (Style.StyleId -> Style.StyleId) -> Event -> Event
modify_style f event = event { _style = f (style event) }

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
    put (Event start dur orient text style stack) =
        put s >> put d >> put text >> put style >> put stack
        where (s, d) = decode_orient orient start dur
    get = do
        start :: ScoreTime <- get
        dur :: ScoreTime <- get
        text :: Text <- get
        style :: Style.StyleId <- get
        stack :: Maybe Stack <- get
        let (s, d, orient) = encode_orient start dur
        return $ Event s d orient text style stack

decode_orient :: Orientation -> TrackTime -> TrackTime -> (TrackTime, TrackTime)
decode_orient orient start dur = case orient of
    Positive -> (start, dur)
    Negative -> (start + dur, -dur)

encode_orient :: TrackTime -> TrackTime -> (TrackTime, TrackTime, Orientation)
encode_orient start dur
    | dur < 0 || isNegativeZero (ScoreTime.to_double dur) =
        (start + dur, -dur, Negative)
    | otherwise = (start, dur, Positive)

instance Serialize.Serialize Stack where
    put (Stack a b) = put a >> put b
    get = do
        stack :: Stack.Stack <- get
        key :: IndexKey <- get
        return $ Stack stack key

-- * storable

#include "Ui/c_interface.h"

instance CStorable Event where
    sizeOf _ = #size Event
    alignment _ = alignment (0 :: CDouble)
    poke = poke_event
    peek = error "Event peek unimplemented"

poke_event :: Ptr Event -> Event -> IO ()
poke_event eventp (Event start dur orient text (Style.StyleId style_id) _) = do
    -- Must be freed by the caller, EventTrackView::draw_area.
    textp <- if Text.null text then return nullPtr else Util.textToCString0 text
    let (s, d) = decode_orient orient start dur
    (#poke Event, start) eventp s
    (#poke Event, duration) eventp d
    (#poke Event, text) eventp textp
    (#poke Event, style_id) eventp style_id
