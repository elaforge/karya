-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
-}
module Ui.Event (
    -- * types
    Event
    , Text
    , Stack(..), IndexKey, event
    -- * access
    , start, duration, text, style, stack
    , start_, duration_, text_, style_, stack_, end_
    , end, range, overlaps, min, max
    -- ** Orientation
    , orientation, orientation_of
    , is_negative, is_positive
    -- * modify
    , set_start, set_end
    , place, round
    -- * misc
    , intern_event
    -- * style
    , EventStyle
) where
import Prelude hiding (round, min, max)
import qualified Prelude
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified Data.Text as Text
import ForeignC

import qualified Util.CUtil as CUtil
import qualified Util.Lens as Lens
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put)
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Style as Style
import qualified Ui.Types as Types

import qualified Derive.Stack as Stack
import qualified App.Config as Config
import Types
import Global hiding (Text)


-- * types

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

-- | TODO remove this, it dates from when event text was ByteString.
type Text = Text.Text

data Stack = Stack {
    -- | The stack is used so the event retains a reference to its generating
    -- event.
    stack_stack :: !Stack.Stack
    , stack_key :: !IndexKey
    } deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData Stack where
    rnf = DeepSeq.rnf . stack_stack

{- | This is the original position of the event on its integrated track.  I can
    use this to find the (hopefully) equivalent event from the next integration
    to apply it even if the event has been moved or altered.

    Keying on just the position has the effect that moving an event means to
    move any event that is generated at that position.  This might be perfectly
    reasonable or even desirable since it's easier to understand.  I'll have to
    see what kinds of edits and what kinds of reintegrations are likely in
    practice.
-}
type IndexKey = TrackTime

instance DeepSeq.NFData Event where
    rnf = DeepSeq.rnf . stack

instance Pretty Event where
    format (Event start dur bs _style stack) =
        "Event" <> Pretty.format (start, dur, bs, stack)

instance Pretty Stack where
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

-- * access

-- Don't allow direct modification.

start :: Event -> TrackTime
start = _start

duration :: Event -> TrackTime
duration = _duration

text :: Event -> Text
text = _text

style :: Event -> Style.StyleId
style = _style

stack :: Event -> Maybe Stack
stack = _stack

start_ :: Lens Event TrackTime
start_ = event_lens _start (\val event -> event { _start = val })

duration_ :: Lens Event TrackTime
duration_ = event_lens_eq eq _duration (\val event -> event { _duration = val })
    where eq a b = a == b && ScoreTime.is_negative a == ScoreTime.is_negative b

text_ :: Lens Event Text
text_ = event_lens _text (\val event -> event { _text = val })

style_ :: Lens Event Style.StyleId
style_ = event_lens _style (\val event -> event { _style = val })

stack_ :: Lens Event (Maybe Stack)
stack_ = event_lens _stack (\val event -> event { _stack = val })

-- -- | The stack is a bit different in that clearing the stack will 'modified'
-- stack_ :: Lens Event (Maybe Stack)
-- stack_ = Lens.lens field update
--     where
--     field = _stack
--     update modify event = case (field event, val) of
--         (Nothing, Nothing) -> event
--         (Nothing, Just _) -> new
--         (Just _, Nothing) -> modified new
--         (Just _, Just _) -> new
--         where
--         val = modify (field event)
--         new = event { _stack = val }

-- | Return the position at the end of the event.  If it has a negative
-- duration, this will be before the 'start'.
end :: Event -> ScoreTime
end e = start e + duration e

end_ :: Lens Event TrackTime
end_ = Lens.lens end update
    where
    update modify event = duration_ #= modify (end event) - start event $ event

range :: Event -> (ScoreTime, ScoreTime)
range e = (min e, max e)

overlaps :: ScoreTime -> Event -> Bool
overlaps p event
    | start event == p = True
    | is_positive event = start event <= p && p < end event
    | otherwise = end event < p && p <= start event

min, max :: Event -> ScoreTime
min e = Prelude.min (start e) (end e)
max e = Prelude.max (start e) (end e)

-- ** lens

event_lens :: Eq a => (Event -> a) -> (a -> Event -> Event) -> Lens.Lens Event a
event_lens = event_lens_eq (==)

event_lens_eq :: (a -> a -> Bool) -> (Event -> a) -> (a -> Event -> Event)
    -> Lens.Lens Event a
event_lens_eq eq field set = Lens.lens field update
    where
    update modify event
        | field event `eq` val = event
        | otherwise = modified (set val event)
        where val = modify (field event)

-- | If this was an integrated event, it might have the unmodified style.
-- Set it to modified now so I don't have to wait for the next integration.
modified :: Event -> Event
modified event = event { _style = Config.modified_style (style event) }

-- ** Orientation

orientation :: Event -> Types.Orientation
orientation = orientation_of . duration

orientation_of :: TrackTime -> Types.Orientation
orientation_of t
    | ScoreTime.is_negative t = Types.Negative
    | otherwise = Types.Positive

is_negative :: Event -> Bool
is_negative = ScoreTime.is_negative . duration

is_positive :: Event -> Bool
is_positive = not . is_negative

-- * modify

-- | Move the start only.  This could invert the duration if the start moves
-- past the end.
set_start :: ScoreTime -> Event -> Event
set_start p event = place p (end event - p) event

-- | Redundant with the 'end_' lens, but here for symmetry with 'set_start'.
set_end :: ScoreTime -> Event -> Event
set_end = (end_ #=)

place :: ScoreTime -> ScoreTime -> Event -> Event
place start dur event = modified $ event { _start = start, _duration = dur }

-- | Round event times as described in 'ScoreTime.round'.
-- TODO used by Events, do I really need this?
round :: Event -> Event
round event = event
    { _start = ScoreTime.round (start event)
    , _duration = ScoreTime.round (duration event)
    }

-- * misc

intern_event :: Map.Map Text (Text, Int) -> Event
    -> (Map.Map Text (Text, Int), Event)
intern_event table event = case Map.lookup (text event) table of
    Nothing -> (Map.insert (text event) (text event, 1) table, event)
    Just (interned, count) ->
        (Map.insert interned (interned, count+1) table,
            event { _text = interned })

-- * style

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
    textp <- if Text.null text
        then return nullPtr else CUtil.textToCString0 text
    (#poke Event, start) eventp start
    (#poke Event, duration) eventp dur
    (#poke Event, text) eventp textp
    (#poke Event, style_id) eventp style_id
