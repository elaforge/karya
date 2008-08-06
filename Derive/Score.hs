{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{- | A Score is the unit at which derivers operate, and is just a list of
Events.  The events here are the derivation-level view of an Event.  They are
generated from UI Events, and will eventually be transformed into Perform
Events, which are specific to the performance backend.
-}
module Derive.Score where
import qualified Data.Generics as Generics
import qualified Data.Map as Map

import Ui.Types

import qualified Perform.Signal as Signal
import qualified Perform.Warning as Warning


data Event = Event {
    -- | These are the core attributes that define an event.  For the moment,
    -- UI display attributes like font style are not preserved here.
    event_start :: TrackPos
    , event_duration :: TrackPos
    , event_text :: String
    , event_controllers :: ControllerMap

    -- | Keep track of this event's display in various tracks (it may appear
    -- in more than one if it appears in a merged track).  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: [Warning.StackPos]

    -- | These are optional parameters that may or may not be required by the
    -- performer.
    , event_instrument :: Maybe Instrument
    } deriving (Show)

event_end event = event_start event + event_duration event

-- | Mostly for testing, since real events will come from 'from_track_event'.
event :: TrackPos -> TrackPos -> String -> Event
event start dur text = Event start dur text Map.empty [] Nothing

type ControllerMap = Map.Map Controller Signal.Signal

-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as the
-- backend itself, but things at the Derive layer and above don't care about
-- all that.
data Instrument = Instrument String
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)
inst_name (Instrument s) = s

newtype Controller = Controller String deriving (Eq, Ord, Show)

-- * constants

pitch :: Controller
pitch = Controller "pitch"
