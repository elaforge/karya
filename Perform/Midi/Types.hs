-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Basic types for "Perform.Midi.Perform".
module Perform.Midi.Types where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.TimeVector as TimeVector
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch

import           Global
import           Types


-- | The Patch is derived from a 'Patch.Patch' and contains all the data
-- necessary to render a 'Perform.Midi.Perform.Event' to a midi message.  Each
-- Event has an attached Patch.
data Patch = Patch {
    -- | The name for the instrument as used in the score.  It should globally
    -- identify the instrument within this score.
    patch_name :: !Score.Instrument
    -- | Keyswitches required by this instrument.  At higher levels, a single
    -- instrument can respond to a variety of keyswitches, but at the perform
    -- level, each instrument of each note is specialized to the particular
    -- keyswitches intended.  So this is normally empty, but filled in by
    -- convert prior to perform.
    , patch_keyswitches :: ![Patch.Keyswitch]
    -- | If true, the keysitch has to be held while the note is playing.
    -- Otherwise, it will just be tapped before the note starts.
    , patch_hold_keyswitches :: !Bool

    -- | Map control names to a control number.  Some controls are shared by
    -- all midi instruments, but some instruments have special controls.
    , patch_control_map :: !Control.ControlMap
    , patch_pitch_bend_range :: !Control.PbRange
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , patch_decay :: !(Maybe RealTime)
    } deriving (Eq, Ord, Show)

patch :: Score.Instrument -> Patch.Config -> Patch.Patch -> Patch
patch score_inst config =
    patch_from_settings score_inst (Patch.config_settings config)

patch_from_settings :: Score.Instrument -> Patch.Settings -> Patch.Patch
    -> Patch
patch_from_settings score_inst settings patch = Patch
    { patch_name = score_inst
    , patch_keyswitches = []
    , patch_hold_keyswitches = maybe False (Set.member Patch.HoldKeyswitch)
        (Patch.config_flags settings)
    , patch_control_map = Patch.patch_control_map patch
    -- This should definitely be Just because the Patch.patch constructor
    -- requires it.  It's Maybe so Patch.config_settings can optionally replace
    -- it.
    , patch_pitch_bend_range = fromMaybe (-100, 100) $
        Patch.config_pitch_bend_range settings
    , patch_decay = Patch.config_decay settings
    }

instance DeepSeq.NFData Patch where
    -- don't bother with the rest since instruments are constructed all at once
    rnf inst = DeepSeq.rnf (patch_keyswitches inst)

instance Pretty Patch where
    format (Patch name keyswitches hold_keyswitches cmap pb_range decay) =
        Pretty.record "Patch"
            [ ("name", Pretty.format name)
            , ("keyswitches", Pretty.format keyswitches)
            , ("hold_keyswitches", Pretty.format hold_keyswitches)
            , ("control_map", Pretty.format cmap)
            , ("pb_range", Pretty.format pb_range)
            , ("decay", Pretty.format decay)
            ]

-- | Somewhat conservative default decay which should suit most instruments.
-- 'decay' will probably only rarely be explicitly set.
default_decay :: RealTime
default_decay = 1.0


-- * event

data Event = Event {
    event_start :: !RealTime
    , event_duration :: !RealTime
    , event_patch :: !Patch
    , event_controls :: !(Map Score.Control MSignal.Signal)
    , event_pitch :: !MSignal.Signal
    , event_start_velocity :: !MSignal.Y
    , event_end_velocity :: !MSignal.Y
    , event_stack :: !Stack.Stack
    } deriving (Eq, Show)

instance DeepSeq.NFData Event where
    rnf (Event start dur inst controls pitch _svel _evel stack) =
        rnf start `seq` rnf dur `seq` rnf inst `seq` rnf controls
        `seq` rnf pitch `seq` rnf stack
        where
        rnf :: DeepSeq.NFData a => a -> ()
        rnf = DeepSeq.rnf

instance Pretty Event where
    format (Event start dur patch controls pitch svel evel stack) =
        Pretty.record "Event"
            [ ("start", Pretty.format start)
            , ("duration", Pretty.format dur)
            , ("patch", Pretty.format (patch_name patch))
            , ("keyswitches", Pretty.format (patch_keyswitches patch))
            , ("controls", Pretty.format controls)
            , ("pitch", Pretty.format pitch)
            , ("velocity", Pretty.format (svel, evel))
            , ("stack", Pretty.format stack)
            ]

-- | Pretty print the event more briefly than the Pretty instance.
show_short :: Event -> Text
show_short event =
    pretty (start, event_duration event, name, pitch, event_controls event)
    where
    start = event_start event
    name = patch_name (event_patch event)
    pitch = Pitch.NoteNumber <$> TimeVector.at start (event_pitch event)

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

event_instrument :: Event -> Score.Instrument
event_instrument = patch_name . event_patch
