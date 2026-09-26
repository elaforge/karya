-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE StrictData #-}
-- | Basic types for "Perform.Midi.Perform".
module Perform.Midi.Types where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.TimeVector as TimeVector
import qualified Derive.ScoreT as ScoreT
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
data Patch = Patch
    -- | The name for the instrument as used in the score.  It should globally
    -- identify the instrument within this score.
    { patchName :: ScoreT.Instrument
    -- | Keyswitches required by this instrument.  At higher levels, a single
    -- instrument can respond to a variety of keyswitches, but at the perform
    -- level, each instrument of each note is specialized to the particular
    -- keyswitches intended.  So this is normally empty, but filled in by
    -- convert prior to perform.
    , patchKeyswitches :: [Patch.Keyswitch]
    -- | If true, the keysitch has to be held while the note is playing.
    -- Otherwise, it will just be tapped before the note starts.
    , patchHoldKeyswitches :: Bool

    -- | Map control names to a control number.  Some controls are shared by
    -- all midi instruments, but some instruments have special controls.
    , patchControlMap :: Control.ControlMap
    , patchPitchBendRange :: Control.PbRange
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , patchDecay :: Maybe RealTime
    } deriving (Eq, Ord, Show)

patch :: ScoreT.Instrument -> Patch.Config -> Patch.Patch -> Patch
patch scoreInst config =
    patchFromSettings scoreInst (Patch.configSettings config)

patchFromSettings :: ScoreT.Instrument -> Patch.Settings -> Patch.Patch
    -> Patch
patchFromSettings scoreInst settings patch = Patch
    { patchName = scoreInst
    , patchKeyswitches = []
    , patchHoldKeyswitches = maybe False (Set.member Patch.HoldKeyswitch)
        (Patch.configFlags settings)
    , patchControlMap = Patch.patchControlMap patch
    -- This should definitely be Just because the Patch.patch constructor
    -- requires it.  It's Maybe so Patch.config_settings can optionally replace
    -- it.
    , patchPitchBendRange = fromMaybe (-100, 100) $
        Patch.configPitchBendRange settings
    , patchDecay = Patch.configDecay settings
    }

instance DeepSeq.NFData Patch where
    -- don't bother with the rest since instruments are constructed all at once
    rnf inst = DeepSeq.rnf (patchKeyswitches inst)

instance Pretty Patch where
    format (Patch name keyswitches holdKeyswitches cmap pbRange decay) =
        Pretty.record "Patch"
            [ ("name", Pretty.format name)
            , ("keyswitches", Pretty.format keyswitches)
            , ("hold_keyswitches", Pretty.format holdKeyswitches)
            , ("control_map", Pretty.format cmap)
            , ("pb_range", Pretty.format pbRange)
            , ("decay", Pretty.format decay)
            ]

-- | Somewhat conservative default decay which should suit most instruments.
-- 'decay' will probably only rarely be explicitly set.
defaultDecay :: RealTime
defaultDecay = 1.0


-- * event

data Event = Event
    { eventStart :: RealTime
    , eventDuration :: RealTime
    , eventPatch :: Patch
    , eventControls :: Map ScoreT.Control MSignal.Signal
    , eventPitch :: MSignal.Signal
    , eventStartVelocity :: MSignal.Y
    , eventEndVelocity :: MSignal.Y
    , eventStack :: Stack.Stack
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
            , ("patch", Pretty.format (patchName patch))
            , ("keyswitches", Pretty.format (patchKeyswitches patch))
            , ("controls", Pretty.format controls)
            , ("pitch", Pretty.format pitch)
            , ("velocity", Pretty.format (svel, evel))
            , ("stack", Pretty.format stack)
            ]

-- | Pretty print the event more briefly than the Pretty instance.
showShort :: Event -> Text
showShort event =
    pretty (start, eventDuration event, name, pitch, eventControls event)
    where
    start = eventStart event
    name = patchName (eventPatch event)
    pitch = Pitch.NoteNumber <$> TimeVector.at start (eventPitch event)

eventEnd :: Event -> RealTime
eventEnd event = eventStart event + eventDuration event

eventInstrument :: Event -> ScoreT.Instrument
eventInstrument = patchName . eventPatch
