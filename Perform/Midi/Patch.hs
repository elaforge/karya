-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Patch where
import qualified Control.DeepSeq as DeepSeq

import qualified Util.Pretty as Pretty
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import Types


-- | The Patch is derived from an 'Instrument.Instrument' and contains all the
-- data necessary to render a 'Perform.Midi.Perform.Event' to a midi message.
-- Each Event has an attached Patch.
data Patch = Patch {
    -- | The name for the instrument as used in the score.  It should globally
    -- identify the instrument within this score.
    name :: !Score.Instrument
    -- | Keyswitches required by this instrument.  At higher levels, a single
    -- instrument can respond to a variety of keyswitches, but at the perform
    -- level, each instrument of each note is specialized to the particular
    -- keyswitches intended.  So this is normally empty, but filled in by
    -- convert prior to perform.
    , keyswitch :: ![Instrument.Keyswitch]
    -- | If true, the keysitch has to be held while the note is playing.
    -- Otherwise, it will just be tapped before the note starts.
    , hold_keyswitch :: !Bool

    -- | Map control names to a control number.  Some controls are shared by
    -- all midi instruments, but some instruments have special controls.
    , control_map :: !Control.ControlMap
    , pitch_bend_range :: !Control.PbRange
    -- | Time from NoteOff to inaudible, in seconds.  This can be used to
    -- figure out how long to generate control messages, or possibly determine
    -- overlap for channel allocation, though I use LRU so it shouldn't matter.
    , decay :: !(Maybe RealTime)
    } deriving (Eq, Ord, Show)

patch :: Score.Instrument -> Instrument.Patch -> Patch
patch score_inst inst = Patch
    { name = score_inst
    , keyswitch = mempty
    , hold_keyswitch = Instrument.has_flag inst Instrument.HoldKeyswitch
    , control_map = Instrument.patch_control_map inst
    , pitch_bend_range = Instrument.patch_pitch_bend_range inst
    , decay = Instrument.patch_decay inst
    }

instance DeepSeq.NFData Patch where
    -- don't bother with the rest since instruments are constructed all at once
    rnf inst = DeepSeq.rnf (keyswitch inst)

instance Pretty.Pretty Patch where
    format (Patch name keyswitch hold_keyswitch cmap pb_range decay) =
        Pretty.record "Patch"
            [ ("name", Pretty.format name)
            , ("keyswitch", Pretty.format keyswitch)
            , ("hold_keyswitch", Pretty.format hold_keyswitch)
            , ("control_map", Pretty.format cmap)
            , ("pb_range", Pretty.format pb_range)
            , ("decay", Pretty.format decay)
            ]

-- | Somewhat conservative default decay which should suit most instruments.
-- 'decay' will probably only rarely be explicitly set.
default_decay :: RealTime
default_decay = 1.0
