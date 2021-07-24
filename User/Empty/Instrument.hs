-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Local instrument definition with nothing in it.  If you have MIDI
-- instruments you'll want to put some basic configuration in here.
-- See User\/Elaforge\/Instrument for examples.
module User.Empty.Instrument (midi_synths, all_loads) where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified User.Empty.Instrument.GeneralMidi as GeneralMidi


-- | Synth declarations for each synth that is declared purely.
midi_synths :: [MidiInst.Synth]
midi_synths = [generic_synth, GeneralMidi.synth]

-- | Each synth that caches to disk has a function to make the cache, and one
-- to load it.
all_loads :: [(InstT.SynthName, (MidiInst.MakeDb, MidiInst.Load))]
all_loads = []

generic_synth :: MidiInst.Synth
generic_synth =
    MidiInst.synth "generic"
        "Generic MIDI instrument with no special configuration." $
        MidiInst.synth_controls controls patches
    where
    -- Special control names, e.g.: (16, "morph-x"), (17, "morph-y")
    controls :: [(Midi.Control, ScoreT.Control)]
    controls = []

    patches :: [MidiInst.Patch]
    patches = [MidiInst.default_patch pb_range []]
    -- Pitch bend range.  Most MIDI patches default to a whole-step in each
    -- direction, which is pretty limiting.  You should configure yours to
    -- some wider range.
    pb_range = (-2, 2)
