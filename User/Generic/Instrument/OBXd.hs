-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Generic.Instrument.OBXd where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.ScoreT as ScoreT
import qualified Midi.Midi as Midi


synth :: MidiInst.Synth
synth =
    MidiInst.synth "obxd" "https://www.discodsp.com/obxd/" $
        MidiInst.synth_controls controls
        [MidiInst.default_patch pb_range []]

-- According to midiMap.h, it actually has bendrange at 118, but it's
-- overwritten by level-dif.  It toggles between 2 and 12.
--
-- Since it's open source if I wanted to I could probably patch it to support
-- Midi.pitch_bend_range.
pb_range :: (Int, Int)
pb_range = (-2, 2)

controls :: [(Midi.Control, ScoreT.Control)]
controls =
    [ (15, "voice-count")
    , (16, "unison")
    , (17, "octave")
    , (18, "filter-warm")
    , (19, "lfofreq")
    , (20, "vampenv")
    , (21, "asplayedallocation")
    , (22, "lfo1amt")
    , (23, "portamento")
    , (24, "udet")
    , (25, "lfo2amt")
    , (33, "tune")
    , (34, "bendosc2")
    , (35, "legatomode")
    , (36, "ldec")
    , (37, "lsus")
    , (38, "fatk")
    , (39, "fdec")
    , (40, "fsus")
    , (41, "frel")
    , (42, "resonance")
    , (43, "osc2-det")
    , (44, "lfosinwave")
    , (45, "lfosquarewave")
    , (46, "lfoshwave")
    , (47, "lfoosc1")
    , (48, "lfoosc2")
    , (49, "lfofilter")
    , (50, "lfopw1")
    , (51, "lfopw2")
    , (52, "osc2hs")
    , (53, "xmod")
    , (54, "osc1p")
    , (55, "osc2p")
    , (56, "oscquantize")
    , (57, "osc1saw")
    , (58, "osc1pul")
    , (59, "osc2saw")
    , (60, "osc2pul")
    , (61, "pw")
    , (62, "brightness")
    , (63, "envpitch")
    , (71, "volume")
    , (72, "lrel")
    , (73, "latk")
    , (74, "cutoff")
    , (75, "bendlforate")
    , (76, "vfltenv")
    , (77, "osc1mix")
    , (78, "osc2mix")
    , (81, "pan1")
    , (82, "pan2")
    , (83, "pan3")
    , (84, "pan4")
    , (85, "pan5")
    , (86, "pan6")
    , (87, "pan7")
    , (88, "pan8")
    , (102, "noisemix")
    , (103, "flt-kf")
    , (104, "multimode")
    , (105, "bandpass")
    , (106, "fourpole")
    , (107, "envelope-amt")
    , (108, "envder")
    , (109, "filterder")
    , (110, "portader")
    , (111, "economy-mode")
    , (113, "pw-env")
    , (114, "pw-env-both")
    , (115, "env-pitch-both")
    , (116, "fenv-invert")
    , (117, "pw-osc2-ofs")
    , (118, "level-dif") -- bendrange?
    , (119, "self-osc_push")
    ]
