-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Audio Modeling's SWAM.
module User.Elaforge.Instrument.Swam where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Articulation as Articulation
import qualified Derive.Call as Call
import qualified Derive.Call.Ly as Ly
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Tags as Tags
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Library as Library
import qualified Derive.Sig as Sig

import qualified Instrument.InstTypes as InstTypes
import qualified Midi.CC as CC
import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch

import           Global


synth :: MidiInst.Synth
synth =
    MidiInst.synth "swam" "Audio Modeling SWAM" $
        MidiInst.synth_controls [] patches
    where
    patches = map (uncurry string)
        [ ("violin", [NN.g3, NN.d4, NN.a4, NN.e4])
        , ("viola", [NN.c3, NN.g3, NN.d4, NN.a4])
        , ("cello", [NN.c2, NN.g2, NN.d3, NN.a3])
        , ("bass", [NN.e1, NN.a1, NN.d2, NN.g2])
        ]

string :: InstTypes.Name -> [Pitch.NoteNumber] -> MidiInst.Patch
string name open_strings = MidiInst.pressure $
    MidiInst.code #= code $
    MidiInst.environ EnvKey.open_strings open_strings $
    MidiInst.patch#Patch.mode_map #= modes $
    MidiInst.patch#Patch.attribute_map #= keyswitches $
    MidiInst.named_patch (-24, 24) name controls
    where
    code = MidiInst.note_calls
        [ MidiInst.both "o" c_harmonic
        ]
    controls =
        [ (CC.mod, Controls.vib)
        , (CC.vib_speed, Controls.vib_speed)
        , (CC.pan, Controls.pan)
        , (15, "bow-force")
        , (16, "bow-pos")
        , (17, "bow-noise")
        -- <64 or >=64
        , (64, Controls.pedal) -- called sustain, but I use that elsewhere
        ]
    -- CC breakpoints are <=42, <=84, >=85
    keyswitches = Patch.cc_keyswitches_permute
        [ (32, [(mempty, 10), (Attrs.pizz, 60), (Attrs.legno, 100)])
        , (39, [(mempty, 10), (Attrs.harm, 60), (Attrs.harm<>Attrs.third, 100)])
        , (65, [(mempty, 0), (Attrs.mute, 127)]) -- con sord
        ]
    modes = Patch.cc_mode_map
        [ ("gesture", 33, [("expr", 10), ("bipolar", 60), ("bowing", 100)])
        , ("fingering", 36, [("mid", 10), ("bridge", 60), ("nut", 100)])
        , ("bow-lift", 37, [("f", 10), ("t", 80)])
        , ("bow-start", 38, [("d", 10), ("u", 80)])
        ]

c_harmonic :: Library.Calls Derive.Note
c_harmonic = Make.transform_notes Module.instrument "harmonic"
    (Tags.attr <> Tags.ly)
    "Harmonic, with lilypond for natural and artificial harmonic notation."
    ((,)
    <$> Sig.defaulted "n" 2 "Which harmonic. SWAM only supports 2 and 3."
    <*> Articulation.lily_harmonic_sig
    ) $ \(harm, lily_args) deriver -> Ly.when_lilypond
        (Articulation.lily_harmonic lily_args (htype harm) deriver)
        (harmonic harm deriver)
    where
    harmonic (h :: Int) deriver = case h of
        2 -> Call.add_attributes Attrs.harm $
            Call.add_constant Controls.nn (-12) deriver
        3 -> Call.add_attributes (Attrs.harm <> Attrs.third) $
            Call.add_constant Controls.nn (-19) deriver
        h -> Derive.throw $ "only 2nd and 3rd harmonics supported: " <> showt h
    -- TODO this doesn't look at open strings, so it will produce lilypond that
    -- doesn't correspond to what the synth plays.  Or maybe it's that the
    -- synth will happily play impossible things, but the notation tries to
    -- be realistic.
    htype 2 = Articulation.Natural
    htype _ = Articulation.Artificial
