-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Modartt's amazing Pianoteq softsynth.
module User.Elaforge.Instrument.Pianoteq (synth) where
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Call.GraceUtil as GraceUtil
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Sub as Sub
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN

import           Global


-- Supports MTS, aka real time tuning.
synth :: MidiInst.Synth
synth = MidiInst.synth "pianoteq" "Modartt Pianoteq" patches

patches :: [MidiInst.Patch]
patches =
    [ MidiInst.default_patch pb_range
        [ (67, "soft-pedal")
        , (69, "harmonic-pedal")
        , (66, "sost-pedal")
        , (64, Controls.pedal)
        -- whole bunch more
        ]
    , MidiInst.code #= Bali.pasang_code $ patch "pasang" []
    , MidiInst.nn_range (NN.g2, NN.a6) $ patch "yangqin" []
    , harp
    ]

pb_range :: Patch.PbRange
pb_range = (-24, 24)

harp :: MidiInst.Patch
harp = MidiInst.code #= code $ MidiInst.common#Common.doc #= doc $
    -- Ensure a known state.
    -- TODO pianoteq supports multiple MIDI channels for pitch, but not
    -- for pedals.  For this to work, the inst has to have only a single
    -- channel allocated.  Otherwise it will just go on a different channel and
    -- the performer doesn't know that pianoteq's channels are actually not
    -- independent WRT controls.
    MidiInst.control_defaults [(gliss, 0), (harmonic, 0), (lute, 0)] $
    patch "harp"
        [ (67, gliss)
        , (69, harmonic)
        , (66, lute)
        , (64, damp)
        ]
    where
    code :: MidiInst.Code
    code = MidiInst.note_calls
        [ MidiInst.both "o" $ Make.control_note Module.instrument "o" harmonic 1
        , MidiInst.both "m" $ Make.control_note Module.instrument "m" lute 1
        , MidiInst.generator "g" c_grace
        ]
    -- TODO add diatonic gliss call, like zheng
    harmonic = "harmonic"
    lute = "lute"
    damp = "damp"
    doc = "The harp has a backwards sustain pedal, in that it sustains by\
        \ default unless " <> ShowVal.doc damp <> " is 1.  The `ped`\
        \ control call is useful to quickly damp ringing notes."

gliss :: ScoreT.Control
gliss = "gliss"

c_grace :: Derive.Generator Derive.Note
c_grace = GraceUtil.make_grace Module.instrument
    ("This is just like the standard `g` call, except it sets "
        <> ShowVal.doc gliss <> " and doesn't use `(`.")
    (Derive.with_constant_control gliss 1) (\_args events -> Sub.derive events)

patch :: InstTypes.Name -> [(Midi.Control, ScoreT.Control)] -> MidiInst.Patch
patch = MidiInst.named_patch pb_range
