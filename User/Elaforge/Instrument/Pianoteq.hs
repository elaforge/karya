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
import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN

import           Global


-- Supports MTS, aka real time tuning.
synth :: MidiInst.Synth
synth = MidiInst.synth "pianoteq" "Modartt Pianoteq" patches

patches :: [MidiInst.Patch]
patches =
    [ MidiInst.defaultPatch pbRange
        [ (67, "soft-pedal")
        , (69, "harmonic-pedal")
        , (66, "sost-pedal")
        , (64, Controls.pedal)
        -- whole bunch more
        ]
    , MidiInst.code #= Bali.pasangCode $ patch "pasang" []
    , MidiInst.nnRange (NN.g2, NN.a6) $ patch "yangqin" []
    , harp
    ]

pbRange :: Patch.PbRange
pbRange = (-24, 24)

harp :: MidiInst.Patch
harp = MidiInst.code #= code $ MidiInst.common#Common.doc #= doc $
    -- Ensure a known state.
    -- TODO pianoteq supports multiple MIDI channels for pitch, but not
    -- for pedals.  For this to work, the inst has to have only a single
    -- channel allocated.  Otherwise it will just go on a different channel and
    -- the performer doesn't know that pianoteq's channels are actually not
    -- independent WRT controls.
    MidiInst.controlDefaults [(gliss, 0), (harmonic, 0), (lute, 0)] $
    patch "harp"
        [ (67, gliss)
        , (69, harmonic)
        , (66, lute)
        , (64, damp)
        ]
    where
    code :: MidiInst.Code
    code = MidiInst.noteCalls
        [ MidiInst.both "o" $ Make.controlNote Module.instrument "o" harmonic 1
        , MidiInst.both "m" $ Make.controlNote Module.instrument "m" lute 1
        , MidiInst.generator "g" cGrace
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

cGrace :: Derive.Generator Derive.Note
cGrace = GraceUtil.makeGrace Module.instrument
    ("This is just like the standard `g` call, except it sets "
        <> ShowVal.doc gliss <> " and doesn't use `(`.")
    (Derive.withConstantControl gliss 1) (\_args events -> Sub.derive events)

patch :: InstT.Name -> [(Midi.Control, ScoreT.Control)] -> MidiInst.Patch
patch = MidiInst.namedPatch pbRange
