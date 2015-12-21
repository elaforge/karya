-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Patches for my reyong samples.
module Local.Instrument.Kontakt.Reyong where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call as Call
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Instrument as Instrument
import Global


patches :: [MidiInst.Patch]
patches =
    [ (set_scale $ patch "reyong", code)
    , (patch "reyong12", code)
    ]
    where
    code = MidiInst.note_calls (MidiInst.null_call note_call)
    patch name = Instrument.set_flag Instrument.ConstantPitch $
        (Instrument.instrument_#Instrument.maybe_decay #= Just 0) $
        (Instrument.attribute_map #= attribute_map) $
        MidiInst.patch (-24, 24) name []
    tuning = BaliScales.Umbang -- TODO verify
    set_scale = (Instrument.scale #= Just patch_scale)
        . MidiInst.default_scale Legong.scale_id
        . MidiInst.environ EnvKey.tuning tuning
    -- Trompong starts at 2a, trompong + reyong has 15 keys.
    patch_scale =
        Legong.patch_scale (take 15 . drop (5+4) . Legong.strip_pemero) tuning

note_call :: Derive.Generator Derive.Note
note_call = Note.transformed_note doc mempty $ \args deriver -> do
    damp <- Derive.untyped_control_at damp_control
        =<< Args.real_start args
    Call.with_constant Controls.release_velocity (fromMaybe 0 damp) deriver
    where
    doc = "Convert " <> ShowVal.doc damp_control <> " to release damp level."

attribute_map :: Instrument.AttributeMap
attribute_map = Instrument.keyswitches $ map (second (map Instrument.Keyswitch))
    [ (Attrs.mute <> Attrs.closed, [13])
    , (Attrs.mute <> Attrs.open, [14])
    , (cek <> Attrs.closed, [15])
    , (cek <> Attrs.open, [16])
    , (damp_closed, [open, 18])
    , (mempty, [open, 17])
    ]
    where open = 12

cek :: Score.Attributes
cek = Score.attr "cek"

-- | Note releases use +mute+closed.  +mute+open is the default.
damp_closed :: Score.Attributes
damp_closed = Score.attr "damp-closed"

-- TODO move to Reyong calls

damp_control :: Score.Control
damp_control = "damp"
