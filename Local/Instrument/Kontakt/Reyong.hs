-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Patches for my reyong samples.
module Local.Instrument.Kontakt.Reyong where
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.Call.Prelude.Articulation as Articulation
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Call.Sub as Sub
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Patch as Patch
import Global


patches :: [MidiInst.Patch]
patches =
    [ set_scale $ patch "reyong"
    , patch "reyong12"
    ]
    where
    code = MidiInst.note_calls null_call
    null_call = MidiInst.null_call $ DUtil.zero_duration "note"
        ("When the event has zero duration, dispatch to the "
            <> ShowVal.doc Articulation.mute_call <> " call.")
        (\args ->
            Eval.reapply_call (Args.context args) Articulation.mute_call [])
        (Sub.inverting $ Note.default_note Note.use_attributes)
    patch name = MidiInst.code #= code $ set_params $
        MidiInst.named_patch (-24, 24) name []
    set_params = (MidiInst.patch %=) $
        Patch.set_flag Patch.ConstantPitch
        . (Patch.decay #= Just 0)
        . (Patch.attribute_map #= attribute_map)
    tuning = BaliScales.Umbang -- TODO verify how mine are tuned
    set_scale = (MidiInst.patch#Patch.scale #= Just instrument_scale)
        . MidiInst.default_scale Legong.scale_id
        . MidiInst.environ EnvKey.tuning tuning
    -- Trompong starts at 3a, trompong + reyong has 15 keys.
    instrument_scale =
        Legong.instrument_scale (take 15 . drop 4 . Legong.strip_pemero) tuning

attribute_map :: Patch.AttributeMap
attribute_map = Patch.keyswitches $ map at
    [ (Reyong.cek <> Attrs.open, 4)
    , (Reyong.cek, 3)
    , (Attrs.mute <> Attrs.open, 2)
    , (Attrs.mute, 1)
    , (mempty, 0)
    ]
    where at = second ((:[]) . Patch.Aftertouch)
