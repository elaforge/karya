-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Ness where
import qualified Data.Map as Map

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.RestrictedEnviron as RestrictedEnviron

import qualified Perform.Im.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.Patch as Guitar.Patch
import qualified Ness.Instruments as Instruments
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.Patch as Multiplate.Patch


synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Inst.SynthDecl Config.nessName
    "Write notes to a file, to submit to NESS by hand." $
    map make (Map.toList Instruments.instruments)

make :: (name, Instruments.Instrument) -> (name, Inst.Inst Cmd.InstrumentCode)
make (name, instrument) = (name,) $ case instrument of
    Instruments.IGuitar inst -> guitar inst
    Instruments.IMultiplate inst -> multiplate inst

guitar :: Guitar.Instrument -> Inst.Inst Cmd.InstrumentCode
guitar inst =
    Inst.Inst (Inst.Im patch)
        (environ EnvKey.open_strings strings $ Common.common Cmd.empty_code)
    where
    strings = map Guitar.sNn $ Guitar.iStrings inst
    patch = Patch.patch
        { Patch.patch_controls = Map.fromList
            [ (Control.pitch, "")
            , (Control.dynamic, "")
            , (Guitar.Patch.c_location, "")
            , (Guitar.Patch.c_finger, "")
            ]
        , Patch.patch_element_key = Just EnvKey.string
        , Patch.patch_attribute_map = Patch.attribute_map [Attrs.mute]
        }

-- -- | The instrument will also set the given environ when it comes into scope.
environ :: RestrictedEnviron.ToVal a => EnvKey.Key -> a
    -> Common.Common code -> Common.Common code
environ name val = Common.environ
    %= (RestrictedEnviron.from_list [(name, RestrictedEnviron.to_val val)] <>)

multiplate :: Multiplate.Instrument -> Inst.Inst Cmd.InstrumentCode
multiplate inst =
    Inst.Inst (Inst.Im patch) (Common.common (MidiInst.make_code code))
    where
    code = MidiInst.note_calls
        [ MidiInst.generator (Expr.Symbol object) $ generator object
        | object <- Multiplate.iObjects inst
        ]
    generator object = fst $ Make.environ_note Module.instrument
        (Derive.CallName object) mempty "doc" Multiplate.Patch.k_object object
    patch = Patch.patch
        { Patch.patch_controls = Map.fromList
            [ (Control.dynamic, "")
            , (Multiplate.Patch.c_x, "")
            , (Multiplate.Patch.c_y, "")
            , (Multiplate.Patch.c_duration, "")
            ]
        , Patch.patch_element_key = Just Multiplate.Patch.k_object
        }
