-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is the 音 variant of "Cmd.Instrument.MidiInst".  All of the the
-- generic bits can be re-exported.
module Cmd.Instrument.ImInst (
    module Cmd.Instrument.ImInst, module MidiInst
) where
import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
import Cmd.Instrument.MidiInst
       (generator, transformer, both, null_call, note_calls, note_generators,
        note_transformers, val_calls, postproc, cmd, thru)

import qualified Derive.EnvKey as EnvKey
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Perform.Im.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import Global


type Synth = Inst.SynthDecl Cmd.InstrumentCode

synth :: InstTypes.SynthName -> Text -> [(InstTypes.Name, Patch)] -> Synth
synth name doc patches =
    Inst.SynthDecl name doc (map (second make_inst) patches)

data Patch = Patch {
    patch_patch :: Patch.Patch
    , patch_common :: Common.Common MidiInst.Code
    }

make_patch :: Patch.Patch -> Patch
make_patch p = Patch
    { patch_patch = p
    , patch_common = Common.common mempty
    }

patch = Lens.lens patch_patch (\f r -> r { patch_patch = f (patch_patch r) })
common = Lens.lens patch_common
    (\f r -> r { patch_common = f (patch_common r) })

code :: Lens Patch MidiInst.Code
code = common # Common.code

doc :: Lens Patch Doc.Doc
doc = common # Common.doc

make_inst :: Patch -> Inst.Inst Cmd.InstrumentCode
make_inst (Patch patch common) = Inst.Inst
    { inst_backend = Inst.Im patch
    , inst_common = common
        { Common.common_code = MidiInst.make_code (Common.common_code common) }
    }

-- | The instrument will also set the given environ when it comes into scope.
environ :: RestrictedEnviron.ToVal a => EnvKey.Key -> a -> Patch -> Patch
environ name val = common#Common.environ
    %= (RestrictedEnviron.from_list [(name, RestrictedEnviron.to_val val)] <>)
