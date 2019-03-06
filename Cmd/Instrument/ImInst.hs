-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is the 音 variant of "Cmd.Instrument.MidiInst".  All of the the
-- generic bits can be re-exported.
module Cmd.Instrument.ImInst (
    module Cmd.Instrument.ImInst, module MidiInst
) where
import qualified Data.Set as Set

import qualified Util.Doc as Doc
import qualified Util.Lens as Lens
import qualified Cmd.Cmd as Cmd
import qualified Cmd.EditUtil as EditUtil
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.MidiInst as MidiInst
import           Cmd.Instrument.MidiInst
    (allocations, both, cmd, generator, inst_range, make_code, note_calls,
     note_generators, note_transformers, null_call, postproc, transformer,
     val_calls, Code)
import qualified Cmd.MidiThru as MidiThru

import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Scale as Scale
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes

import qualified Perform.Im.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Osc as Osc
import qualified Ui.UiConfig as UiConfig

import           Global


type Synth = Inst.SynthDecl Cmd.InstrumentCode

synth :: InstTypes.SynthName -> Text -> [(InstTypes.Name, Patch)] -> Synth
synth name doc patches =
    Inst.SynthDecl name doc (map (second make_inst) patches)

data Patch = Patch {
    patch_patch :: Patch.Patch
    , patch_common :: Common.Common Code
    }

make_patch :: Patch.Patch -> Patch
make_patch p = Patch
    { patch_patch = p
    , patch_common = Common.common mempty
    }

patch = Lens.lens patch_patch (\f r -> r { patch_patch = f (patch_patch r) })
common = Lens.lens patch_common
    (\f r -> r { patch_common = f (patch_common r) })

code :: Lens Patch Code
code = common # Common.code

doc :: Lens Patch Doc.Doc
doc = common # Common.doc

make_inst :: Patch -> Inst.Inst Cmd.InstrumentCode
make_inst (Patch patch common) = Inst.Inst
    { inst_backend = Inst.Im patch
    , inst_common = common
        { Common.common_code = MidiInst.make_code (Common.common_code common) }
    }

-- TODO: these are copy paste from MidiInst, only 'common' is different.
-- I should be able to share the code.

-- | The instrument will also set the given environ when it comes into scope.
environ :: RestrictedEnviron.ToVal a => EnvKey.Key -> a -> Patch -> Patch
environ name val = common#Common.environ
    %= (RestrictedEnviron.from_list [(name, RestrictedEnviron.to_val val)] <>)

-- | The instrument will set the given scale when it comes into scope.
default_scale :: Pitch.ScaleId -> Patch -> Patch
default_scale = environ EnvKey.scale . Expr.scale_id_to_str

-- | Set instrument range.
range :: Scale.Range -> Patch -> Patch
range range =
    environ EnvKey.instrument_bottom (Scale.range_bottom range)
    . environ EnvKey.instrument_top (Scale.range_top range)

nn_range :: (Pitch.NoteNumber, Pitch.NoteNumber) -> Patch -> Patch
nn_range (bottom, top) =
    environ EnvKey.instrument_bottom bottom
    . environ EnvKey.instrument_top top

-- | Adapt a 'Osc.ThruFunction' to 'Cmd.ThruFunction'.
thru :: Osc.ThruFunction -> Code
thru thru_f = MidiInst.thru convert
    where
    convert scale attrs input = do
        inst <- Cmd.abort_unless =<< EditUtil.lookup_instrument
        MidiThru.convert_input inst scale input >>= \case
            InputNote.NoteOn _ pitch velocity ->
                case thru_f attrs pitch velocity of
                    Left err -> Cmd.throw err
                    Right plays -> return $ map (Cmd.ImThru . Osc.play) plays
            _ -> return []

add_flag :: Common.Flag -> Patch -> Patch
add_flag flag = common#Common.flags %= Set.insert flag

triggered :: Patch -> Patch
triggered = add_flag Common.Triggered

im_allocations :: [(ScoreT.Instrument, Text, Common.Config -> Common.Config)]
    -- ^ (inst, qualified, set_config)
    -> UiConfig.Allocations
im_allocations = UiConfig.make_allocations . map (_make_allocation UiConfig.Im)

dummy_allocations :: [(ScoreT.Instrument, Text, Common.Config -> Common.Config)]
    -- ^ (inst, qualified, set_config)
    -> UiConfig.Allocations
dummy_allocations =
    UiConfig.make_allocations . map (_make_allocation UiConfig.Dummy)

_make_allocation :: UiConfig.Backend
    -> (a, Text, Common.Config -> Common.Config) -> (a, UiConfig.Allocation)
_make_allocation backend (name, qualified, set_config) =
    ( name
    , UiConfig.Allocation
        { alloc_qualified = InstTypes.parse_qualified qualified
        , alloc_config = set_config Common.empty_config
        , alloc_backend = backend
        }
    )
