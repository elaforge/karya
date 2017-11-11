-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Instruments for the offline NESS synthesizer.
module Local.Instrument.Ness where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.ScoreTypes as ScoreTypes

import qualified Perform.Im.Patch as Patch
import qualified Instrument.Inst as Inst
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import Global
import qualified Ness.Guitar as Guitar
import qualified Ness.Guitar.Patch as Guitar.Patch
import qualified Ness.Multiplate as Multiplate
import qualified Ness.Multiplate.Patch as Multiplate.Patch
import qualified Ness.Patches as Patches


synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = ImInst.synth Config.nessName
    "Write notes to a file, to submit to NESS by hand." $
    map (second make) (Map.toList Patches.patches)

make :: Patches.Patch -> ImInst.Patch
make patch = case patch of
    Patches.PGuitar inst -> guitar inst
    Patches.PMultiplate inst -> multiplate inst

guitar :: Guitar.Instrument -> ImInst.Patch
guitar inst = ImInst.code #= code $ ImInst.environ EnvKey.open_strings strings $
    ImInst.make_patch $ Patch.patch
        { Patch.patch_controls = Map.fromList
            [ (Control.pitch, "")
            , (Control.dynamic, "")
            , (Guitar.Patch.c_location, "")
            , (Guitar.Patch.c_finger, "")
            ]
        , Patch.patch_element_key = Just EnvKey.string
        , Patch.patch_attribute_map = Patch.attribute_map [Attrs.mute]
        }
    where
    strings = map Guitar.sNn $ Guitar.iStrings inst
    code = ImInst.null_call $ DUtil.attack_sample_note $
        Set.fromList $ map control [Guitar.Patch.c_location, Control.dynamic]

control :: Control.Control -> ScoreTypes.Control
control (Control.Control c) = ScoreTypes.Control c

multiplate :: Multiplate.Instrument -> ImInst.Patch
multiplate inst = ImInst.code #= code $ ImInst.make_patch patch
    where
    code = ImInst.note_calls
        [ ImInst.generator (Expr.Symbol object) $ generator object
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
