-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Instruments for the offline NESS synthesizer.
module User.Elaforge.Instrument.Ness where
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.Make as Make
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Library as Library
import qualified Derive.PSignal as PSignal
import qualified Derive.REnv as REnv
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Im.Patch as Patch
import qualified Perform.Pitch as Pitch
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
guitar inst = ImInst.code #= code $ ImInst.environ EnvKey.openStrings strings $
    ImInst.makePatch $ Patch.patch
        { Patch.patchControls = Map.fromList
            [ (Control.pitch, "")
            , (Control.dynamic, "")
            , (Guitar.Patch.cLocation, "Pluck location.")
            -- TODO use Derive.Controls.finger
            , (Guitar.Patch.cFinger, "Stopping finger weight.")
            ]
        , Patch.patchAttributeMap = Patch.makeAttributeMap [Attrs.mute]
        }
    where
    strings = map makeString $ Guitar.iStrings inst
    code = note <> postproc
    postproc = ImInst.postproc $
        DUtil.moveVal EnvKey.string EnvKey.element showStr
    note = ImInst.nullCall $ DUtil.constantControls False $
        Set.fromList $ map control [Guitar.Patch.cLocation, Control.dynamic]

showStr :: PSignal.Pitch -> Either Log.Msg Text
showStr = bimap (Log.msg Log.Warn Nothing . pretty) Pitch.noteText
    . PSignal.pitchNote . PSignal.coerce

makeString :: Guitar.String -> REnv.ConstantPitch
makeString str = REnv.ConstantPitch Twelve.scaleId
    (Pitch.Note (Guitar.sName str)) (Guitar.sNn str)
    -- TODO Twelve.scale_id may well be wrong, which can cause parsing errors.
    -- The string should also give the scale.

control :: Control.Control -> ScoreT.Control
control (Control.Control c) = ScoreT.Control c

multiplate :: Multiplate.Instrument -> ImInst.Patch
multiplate inst = ImInst.code #= code $ ImInst.makePatch patch
    where
    code = ImInst.noteCalls
        [ ImInst.generator (Expr.Symbol object) $ generator object
        | object <- Multiplate.iObjects inst
        ]
    generator object = Library.generator $ Make.environNote Module.instrument
        (Derive.CallName object) mempty "Strike the named object."
        EnvKey.element object
    patch = Patch.patch
        { Patch.patchControls = Map.fromList
            [ (Control.dynamic, "")
            , (Multiplate.Patch.cX, "")
            , (Multiplate.Patch.cY, "")
            , (Multiplate.Patch.cDuration, "")
            ]
        }
