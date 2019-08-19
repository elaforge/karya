-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Faust.PatchDb (synth, warnings) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Doc as Doc
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.ScoreT as ScoreT
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Perform.Im.Patch as Patch
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control

import           Global


synth :: ImInst.Synth
synth = ImInst.synth Config.faustName "音 faust synthesizer" patches

patches :: [(InstTypes.Name, ImInst.Patch)]
warnings :: [Text]
(warnings, patches) = Unsafe.unsafePerformIO $ do
    -- These are in IO, but should be safe, because they are just reading
    -- static data.  In fact the FFI functions could probably omit the IO.
    pmap <- DriverC.getPatches
    let errors =
            [ "faust/" <> name <> ": " <> err
            | (name, Left err) <- Map.toList pmap
            ]
        patches = [(name, patch) | (name, Right patch) <- Map.toList pmap]
    return (errors, map (second makePatch) patches)

makePatch :: DriverC.Patch -> ImInst.Patch
makePatch patch =
    ImInst.doc #= Doc.Doc (DriverC._doc patch) $
    ImInst.common#Common.flags #= (if DriverC._triggered patch
        then Set.singleton Common.Triggered else mempty) $
    code constantPitch constantControls $
    ImInst.make_patch $ Patch.patch
        { Patch.patch_controls = (pretty <$> controls) <> standardControls
        }
    where
    constantControls = map fst $ filter (DriverC._constant . snd) $
        filter ((/=Control.pitch) . fst) $ Map.toList controls
    constantPitch = maybe False DriverC._constant $
        Map.lookup Control.pitch controls
    controls = DriverC.imControls patch
    code False [] = id
    code constantPitch constantControls = (ImInst.code #=) $ ImInst.null_call $
        DUtil.constant_controls constantPitch
            (Set.fromList (map control constantControls))

control :: Control.Control -> ScoreT.Control
control (Control.Control c) = ScoreT.Control c

standardControls :: Map Control.Control Text
standardControls = Map.fromList
    [ (Control.volume, "Low level volume, in dB.")
    -- , (Control.pan, "Pan, where -1 is left, and 1 is right.")
    ]
    -- All instruments put dyn in the gate signal.
    <> Control.supportDyn
