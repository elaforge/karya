-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Faust.PatchDb (synth, warnings) where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified System.IO.Unsafe as Unsafe

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.ScoreT as ScoreT
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
    code constantPitch constantControls $
    ImInst.make_patch $
    Patch.patch { Patch.patch_controls = pretty <$> controls }
    where
    constantControls = map fst $ filter (DriverC._constant . snd) $
        filter ((/=Control.pitch) . fst) $ Map.toList controls
    constantPitch = maybe False DriverC._constant $
        Map.lookup Control.pitch controls
    controls = DriverC._inputControls patch <> ui_controls
    ui_controls = Map.fromList $ do
        (control, controls@((_, ((), config)) : _)) <- by_control
        let elts = filter (/="") $ map (fst . fst) controls
        return $ (control,) $ config
            { DriverC._description =
                (if null elts then ""
                    else "elements: [" <> Text.intercalate ", " elts <> "], ")
                <> DriverC._description config
            }
    by_control = Seq.keyed_group_sort (snd . fst) $
        Map.toList $ DriverC._controls patch

    code False [] = id
    code constantPitch constantControls = (ImInst.code #=) $ ImInst.null_call $
        DUtil.constant_controls constantPitch
            (Set.fromList (map control constantControls))

control :: Control.Control -> ScoreT.Control
control (Control.Control c) = ScoreT.Control c
