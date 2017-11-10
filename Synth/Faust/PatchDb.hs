-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Faust.PatchDb (synth, warnings) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified System.IO.Unsafe as Unsafe

import qualified Util.Doc as Doc
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Perform.Im.Patch as Patch
import qualified Instrument.InstTypes as InstTypes
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

import Global


synth :: ImInst.Synth
synth = ImInst.synth Config.faustName "音 faust synthesizer" patches

patches :: [(InstTypes.Name, ImInst.Patch)]
warnings :: [Text]
(warnings, patches) = Unsafe.unsafePerformIO $ do
    -- These are in IO, but should be safe, because they are just reading
    -- static data.  In fact the FFI functions could probably omit the IO.
    patches <- DriverC.getPatches
    Either.partitionEithers <$> mapM make (Map.toList patches)
    where
    make (name, patch) = do
        result <- DriverC.getParsedMetadata patch
        return $ first (("faust/" <> name <> ": ") <>) $ do
            (doc, controls) <- result
            patch <- makePatch doc controls name
            return (name, patch)

makePatch :: Doc.Doc -> [(Control.Control, Text)] -> Note.PatchName
    -> Either Text ImInst.Patch
makePatch doc controls name = do
    let attackSampled = Map.findWithDefault [] name controlConfigs
    let unused = attackSampled List.\\ map fst controls
    unless (null unused) $
        Left $ "attack sampled controls don't exist: " <> pretty unused
    return $ ImInst.doc #= doc $ code attackSampled $
        ImInst.make_patch $
        Patch.patch { Patch.patch_controls = Map.fromList controls }
    where
    code [] = id
    code attackSampled = (ImInst.code #=) $ ImInst.null_call $
        DUtil.attack_sample_note (Set.fromList (map control attackSampled))

control (Control.Control c) = ScoreTypes.Control c

controlConfigs :: Map Note.PatchName [Control.Control]
controlConfigs = Map.fromList
    [ ("guitar", [Control.dynamic, "pos"])
    ]
