-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Faust.PatchDb (synthName, synth) where
import qualified System.IO.Unsafe as Unsafe
import qualified Data.Map as Map

import qualified Util.Doc as Doc
import qualified Cmd.Cmd as Cmd
import qualified Derive.ScoreTypes as ScoreTypes
import qualified Perform.Im.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Types as Types

import Global


synthName :: Text
synthName = "faust"

synth :: Inst.SynthDecl Cmd.InstrumentCode
synth = Unsafe.unsafePerformIO $ do
    -- These are in IO, but should be safe, because they are just reading
    -- static data.
    patches <- DriverC.getPatches
    patches <- forM (Map.toList patches) $ \(name, patch) -> do
        (name,) <$> DriverC.getControls patch
    return $ makeSynth patches

-- | Declaration for "Local.Instrument".
makeSynth :: [(Types.PatchName, (Text, [(Control.Control, Text)]))]
    -> Inst.SynthDecl Cmd.InstrumentCode
makeSynth patches = Inst.SynthDecl synthName "音 faust synthesizer"
    [ (name, makeInst description controls)
    | (name, (description, controls)) <- patches
    ]

makeInst :: Text -> [(Control.Control, Text)] -> Inst.Inst Cmd.InstrumentCode
makeInst description controls = Inst.Inst
    { inst_backend = Inst.Im $ makePatch controls
    , inst_common = (Common.common Cmd.empty_code)
        { Common.common_doc = Doc.Doc description }
    }

makePatch :: [(Control.Control, Text)] -> Patch.Patch
makePatch controls = Patch.Patch
    { patch_controls = Map.fromList
        [(ScoreTypes.Control c, doc) | (Control.Control c, doc) <- controls]
    , patch_attribute_map = Common.attribute_map []
    , patch_flags = mempty
    }
