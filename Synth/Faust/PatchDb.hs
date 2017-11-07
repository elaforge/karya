-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Export a 'synth' with all the supported patches.
module Synth.Faust.PatchDb (synth, warnings) where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Doc as Doc
import qualified Cmd.Cmd as Cmd
import qualified Perform.Im.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

import Global


synth :: Inst.SynthDecl Cmd.InstrumentCode
warnings :: [Text]
(synth, warnings) = Unsafe.unsafePerformIO $ do
    -- These are in IO, but should be safe, because they are just reading
    -- static data.
    patches <- Map.toList <$> DriverC.getPatches
    inputs <- mapM (DriverC.patchInputs . snd) patches
    (warnings, patches) <- fmap Either.partitionEithers $
        forM (zip patches inputs) $ \((name, patch), inputs) -> do
            (doc, controls) <- DriverC.getControls patch
            return $ if length controls == inputs
                then Right (name, (doc, controls))
                else Left $ "faust/" <> name <> " input count " <> showt inputs
                    <> " doesn't match controls: " <> pretty (map fst controls)
    return (makeSynth patches, warnings)

-- | Declaration for "Local.Instrument".
makeSynth :: [(Note.PatchName, (Text, [(Control.Control, Text)]))]
    -> Inst.SynthDecl Cmd.InstrumentCode
makeSynth patches = Inst.SynthDecl Config.faustName "音 faust synthesizer"
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
makePatch controls =
    Patch.patch { Patch.patch_controls = Map.fromList controls }
