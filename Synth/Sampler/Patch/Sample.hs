-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A generic way to play a sample by name.
module Synth.Sampler.Patch.Sample where
import qualified Control.Monad.Except as Except
import qualified Data.Text as Text

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Call as Call
import qualified Derive.Call.Module as Module
import qualified Derive.Derive as Derive
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Sig as Sig

import qualified Perform.Im.Patch as Im.Patch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note

import           Global


patches :: [Patch.DbPatch]
patches = (:[]) $ Patch.DbPatch $ (Patch.patch patchName)
    { Patch._dir = dir
    , Patch._convert = convert
    , Patch._karyaPatch =
        ImInst.code #= code $
        ImInst.make_patch $ Im.Patch.patch
            { Im.Patch.patch_controls = Control.supportDyn }
    }
    where
    code = ImInst.note_generators [("s", c_sample)]
    dir = untxt patchName

patchName :: Text
patchName = "sample"

-- TODO start offset, pitch adjust
-- time stretch?  I need rubberband in sampler though.
c_sample :: Derive.Generator Derive.Note
c_sample = Derive.generator Module.instrument "sample" mempty doc $
    Sig.call ((,,)
        <$> Sig.required "name" "name of sample"
        <*> Sig.required "dir" "directory of sample"
        <*> Sig.defaulted "ext" "flac" "extension of sample"
    ) $ \(sample, dir, ext) args -> do
        when (Text.null sample) $ Derive.throw "no sample"
        when (Text.null dir) $ Derive.throw "no sample dir"
        Derive.with_val EnvKey.element (dir <> "/" <> sample <> "." <> ext) $
            Call.placed_note args
    where
    doc = "Play a single sample."

convert :: Note.Note -> Patch.ConvertM Sample.Sample
convert note = do
    filename <- if Note.element note == ""
        then Except.throwError "no element" else return $ Note.element note
    let dynVal = Note.initial0 Control.dynamic note
    return $ (Sample.make (untxt filename))
        { Sample.envelope = Util.asr dynVal 0.15 note }
