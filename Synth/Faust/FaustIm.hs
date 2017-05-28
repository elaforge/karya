-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline synthesizer that uses FAUST.
module Synth.Faust.FaustIm (main) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Audio as Audio
import qualified Data.Conduit.Audio.Sndfile as Sndfile
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector.Storable as Storable

import qualified System.Environment as Environment
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Synth.Faust.Convert as Convert
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Types as Types

import Synth.Types
import Global


main :: IO ()
main = do
    args <- Environment.getArgs
    patches <- DriverC.getPatches
    let process_ = process patches Config.cache <=< either errorIO return
    case args of
        ["print-patches"] -> forM_ (Map.toList patches) $ \(name, patch) -> do
            Text.IO.putStrLn name
            print =<< DriverC.getControls patch
            print =<< DriverC.getUiControls patch
        [notesJson] -> process_ =<< Note.unserializeJson notesJson
        [] -> process_ . first pretty
            =<< Note.unserialize (Config.notes Config.faust)
        _ -> errorIO $ "usage: faust-im [notes.json | print-patches]"

process :: Map Types.PatchName DriverC.Patch -> FilePath -> [Note.Note] -> IO ()
process patches outputDir notes = do
    let output = outputDir </> "out.wav"
    putStrLn "processing"
    (errs, rendered) <- Either.partitionEithers <$>
        mapM (renderNote patches) notes
    mapM_ Text.IO.putStrLn errs
    putStrLn $ "writing " <> output
    result <- AUtil.catchSndfile $ Resource.runResourceT $
        Sndfile.sinkSnd output AUtil.outputFormat (AUtil.mix rendered)
    case result of
        Left err -> Log.error $
            "writing to output: " <> showt output <> ": " <> err
        Right () -> return ()
    putStrLn "done"

-- | Render samples for a single note.
renderNote :: Map Types.PatchName DriverC.Patch -> Note.Note
    -> IO (Either Text (RealTime, AUtil.Audio))
renderNote patches note = case Map.lookup (Note.instrument note) patches of
    Nothing -> return $ Left $ "no patch: " <> Note.instrument note
    Just patch -> DriverC.withInstrument patch $ \inst -> do
        patchControls <- map fst . snd <$> DriverC.getControls patch
        Convert.controls patchControls (Note.controls note) $
            renderNoteAudio note inst

-- | Render samples.
renderNoteAudio :: Note.Note -> DriverC.Instrument
    -> [(Convert.SignalP, Int)] -> IO (Either Text (RealTime, AUtil.Audio))
renderNoteAudio note inst controlLengths = do
    let start = AUtil.toFrames (Note.start note)
        end = AUtil.toFrames (Note.end note)
    buffers <- DriverC.render inst start end controlLengths
    return $ (Note.start note,) <$> case buffers of
        [left, right] -> Right $
            Audio.merge (audioSource left) (audioSource right)
        [center] -> Right $
            Audio.merge (audioSource center) (audioSource center)
        _ -> Left $ "expected 1 or 2 outputs for "
            <> Note.instrument note

audioSource :: Storable.Vector Float -> AUtil.Audio
audioSource samples = Audio.AudioSource
    { source = Conduit.yield samples
    , rate = fromIntegral Config.samplingRate
    , channels = 1
    , frames = Storable.length samples
    }
