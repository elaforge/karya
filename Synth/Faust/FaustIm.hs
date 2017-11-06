-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline synthesizer that uses FAUST.
module Synth.Faust.FaustIm (main) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Audio as Audio
import qualified Data.Conduit.Audio.Sndfile as Sndfile
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector.Storable as Storable

import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Posix.Process as Posix.Process
import qualified System.Posix.Signals as Signals

import qualified Util.Log as Log
import qualified Synth.Faust.Convert as Convert
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note
import Synth.Types

import Global


main :: IO ()
main = do
    args <- Environment.getArgs
    patches <- DriverC.getPatches
    -- Make sure I get some output if the process is killed.
    thread <- Concurrent.myThreadId
    Signals.installHandler Signals.sigTERM
        (Signals.CatchOnce (Concurrent.killThread thread)) Nothing
    case args of
        ["print-patches"] -> forM_ (Map.toList patches) $ \(name, patch) -> do
            Text.IO.putStrLn name
            print =<< DriverC.getControls patch
            print =<< DriverC.getUiControls patch
        [notesFilename] -> do
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process patches notesFilename notes
        _ -> errorIO $ "usage: faust-im [notes | print-patches]"

process :: Map Note.PatchName DriverC.Patch -> FilePath -> [Note.Note] -> IO ()
process patches notesFilename notes = do
    pid <- Posix.Process.getProcessID
    let put = Text.IO.putStrLn . (prefix<>)
        prefix = showt pid <> ": " <> txt (FilePath.takeFileName notesFilename)
            <> ": "
    -- TODO divide up output by instrument instead of mixing them here
    let output = Config.outputFilename notesFilename Nothing

    -- This should catch SIGINT, so I can see who was killed.
    let catchAsync :: Exception.AsyncException -> IO ()
        catchAsync exc = put $ "exception: " <> showt exc
    Exception.handle catchAsync $ do
        put $ "processing " <> showt (length notes) <> " notes"
        (errs, rendered) <- fmap Either.partitionEithers $
            forM (zip [0..] notes) $ \(i, n) -> do
                when (i > 0 && i `mod` 10 == 0) $
                    put $ "note #" <> showt i
                renderNote patches n
        mapM_ put errs
        put $ "writing " <> txt output
        result <- AUtil.catchSndfile $ Resource.runResourceT $
            Sndfile.sinkSnd output AUtil.outputFormat (AUtil.mix rendered)
        case result of
            Left err -> Log.error $
                "writing to output: " <> showt output <> ": " <> err
            Right () -> return ()
        put "done"

-- | Render samples for a single note.
renderNote :: Map Note.PatchName DriverC.Patch -> Note.Note
    -> IO (Either Text (RealTime, AUtil.Audio))
renderNote patches note = case Map.lookup (Note.patch note) patches of
    Nothing -> return $ Left $ "no patch: " <> Note.patch note
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
        _ -> Left $ "expected 1 or 2 outputs for " <> Note.patch note

audioSource :: Storable.Vector Float -> AUtil.Audio
audioSource samples = Audio.AudioSource
    { source = Conduit.yield samples
    , rate = fromIntegral Config.samplingRate
    , channels = 1
    , frames = Storable.length samples
    }
