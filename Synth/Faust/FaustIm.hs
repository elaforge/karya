-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline synthesizer that uses FAUST.
module Synth.Faust.FaustIm (main) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.Posix.Process as Posix.Process
import qualified System.Posix.Signals as Signals

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Perform.RealTime as RealTime
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Faust.Render as Render
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Note as Note

import           Global


-- | If True, write control signals to separate files for visualization.
debugControls :: Bool
debugControls = False

useCheckpoints :: Bool
useCheckpoints = True

main :: IO ()
main = do
    args <- Environment.getArgs
    patches <- DriverC.getPatches
    thread <- Concurrent.myThreadId
    -- Make sure I get some output if the process is killed.
    Signals.installHandler Signals.sigTERM
        (Signals.CatchOnce (Concurrent.killThread thread)) Nothing
    case args of
        ["print-patches"] -> forM_ (Map.toList patches) $ \(name, epatch) -> do
            Text.IO.putStrLn $ "=== " <> name <> " ==="
            case epatch of
                Left err -> Text.IO.putStrLn $ "ERROR: " <> err
                Right patch -> printPatch patch
            putStrLn ""
        [notesFilename, outputDir] -> do
            Log.notice $ Text.unwords
                ["faust-im", txt notesFilename, txt outputDir]
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            pid <- Posix.Process.getProcessID
            let prefix = showt pid <> ": " <> txt notesFilename
            patches <- traverse (either errorIO pure) patches
            process prefix patches notes outputDir
        _ -> errorIO $ "usage: faust-im [print-patches | notes outputDir]"
    where
    printPatch patch = do
        Text.IO.putStrLn $ DriverC._doc patch
        forM_ (Map.toList (DriverC._inputControls patch)) $ \(c, config) ->
            Text.IO.putStrLn $ "input: " <> pretty c <> ": " <> pretty config
        forM_ (Map.toList (DriverC._controls patch)) $ \(c, (_, config)) ->
            Text.IO.putStrLn $ "control: " <> showControl c <> ": "
                <> pretty config

showControl :: DriverC.Control -> Text
showControl ("", c) = pretty c
showControl (elt, c) = elt <> ":" <> pretty c

process :: Text -> Map Note.PatchName DriverC.Patch -> [Note.Note] -> FilePath
    -> IO ()
process prefix patches notes outputDir = do
    putLock <- MVar.newMVar ()
    let put msg = MVar.withMVar putLock $ const $
            Text.IO.putStrLn $ prefix <> ": " <> msg
    put $ "processing " <> showt (length notes) <> " notes"
    let (notFound, patchInstNotes) = lookupPatches patches notes
    unless (null notFound) $
        put $ "patches not found: " <> Text.unwords notFound
    -- Signals.installHandler above will make SIGINT throw.
    let async :: Exception.AsyncException -> IO ()
        async exc = put $ "exception: " <> showt exc
    Exception.handle async $ Async.forConcurrently_ (flatten patchInstNotes) $
        \(patch, inst, notes) -> do
            let output = if useCheckpoints
                    then outputDir </> untxt inst
                    else outputDir </> untxt inst <> ".wav"
            put $ inst <> " notes: " <> showt (length notes) <> " -> "
                <> txt output
            Directory.createDirectoryIfMissing True $
                if useCheckpoints then output else FilePath.takeDirectory output
            (result, elapsed) <- Thread.timeActionText $ if useCheckpoints
                then Render.write output
                    (Set.fromList $ mapMaybe Note.trackId notes) patch notes
                else fmap (second (\() -> (0, 0))) $ AUtil.catchSndfile $
                    Resource.runResourceT $
                    Audio.File.write AUtil.outputFormat output $
                    Render.renderPatch patch Render.defaultConfig
                        Nothing (\_ -> return ()) notes 0
            when debugControls $
                writeControls output patch notes
            case result of
                Left err -> put $ inst <> " writing " <> txt output
                    <> ": " <> err
                Right (rendered, total) ->
                    put $ inst <> " " <> showt rendered <> "/"
                        <> showt total <> " chunks: " <> txt output
                        <> " (" <> elapsed <> ")"
    put "done"
    where
    flatten patchInstNotes =
        [ (patch, inst, notes)
        | (patch, instNotes) <- patchInstNotes
        , (inst, notes) <- instNotes
        ]

writeControls :: FilePath -> DriverC.Patch -> [Note.Note] -> IO ()
writeControls output patch notes =
    forM_ controls $ \control -> Resource.runResourceT $
        Audio.File.write AUtil.outputFormat (fname control) $
        Audio.take (Audio.Seconds final) $
        fromMaybe Audio.silence
        (Render.renderInput chunkSize notes 0 control :: Maybe AUtil.Audio1)
    where
    chunkSize = Render._chunkSize Render.defaultConfig
    final = RealTime.to_seconds $ maybe 0 Note.end (Seq.last notes)
    -- play_cache is special-cased to ignore *.debug.wav.
    fname c = FilePath.dropExtension output <> "-" <> prettys c <> ".debug.wav"
    controls = Map.keys (DriverC._inputControls patch)

lookupPatches :: Map Note.PatchName patch -> [Note.Note]
    -> ([Note.PatchName], [(patch, [(Note.InstrumentName, [Note.Note])])])
lookupPatches patches notes =
    Either.partitionEithers $
        map (\(patch, instNotes) -> (, instNotes) <$> find patch) $
        map (second (Seq.keyed_group_sort Note.instrument)) $
        Seq.keyed_group_sort Note.patch notes
    where
    find patch = maybe (Left patch) Right $ Map.lookup patch patches
