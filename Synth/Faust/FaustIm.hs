-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
-- | Offline synthesizer that uses FAUST.
module Synth.Faust.FaustIm (main) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector.Storable as Vector.Storable

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Posix.Process as Posix.Process
import qualified System.Posix.Signals as Signals

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Synth.Faust.Convert as Convert
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Lib.AUtil2 as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


main :: IO ()
main = do
    args <- Environment.getArgs
    patches <- DriverC.getPatches
    thread <- Concurrent.myThreadId
    -- Make sure I get some output if the process is killed.
    Signals.installHandler Signals.sigTERM
        (Signals.CatchOnce (Concurrent.killThread thread)) Nothing
    case args of
        ["print-patches"] -> forM_ (Map.toList patches) $ \(name, patch) -> do
            Text.IO.putStrLn $ name <> ":"
            result <- DriverC.getParsedMetadata patch
            case result of
                Left err -> Text.IO.putStrLn $ "ERROR: " <> err
                Right (doc, controls) -> do
                    Text.IO.putStrLn $ TextUtil.toText doc
                    forM_ controls $ \(c, cdoc) ->
                        Text.IO.putStrLn $ pretty c <> ": " <> pretty cdoc
                    uiControls <- DriverC.getUiControls patch
                    forM_ uiControls $ \(c, _, cdoc) ->
                        Text.IO.putStrLn $ "UI: " <> pretty c <> ": " <> cdoc
            putStrLn ""
        [notesFilename] -> do
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            pid <- Posix.Process.getProcessID
            let prefix = showt pid <> ": "
                    <> txt (FilePath.takeFileName notesFilename)
            process prefix patches notesFilename notes
        _ -> errorIO $ "usage: faust-im [notes | print-patches]"

process :: Text -> Map Note.PatchName DriverC.Patch -> FilePath -> [Note.Note]
    -> IO ()
process prefix patches notesFilename notes = do
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
            let output = Config.outputFilename (Config.rootDir Config.config)
                    notesFilename (Just inst)
            put $ inst <> " notes: " <> showt (length notes) <> " -> "
                <> txt output
            audio <- renderInstrument patch notes
            Directory.createDirectoryIfMissing True
                (FilePath.takeDirectory output)
            result <- AUtil.catchSndfile $ Resource.runResourceT $
                Audio.File.write AUtil.outputFormat output audio
            case result of
                Left err -> put $ inst <> " writing " <> showt output <> ": "
                    <> err
                Right () -> put $ inst <> " done"
    put "done"
    where
    flatten patchInstNotes =
        [ (patch, inst, notes)
        | (patch, instNotes) <- patchInstNotes
        , (inst, notes) <- instNotes
        ]

lookupPatches :: Map Note.PatchName patch -> [Note.Note]
    -> ([Note.PatchName], [(patch, [(Note.InstrumentName, [Note.Note])])])
lookupPatches patches notes =
    Either.partitionEithers $
        map (\(patch, instNotes) -> (, instNotes) <$> find patch) $
        map (second (Seq.keyed_group_sort Note.instrument)) $
        Seq.keyed_group_sort Note.patch notes
    where
    find patch = maybe (Left patch) Right $ Map.lookup patch patches

renderInstrument :: DriverC.Patch -> [Note.Note] -> IO AUtil.Audio
renderInstrument patch notes = DriverC.withInstrument patch $ \inst -> do
    supported <- DriverC.getControls patch
    let gate = if Control.gate `elem` supported
            then makeGate notes else mempty
    let controls = Map.insert Control.gate gate $ mergeControls supported notes
    Convert.controls supported controls $ \controlLengths -> do
        let start = 0
            end = maybe 0 (AUtil.toFrames . (+decay) . Note.end)
                (Seq.last notes)
            decay = 2
            -- TODO how can I figure out how long decay actually is?
            -- I probably have to keep rendering until the samples stay below
            -- a threshold.
        buffers <- DriverC.render inst start (fromIntegral end) controlLengths
        case buffers of
            [left, right] -> return $
                Audio.mergeChannels (fromSamples left) (fromSamples right)
            [center] -> return $
                Audio.mergeChannels (fromSamples center) (fromSamples center)
            -- This should have been verified by DriverC.getParsedMetadata.
            _ -> errorIO $ "expected 1 or 2 outputs, but got "
                <> showt (length buffers)

mergeControls :: [Control.Control] -> [Note.Note]
    -> Map Control.Control Signal.Signal
mergeControls supported notes =
    Map.fromList $ filter (not . Signal.null . snd) $ map merge supported
    where
    merge control = (control,) $ mconcat
        [ Signal.clip_before (Note.start n) signal
        | n <- notes
        , signal <- maybe [] (:[]) $ Map.lookup control (Note.controls n)
        ]

-- | This makes a sawtooth that goes to 1 on every note start.  This is
-- suitable for percussion, but maybe continuious instruments expect a constant
-- 1 for as long as the note is sustained?
makeGate :: [Note.Note] -> Signal.Signal
makeGate notes = Signal.from_pairs $
    concat [[(Note.start n, 0), (Note.start n, 1)]  | n <- notes]

fromSamples :: Monad m => Vector.Storable.Vector Audio.Sample
    -> Audio.AudioM m rate 1
fromSamples = Audio.fromSamples . (:[])

-- audioSource :: Storable.Vector Float -> AUtil.Audio
-- audioSource samples = Audio.AudioSource
--     { source = Conduit.yield samples
--     , rate = fromIntegral Config.samplingRate
--     , channels = 1
--     , frames = Storable.length samples
--     }
