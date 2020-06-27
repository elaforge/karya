-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline synthesizer that uses FAUST.
module Synth.Faust.FaustIm (main) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import           System.FilePath ((</>))
import qualified System.Posix.Signals as Signals

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Faust.EffectC as EffectC
import qualified Synth.Faust.Preview as Preview
import qualified Synth.Faust.Render as Render
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Note as Note

import           Global


main :: IO ()
main = do
    Log.configure $ \st -> st { Log.state_priority = Log.Notice }
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    patches <- DriverC.getPatches
    thread <- Concurrent.myThreadId
    -- Make sure I get some output if the process is killed.
    Signals.installHandler Signals.sigTERM
        (Signals.CatchOnce (Concurrent.killThread thread)) Nothing
    case args of
        ["print-effects"] -> do
            patches <- EffectC.getPatches
            forM_ (Map.toList patches) $ \(name, epatch) -> do
                Text.IO.putStrLn $ "=== " <> name <> " ==="
                case epatch of
                    Left err -> Text.IO.putStrLn $ "ERROR: " <> err
                    Right patch -> printEffect patch
                putStrLn ""
        ["print-patches"] -> forM_ (Map.toList patches) $ \(name, epatch) -> do
            Text.IO.putStrLn $ "=== " <> name <> " ==="
            case epatch of
                Left err -> Text.IO.putStrLn $ "ERROR: " <> err
                Right patch -> printPatch patch
            putStrLn ""
        ["print-patches", patch] -> case Map.lookup (txt patch) patches of
            Nothing -> Text.IO.putStrLn $ "no patch: " <> txt patch
            Just (Left err) -> Text.IO.putStrLn $ "ERROR: " <> err
            Just (Right patch) -> printPatch patch
        ["render-preview", patch] -> case Map.lookup (txt patch) patches of
            Nothing -> errorIO $ "no such patch: " <> txt patch
            Just (Left err) -> errorIO $ "loading patch " <> txt patch <> ": "
                <> err
            Just (Right patch) -> Preview.render patch
        ["render-preview"] -> mapM_ Preview.render $
            Either.rights (Map.elems patches)
        ["dump", notesFilename] -> do
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            patches <- traverse (either errorIO pure) patches
            dump patches notes
        [notesFilename, outputDir] -> do
            Log.notice $ Text.unwords
                ["faust-im", txt notesFilename, txt outputDir]
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            patches <- traverse (either errorIO pure) patches
            process (Progress `elem` flags) patches notes outputDir
        _ -> usage ""
    where
    printPatch patch = do
        put $ DriverC._doc patch
        when (DriverC._impulseGate patch) $
            put "flags: impulse-gate"
        forM_ (DriverC._inputControls patch) $ \(c, config) ->
            put $ "input: " <> pretty c <> ": " <> pretty config
        forM_ (Map.toList (DriverC._controls patch)) $ \(c, (_, config)) ->
            put $ "control: " <> showControl c <> ": " <> pretty config
    printEffect patch = do
        put $ EffectC._doc patch
        forM_ (Map.toList (EffectC._controls patch)) $ \(c, (_, doc)) ->
            put $ "control: " <> pretty c <> ": " <> doc

    put = Text.IO.putStrLn

usage :: String -> IO a
usage msg = do
    unless (null msg) $
        putStrLn $ "ERROR: " ++ msg
    putStr $ GetOpt.usageInfo
        (unlines
            [ "faust-im [print-patches | render-preview | notes outputDir]"
            ])
        options
    Exit.exitFailure

data Flag = Progress
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["progress"] (GetOpt.NoArg Progress) "emit json progress"
    ]

showControl :: DriverC.Control -> Text
showControl ("", c) = pretty c
showControl (elt, c) = elt <> ":" <> pretty c

dump :: Map Note.PatchName DriverC.Patch -> [Note.Note] -> IO ()
dump patches notes = do
    let (notFound, patchInstNotes) = lookupPatches patches notes
    unless (null notFound) $
        Log.warn $ "patches not found: " <> Text.unwords notFound
    forM_ (extractBreakpoints $ flatten patchInstNotes) $ \(inst, cbps) -> do
        Text.IO.putStrLn $ "=== " <> inst <> ":"
        -- TODO implement Texts.wrappedColumns to make it readable?  Or just
        -- use a wide window.
        -- mapM_ Text.IO.putStrLn $ Texts.columns 2 $
        --     map (map (fromMaybe "")) $ Seq.rotate2
        --         [ name : map showBp bps
        --         | (name, bps) <- cbps
        --         ]

        let maxlen = maximum $ 1 : map (Text.length . fst) cbps
        forM_ cbps $ \(control, bps) -> do
            Text.IO.putStrLn $
                Text.justifyLeft (maxlen+2) ' ' (control <> ":")
                <> Text.unwords (map showBp bps)
    where
    showBp (x, y) = "(" <> showNum x <> "," <> showNum y <> ")"
    showNum = Num.showFloat 2
    flatten patchInstNotes =
        [ (patch, inst, notes)
        | (patch, instNotes) <- patchInstNotes
        , (inst, notes) <- instNotes
        ]

extractBreakpoints
    :: [(DriverC.Patch, Note.InstrumentName, [Note.Note])]
    -> [(Note.InstrumentName, [(Text, [(Double, Double)])])]
extractBreakpoints patchInstNotes = filter (not . null . snd)
    [(inst, extract patch notes) | (patch, inst, notes) <- patchInstNotes]
    where
    extract patch notes =
        zip (map pretty inputNames) inputs
            ++ map (first showControl) (Map.toList controls)
        where
        inputs = Render.inputsBreakpoints patch notes
        controls = Render.controlsBreakpoints
            (Render._controlSize Render.defaultConfig) patch notes
        inputNames = map fst $ DriverC._inputControls patch

process :: Bool -> Map Note.PatchName DriverC.Patch -> [Note.Note] -> FilePath
    -> IO ()
process emitProgress patches notes outputDir = do
    Log.notice $ "processing " <> showt (length notes) <> " notes"
    let (notFound, patchInstNotes) = lookupPatches patches notes
    unless (null notFound) $
        Log.warn $ "patches not found: " <> Text.unwords notFound
    -- Signals.installHandler above will make SIGINT throw.
    let async :: Exception.AsyncException -> IO ()
        async exc = Log.error $ "exception: " <> showt exc
    let instruments = Set.fromList $ map fst $ concatMap snd patchInstNotes
    Checkpoint.clearUnusedInstruments outputDir instruments
    Exception.handle async $ Async.forConcurrently_ (flatten patchInstNotes) $
        \(patch, inst, notes) -> do
            -- Put the patch name after _.  Instruments never have '_', so this
            -- is safe.  tools/clear_faust will use this to clear obsolete
            -- checkpoints.
            let output = outputDir
                    </> untxt (inst <> "_" <> DriverC._name patch)
            Log.notice $ inst <> " notes: " <> showt (length notes) <> " -> "
                <> txt output
            Directory.createDirectoryIfMissing True output
            (result, elapsed) <- Thread.timeActionText $
                Render.write config output
                    (Set.fromList $ mapMaybe Note.trackId notes) patch notes
            case result of
                Left err -> Log.notice $ inst <> " writing " <> txt output
                    <> ": " <> err
                Right (rendered, total) ->
                    Log.notice $ inst <> " " <> showt rendered <> "/"
                        <> showt total <> " chunks: " <> txt output
                        <> " (" <> elapsed <> ")"
    where
    config = Render.defaultConfig
        { Render._emitProgress = emitProgress }
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
