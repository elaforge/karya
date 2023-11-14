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
import qualified Data.Typeable as Typeable

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Posix.Signals as Signals

import qualified Text.Read as Read

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Thread as Thread

import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch
import qualified Synth.Faust.EffectC as EffectC
import qualified Synth.Faust.InstrumentC as InstrumentC
import qualified Synth.Faust.Preview as Preview
import qualified Synth.Faust.Render as Render
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import           Global


main :: IO ()
main = do
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Lists.join ", " errs
    logFname <- Config.getLogFilename "faust.log"
    logHdl <- Log.rotate logFname
    Log.configure $ const $ Log.State
        { state_write_msg = Log.write_formatted logHdl
        , state_priority = Log.Notice
        }
    patches <- InstrumentC.getPatches
    thread <- Concurrent.myThreadId
    -- Make sure I get some output if the process is killed.
    Signals.installHandler Signals.sigTERM
        (Signals.CatchOnce (Concurrent.killThread thread)) Nothing
    imDir <- Config.imDir <$> Config.getConfig
    let calibrateDir = imDir </> "calibrate"
    case args of
        ["print-effects"] ->
            forM_ (Map.toList EffectC.patches) $ \(name, epatch) -> do
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
        ["print-patches", patch] -> printPatch =<< getPatch patch patches
        -- Similar to sampler-im calibrate, play a pitch at increasing dyn, to
        -- see if it's even.
        ["calibrate", patch, nn, dyns] -> do
            patch <- getPatch patch patches
            nn <- parse @Double nn
            dyns <- parse dyns
            Preview.renderSequence calibrateDir patch $ map snd $
                Preview.dynSequence True dur (Pitch.nn nn) dyns
            where dur = 1
        -- Like calibrate, but create individual samples.  I used this to
        -- create samples with a known dyn for sampler-im.
        ["calibrate-samples", patch, nn, dyns] -> do
            patch <- getPatch patch patches
            nn <- parse @Double nn
            dyns <- parse dyns
            Preview.renderSamples
                (calibrateDir </> untxt (InstrumentC._name patch)) patch $
                Preview.dynSequence False dur (Pitch.nn nn) dyns
            where dur = 1
        ["render-preview", patch] ->
            Preview.renderPreview =<< getPatch patch patches
        ["render-preview"] ->
            mapM_ Preview.renderPreview $ Either.rights (Map.elems patches)
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
        put $ InstrumentC._doc patch
        when (InstrumentC._impulseGate patch) $
            put "flags: impulse-gate"
        forM_ (InstrumentC._inputControls patch) $ \(c, config) ->
            put $ "input: " <> pretty c <> ": " <> pretty config
        forM_ (Map.toList (InstrumentC._controls patch)) $ \(c, (_, config)) ->
            put $ "control: " <> showControl c <> ": " <> pretty config
    printEffect patch = do
        put $ EffectC._doc patch
        forM_ (Map.toList (EffectC._controls patch)) $ \(c, (_, doc)) ->
            put $ "control: " <> pretty c <> ": " <> doc
    put = Text.IO.putStrLn

parse :: forall a. (Typeable.Typeable a, Read a) => String -> IO a
parse n = maybe (errorIO $ "expected " <> typ <> ": " <> txt n) pure
    (Read.readMaybe n)
    where
    typ = showt $ Typeable.typeRep (Proxy @a)

getPatch :: String -> Map Text (Either Text InstrumentC.Patch)
    -> IO InstrumentC.Patch
getPatch name patches = case Map.lookup (txt name) patches of
    Nothing -> errorIO $ "no such patch: " <> txt name
    Just (Left err) -> errorIO $ "patch: " <> txt name <> ": " <> err
    Just (Right patch) -> return patch

usage :: String -> IO a
usage msg = do
    unless (null msg) $
        putStrLn $ "ERROR: " ++ msg
    putStr $ GetOpt.usageInfo "faust-im [ flags ] <cmd>" options
    mapM_ putStrLn
        [ "  print-effects"
        , "  print_patches [ <patch-name> ]"
        , "  render-preview [ <patch-name> ]"
        , "  dump <filename>"
        , "  <notes-filename> <output-dir>"
        ]
    Exit.exitFailure

data Flag = Progress
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["progress"] (GetOpt.NoArg Progress) "emit json progress"
    ]

showControl :: InstrumentC.Control -> Text
showControl ("", c) = pretty c
showControl (elt, c) = elt <> ":" <> pretty c

dump :: Map Note.PatchName InstrumentC.Patch -> [Note.Note] -> IO ()
dump patches notes = do
    let (notFound, patchInstNotes) = lookupPatches patches notes
    unless (null notFound) $
        Log.warn $ "patches not found: " <> Text.unwords notFound
    forM_ (extractBreakpoints $ flatten patchInstNotes) $ \(inst, cbps) -> do
        Text.IO.putStrLn $ "=== " <> pretty inst <> ":"
        -- TODO implement Texts.wrappedColumns to make it readable?  Or just
        -- use a wide window.
        -- mapM_ Text.IO.putStrLn $ Texts.columns 2 $
        --     map (map (fromMaybe "")) $ Lists.rotate2
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
    :: [(InstrumentC.Patch, ScoreT.Instrument, [Note.Note])]
    -> [(ScoreT.Instrument, [(Text, [(Double, Double)])])]
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
        inputNames = map fst $ InstrumentC._inputControls patch

process :: Bool -> Map Note.PatchName InstrumentC.Patch -> [Note.Note]
    -> FilePath -> IO ()
process emitProgress patches notes outputDir = do
    Log.notice $ "processing " <> showt (length notes) <> " notes"
    let (notFound, patchInstNotes) = lookupPatches patches notes
    unless (null notFound) $
        Log.warn $ "patches not found: " <> Text.unwords notFound
    -- Signals.installHandler above will make SIGINT throw.
    let async :: Exception.AsyncException -> IO ()
        async exc = Log.error $ "exception: " <> showt exc
    Exception.handle async $ Async.forConcurrently_ (flatten patchInstNotes) $
        \(patch, inst, notes) -> do
            let output = outputDir </> Config.instrumentToDir inst
            Log.notice $ pretty inst <> " notes: " <> showt (length notes)
                <> " -> " <> txt output
            Directory.createDirectoryIfMissing True output
            -- Record the patch associated with this instrument.
            -- tools/clear_faust will use this to clear checkpoints whose patch
            -- has changed.  If the patch has changed but the instrument is the
            -- same, there will be multiple such files, but it's ok because
            -- clear_faust will just clear if any of them change.  Previously I
            -- encoded it into the directory name, but that prevents
            -- Config.clearUnusedInstruments from working when the instrument
            -- name remains the same but the patch name changed.
            touch $ output </> untxt (InstrumentC._name patch)
            (result, elapsed) <- Thread.timeActionText $
                Render.write config output
                    (Set.fromList $ mapMaybe Note.trackId notes) patch notes
            case result of
                Left err -> Log.notice $
                    pretty inst <> " writing " <> txt output <> ": " <> err
                Right (rendered, total) ->
                    Log.notice $ pretty inst <> " " <> showt rendered <> "/"
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

touch :: FilePath -> IO ()
touch fname = IO.withFile fname IO.WriteMode (const (return ()))

lookupPatches :: Map Note.PatchName patch -> [Note.Note]
    -> ([Note.PatchName], [(patch, [(ScoreT.Instrument, [Note.Note])])])
lookupPatches patches notes =
    Either.partitionEithers $
        map (\(patch, instNotes) -> (, instNotes) <$> find patch) $
        map (second (Lists.keyedGroupSort Note.instrument)) $
        Lists.keyedGroupSort Note.patch notes
    where
    find patch = maybe (Left patch) Right $ Map.lookup patch patches


{- NOTE [merge-sampler-faust]

    I decided against doing this and instead augment sampler-im with faust
    effects.  Here are the old notes:

    / First extend faust-im to be an Audio processor, so it can take inputs.
      . I wind up with separate generator and transformers.
      . A generator goes until env goes to 0 and the signal decays to -96dB
      . A transformer goes until the source signal runs out and the signal
        decays to -96dB.
      . Or they all go forever, and the consumer stops when the score runs out
        and -96dB.  But then I don't stop instruments that actually do stop,
        so let's not.
      . So in the transformer I need to keep the audio inputs separate, since
        the control inputs are zero padded out forever.
      . Or I can zero-pad only controls, and when I run out inputs flip
        a switch to watch for -96dB.
      . No that doesn't work, because it's one NAudio, so they all end
        together.  And I can't make controls end with the input signal.
        I need some explicit signal.
      . If the input is a sample, it has a definite end.  In fact, I could
        supply it ahead of time.
      . So transformer render gets an explicit end.
    . Integrate sampler-im's multiple voice ability with faust.
    . Patches now have to define a signal network, but it can probably be
      hardcoded to 'sampler -> faust' or just 'faust'.
    . I'd need some routing so I can get signals to those processors.
    . Later I'll want to integrate my own synth as well.  I guess as long as
      it produces an audio stream it's fine.
    . Does this mean everything goes into one binary?  I think so, unless
      I want to reinvent plugins.
    . In that case, do I still want to divide up notes files?  I think
      I should, because then I get better caching.  That also implies
      running multiple copies of im?  Actually there's no need, just have it
      read all the note files in the directory.
    . In that case, should I split up by instrument?  I think I already have
      to do the work when I split im from non-im events.
    Actually, merge faust and sampler before doing audio transformer.
      . How does multiple vs. single voice stuff work?
      . Multiple means it allocates a new instrument for each note, which is
        how the sampler does it.  Mono mode for sampler wouldn't make so much
        sense because each note can be a different sample, at which point the
        resample state no longer applies.
      . For faust, I could just build it into instruments.  Do all-mono for
        now.
      . Should I serialize to different files, or split them up in the
        sampler?  How about split by patch?
      . I guess there isn't any big reason to split into separate files, but
        I have to split anyway to separate im from non-im, and since there
        will be only one synth, I might as well split by instrument.  Then the
        synth doesn't have to split, it just evaluates everything in the
        directory in parallel.  I have to delete the existing contents
        though... at least with a single file I can replace it atomically.
        But I already have this problem with synth-per-file, I just haven't
        noticed it because I don't really use faust.  And deleting seems like
        not a huge problem.
    . Another reason is to unify Render.write.
    . Another is to have a per-note allocation mode for faust.
    Minimal necessary to merge:
      . One binary with one PatchDb has to have both kinds of patches.
        Ultimately I want to merge to one patch, and faust and the sampler
        become signal generators.
      . They should both use the same Render.write loop.
    Differences:
      . Faust has only one audio generator per instrument, and voices are
        implemented as Note.element.  The audio generator runs constantly, so
        all notes are merged into a set of signals.  There is only one state.
      . Meanwhile, the sampler has a separate audio generator for each note.
        Each one has its own state and independent set of controls.
-}
