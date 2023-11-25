-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | See 'render'.
module Synth.Faust.Preview (
    renderPreview
    , pitchSequence
    , pitchToSample
    , dynSequence
    -- * render
    , renderSamples
    , renderSequence
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Audio.File as Audio.File
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Faust.InstrumentC as InstrumentC
import qualified Synth.Faust.Render as Render
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


-- | The patch preview cache is in imDir </> cacheDir </> patchName </> nn.wav
previewDir :: FilePath -> Text -> FilePath
previewDir imDir patchName = imDir </> "preview" </> untxt patchName

{- | Render representative samples for this instrument so they can be played in
    realtime by the thru mechanism.  This will stop if the output directory
    already exists, so it's up to the shakefile to remove the directories if
    the underlying .dsp file has changed.

    In theory this should divide the patch up along its important axes
    (pitch, dynamic, attributes) and render the whole matrix, but for now I
    only have pitch.

    This skips if the directory exists, so I can call it on every rebuild.
    @tools/clear_faust@ is hooked into the build to clear preview directories
    when a patch changes.
-}
renderPreview :: InstrumentC.Patch -> IO ()
renderPreview patch = do
    imDir <- Config.imDir <$> Config.getConfig
    let outDir = previewDir imDir (InstrumentC._name patch)
    unlessM (Directory.doesDirectoryExist outDir) $
        renderSamples outDir patch (map (first nnFilename) notes)
    where
    notes = pitchSequence 1 element
    element = fromMaybe "" $ Lists.head $ filter (/="") $ map fst $
        Map.keys $ InstrumentC._controls patch

-- | Render previews of these notes.
pitchSequence :: RealTime -> Note.Element -> [(Pitch.NoteNumber, Note.Note)]
pitchSequence dur element =
    zip nns (map (makeNote 0 dur element 0.75) nns)
    where nns = Lists.range NN.c1 NN.c8 2

-- | Used by the thru mechanism to find the preview samples.
pitchToSample :: FilePath -> Text -> Map Pitch.NoteNumber FilePath
pitchToSample imDir patchName =
    Map.fromList [(nn, dir </> nnFilename nn) | nn <- nns]
    where
    dir = previewDir imDir patchName
    nns = map fst $ pitchSequence 1 ""

nnFilename :: Pitch.NoteNumber -> FilePath
nnFilename nn = prettys nn <> ".wav"

dynSequence :: Bool -> RealTime -> Pitch.NoteNumber -> Int
    -> [(FilePath, Note.Note)]
dynSequence sequence dur nn dynamics =
    [ ( prettys nn <> "-" <> showDyn dyn <> ".wav"
      , makeNote (if sequence then start else 0) dur "" dyn nn
      )
    | (start, dyn) <- zip (Lists.range_ 0 dur) dyns
    ]
    where
    dyns = evenDyns dynamics
    showDyn = untxt . Num.zeroPad 3 . round . (*100)

evenDyns :: Int -> [Double]
evenDyns n = take n (Lists.range_ step step)
    where step = 1 / fromIntegral n

-- * render

renderSamples :: FilePath -> InstrumentC.Patch -> [(FilePath, Note.Note)]
    -> IO ()
renderSamples outDir patch fnameNotes = do
    let fnames = map (txt . FilePath.takeFileName . fst) fnameNotes
    mapM_ Text.IO.putStrLn $ Texts.columns 2 $
        zipWith (:) ("fname" : fnames) $
        showNotes $ map snd fnameNotes
    Directory.createDirectoryIfMissing True outDir
    Thread.forCpu_ fnameNotes $ \(fname, note) ->
        renderNotes (outDir </> fname) patch [note]

renderSequence :: FilePath -> InstrumentC.Patch -> [Note.Note] -> IO ()
renderSequence outDir patch notes = do
    mapM_ Text.IO.putStrLn $ Texts.columns 2 $ showNotes notes
    Directory.createDirectoryIfMissing True outDir
    renderNotes
        (outDir </> "calibrate-" <> untxt (InstrumentC._name patch) <> ".wav")
        patch notes

showNotes :: [Note.Note] -> [[Text]]
showNotes notes = ["n", "time", "dyn"] : zipWith fmt [0..] notes
    where
    fmt (i :: Int) n =
        [ Num.zeroPad 2 i
        , RealTime.show_units (Note.start n)
        , pretty $ Note.initial Control.dynamic n
        ]

renderNotes :: FilePath -> InstrumentC.Patch -> [Note.Note] -> IO ()
renderNotes fname patch notes = do
    Log.with_stdio_lock $
        Text.IO.putStrLn $ InstrumentC._name patch <> ": " <> txt fname
    Resource.runResourceT $
        Audio.File.write AUtil.outputFormat fname $
        Render.renderPatch emitMessage patch Render.defaultConfig Nothing
            notifyState notes 0
    where
    emitMessage (Config.Warn _ msg) = Text.IO.putStrLn msg
    emitMessage (Config.Failure msg) = Text.IO.putStrLn msg
    emitMessage _ = return ()
    notifyState = const $ return ()

makeNote :: RealTime -> RealTime -> Note.Element -> Double -> Pitch.NoteNumber
    -> Note.Note
makeNote start dur element dyn nn =
    Note.withControl Control.dynamic (Signal.constant dyn) $
    (Note.withPitch nn $ Note.testNote start dur) { Note.element = element }
