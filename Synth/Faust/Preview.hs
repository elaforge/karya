-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | See 'render'.
module Synth.Faust.Preview where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.Audio.File as Audio.File
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Faust.InstrumentC as InstrumentC
import qualified Synth.Faust.Render as Render
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global


-- | The patch preview cache is in imDir </> cacheDir </> patchName </> nn.wav
cacheDir :: FilePath -> Text -> FilePath
cacheDir imDir patchName = imDir </> "preview" </> untxt patchName

{- | Render representative samples for this instrument so they can be played in
    realtime by the thru mechanism.  This will stop if the output directory
    already exists, so it's up to the shakefile to remove the directories if
    the underlying .dsp file has changed.

    In theory this should divide the patch up along its important axes
    (pitch, dynamic, attributes) and render the whole matrix, but for now I
    only have pitch.
-}
render :: InstrumentC.Patch -> IO ()
render patch = do
    imDir <- Config.imDir <$> Config.getConfig
    let out = cacheDir imDir (InstrumentC._name patch)
    unlessM (Directory.doesDirectoryExist out) $ do
        Directory.createDirectoryIfMissing True out
        let element = fromMaybe "" $ Lists.head $ filter (/="") $ map fst $
                Map.keys $ InstrumentC._controls patch
        Thread.forCpu_ (standardNotes element) $ \(nn, note) -> do
            Log.with_stdio_lock $
                Text.IO.putStrLn $ InstrumentC._name patch <> ": " <> pretty nn
            renderNote (out </> noteFilename nn) patch note

noteFilename :: Pitch.NoteNumber -> FilePath
noteFilename nn = prettys nn <> ".wav"

renderNote :: FilePath -> InstrumentC.Patch -> Note.Note -> IO ()
renderNote fname patch note = Resource.runResourceT $
    Audio.File.write AUtil.outputFormat fname $
    Render.renderPatch emitMessage patch Render.defaultConfig Nothing
        notifyState [note] 0
    where
    emitMessage (Config.Warn _ msg) = Text.IO.putStrLn msg
    emitMessage (Config.Failure msg) = Text.IO.putStrLn msg
    emitMessage _ = return ()
    notifyState = const $ return ()

pitchToSample :: FilePath -> Text -> Map Pitch.NoteNumber FilePath
pitchToSample imDir patchName =
    Map.fromList [(nn, dir </> noteFilename nn) | nn <- nns]
    where
    dir = cacheDir imDir patchName
    nns = map fst $ standardNotes ""

-- | Render previews of these notes.
standardNotes :: Note.Element -> [(Pitch.NoteNumber, Note.Note)]
standardNotes element = zip nns (map (makeNote element) nns)
    where nns = Lists.range NN.c1 NN.c8 2

makeNote :: Note.Element -> Pitch.NoteNumber -> Note.Note
makeNote element nn =
    Note.withControl Control.dynamic (Signal.constant 0.75) $
    (Note.withPitch nn $ Note.testNote 0 1) { Note.element = element }
