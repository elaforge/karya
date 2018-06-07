-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Faust.Checkpoint_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector.Storable as Vector

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import Util.Test
import qualified Util.Test.Testing as Testing

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Faust.Checkpoint as Checkpoint
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Faust.Render as Render
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Types


test_write_incremental = do
    dir <- Testing.tmp_dir "write"
    putStrLn $ "** tmp dir: " ++ dir
    patch <- getPatch
    let write = Checkpoint.writeConfig config dir patch
    -- no notes produces no output
    io_equal (write []) Nothing
    io_equal (Directory.listDirectory (dir </> Checkpoint.cacheDir)) []

    let dur = AUtil.toSeconds (Render._chunkSize config)
    let oldNotes = mkNotes (dur/2) [NN.c4, NN.d4, NN.e4, NN.f4, NN.g4]
    let newNotes = mkNotes (dur/2) [NN.c4, NN.d4, NN.e4, NN.f4, NN.c4]

    -- Should be be 2.5 checkpoints.
    io_equal (write oldNotes) Nothing
    wavs <- listWavs dir
    equal_on length wavs 3
    io_equal (length <$> listWavs (dir </> Checkpoint.cacheDir)) 3
    states <- filter (".state." `List.isInfixOf`) <$>
        Directory.listDirectory (dir </> Checkpoint.cacheDir)
    -- All of them have states, since they are at the end of each chunk.
    io_equal (mapM (Directory.getFileSize
            . ((dir </> Checkpoint.cacheDir) </>)) states)
        [40, 40, 40]

    -- change only last note: only 3rd sample should rerender, but contents
    -- should be the same.
    io_equal (write newNotes) Nothing

    -- Test skipCheckpoints directly.
    let hashes = Checkpoint.noteHashes (Render._chunkSize config) newNotes
    (skippedHashes, state) <- Checkpoint.skipCheckpoints dir hashes
    equal_on (fst . head) (take 4 skippedHashes) 2
    equal ((/= DriverC.State mempty) <$> state) (Just True)

    -- Only 1 was rerendered, so now there are 4.
    wavs <- listWavs (dir </> Checkpoint.cacheDir)
    equal_on length wavs 4

    incremental <- readSamples dir
    nonIncremental <- renderSamples patch newNotes
    equal incremental nonIncremental

    -- Switched back to the old ones, nothing new should render.
    io_equal (write oldNotes) Nothing

    let hashes = Checkpoint.noteHashes (Render._chunkSize config) oldNotes
    (skippedHashes, state) <- Checkpoint.skipCheckpoints dir hashes
    equal skippedHashes []
    equal state Nothing
    -- Should have rendered 0 more files, since Render.renderPatch should exit
    -- immediately, due to start >= end.
    wavs <- listWavs (dir </> Checkpoint.cacheDir)
    equal_on length wavs 4

-- TODO test volume and dyn

renderSamples :: DriverC.Patch -> [Note.Note] -> IO [Float]
renderSamples patch notes = do
    dir <- Testing.tmp_dir "renderSamples"
    io_equal (Checkpoint.writeConfig config dir patch notes) Nothing
    readSamples dir

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat . map (dir</>) =<< listWavs dir

-- equal_on f got expected = equal (f got)

listWavs :: FilePath -> IO [FilePath]
listWavs = fmap (List.sort . filter (".wav" `List.isSuffixOf`))
    . Directory.listDirectory

config :: Render.Config
config = Render.Config
    { _chunkSize = 8
    , _maxDecay = 0
    }

getPatch :: IO DriverC.Patch
getPatch = do
    patches <- DriverC.getPatches
    return $ fromMaybe (error "no sine patch") $ Map.lookup "sine" patches

mkNotes :: RealTime -> [Pitch.NoteNumber] -> [Note.Note]
mkNotes dur nns =
    [ mkNote start dur nn
    | (start, nn) <- zip (iterate (+dur) 0) nns
    ]

mkNote :: RealTime -> RealTime -> Pitch.NoteNumber -> Note.Note
mkNote start dur nn = Note.withControl Control.volume (Signal.constant 1) $
    Note.withControl Control.dynamic (Signal.constant 1) $
    Note.withPitch nn $ Note.note "sine" "sine" start dur

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap Vector.toList) . Resource.runResourceT
    . Audio.toSamples
