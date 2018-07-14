-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Render_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector.Storable as Vector

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample
import Util.Test
import qualified Util.Test.Testing as Testing

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Render as Render
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global


test_write_noop = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir dir
    -- no notes produces no output
    io_equal (write []) (Right (0, 0))
    io_equal (Directory.listDirectory (dir </> Checkpoint.cacheDir)) []

    -- One failed note produces no output.
    io_equal (write [mkNote "no-such-patch" 0 16 NN.c4]) (Right (0, 0))
    io_equal (listWavs dir) []

test_write_simple = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir dir
    writeDb dir
    -- A single note that spans two checkpoints.  Dur is determined by the
    -- sample length.
    io_equal (write [mkNote "patch" 0 8 NN.c4]) (Right (2, 2))
    io_equal (length <$> listWavs dir) 2
    io_equal (readSamples dir) triangle

test_write_simple_offset = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir dir
    writeDb dir
    io_equal (write [mkNote "patch" 8 8 NN.c4]) (Right (4, 4))
    io_equal (readSamples dir) (replicate 8 0 ++ triangle)

test_write_freq = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir dir
    writeDb dir
    -- Freq*2 is half as many samples.
    io_equal (write [mkNote "patch" 0 8 NN.c5]) (Right (1, 1))
    io_equal (length <$> listWavs dir) 1
    io_equal (readSamples dir) [1, 2, 4, 2]

test_write_incremental = do
    -- Resume after changing a later note, results same as rerender from
    -- scratch.
    dir <- Testing.tmp_dir "write"
    let write = write_ dir dir
    writeDb dir
    -- 0   4   8   12   16
    -- 12343210
    --     12343210
    let oldNotes =
            [ mkNote "patch" 0 8 NN.c4
            , mkNote "patch" 4 8 NN.c4
            ]
    -- 0   4   8   12   16
    -- 12343210
    --       12343210
    -- ====3210
    --     0012
    let newNotes =
            [ mkNote "patch" 0 8 NN.c4
            , mkNote "patch" 6 8 NN.c4
            ]
    io_equal (write oldNotes) (Right (3, 3))
    io_equal (readSamples dir)
        (zipWith (+) (triangle ++ repeat 0) (replicate 4 0 ++ triangle))
    -- resume 0 note at -4
    io_equal (write newNotes) (Right (3, 4))
    let expected =
            zipWith (+) (triangle ++ repeat 0) (replicate 6 0 ++ triangle)
            ++ [0, 0] -- pad to 16
    incremental <- readSamples dir
    equal incremental expected
    nonIncremental <- renderSamples dir newNotes
    equal nonIncremental expected

renderSamples :: FilePath -> [Note.Note] -> IO [Float]
renderSamples dbDir notes = do
    dir <- Testing.tmp_dir "renderSamples"
    write_ dbDir dir notes
    readSamples dir

test_overlappingNotes = do
    let f = (\(a, b, c) -> (map extract a, map extract b, map extract c))
            . Render.overlappingNotes 0 1
            . map (\(s, d) -> mkNote "patch" s d NN.c4)
        extract n =
            ( AUtil.toFrame (Note.start n)
            , AUtil.toFrame (Note.duration n)
            )
    equal (f []) ([], [], [])
    equal (f [(0, 0)]) ([], [(0, 0)], [])
    equal (f [(-4, 4), (-2, 4), (0, 4), (4, 4)])
        ([(-2, 4)], [(0, 4)], [(4, 4)])


write_ :: FilePath -> FilePath -> [Note.Note] -> IO (Either Text (Int, Int))
write_ dbDir outDir =
    Render.write_ (mkDb dbDir) chunkSize Resample.Linear outDir

patchDir :: FilePath
patchDir = "patch"

chunkSize :: Audio.Frame
chunkSize = 4

mkDb :: FilePath -> Patch.Db
mkDb dir = Patch.Db
    { _patches = Map.fromList
        [ ("patch", Patch.patch "."
            [("tri.wav", Patch.pitchedSample NN.c4)])
        ]
    , _rootDir = dir </> patchDir
    }

writeDb :: FilePath -> IO ()
writeDb dir = do
    let patch = dir </> patchDir
    Directory.createDirectoryIfMissing True patch
    Resource.runResourceT $
        File.write AUtil.outputFormat (patch </> "tri.wav") audio
    where
    audio :: AUtil.Audio
    audio = Audio.expandChannels $ Audio.fromSampleLists [triangle]

triangle :: [Audio.Sample]
triangle = [1, 2, 3, 4, 3, 2, 1, 0]


-- * TODO copy paste with Faust.Render_test

mkNote :: Note.PatchName -> Audio.Frame -> Audio.Frame -> Pitch.NoteNumber
    -> Note.Note
mkNote patch start dur nn = Note.setHash $
    Note.withControl Control.volume (Signal.constant 1) $
    Note.withControl Control.dynamic (Signal.constant 1) $
    Note.withPitch nn $
    Note.note patch "inst" (AUtil.toSeconds start) (AUtil.toSeconds dur)

listWavs :: FilePath -> IO [FilePath]
listWavs = fmap (List.sort . filter (".wav" `List.isSuffixOf`))
    . Directory.listDirectory

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat . map (dir</>) =<< listWavs dir

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap Vector.toList) . Resource.runResourceT
    . Audio.toSamples . Audio.extractChannel 0
