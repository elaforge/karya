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
import Types


test_write_noop = do
    dir <- Testing.tmp_dir "write"
    -- no notes produces no output
    io_equal (write dir []) (Right (0, 0))
    io_equal (Directory.listDirectory (dir </> Checkpoint.cacheDir)) []

    -- One failed note produces no output.
    io_equal (write dir [mkNote "no-such-patch" 0 dur NN.c4]) (Right (0, 0))
    io_equal (listWavs dir) []

test_write_simple = do
    dir <- Testing.tmp_dir "write"
    samples <- writeDb dir
    -- A single note that spans two checkpoints.  Dur is determined by the
    -- sample length.
    io_equal (write dir [mkNote "patch" 0 0 NN.c4]) (Right (2, 2))
    io_equal (length <$> listWavs dir) 2
    io_equal (readSamples dir) samples

test_write_simple_offset = do
    dir <- Testing.tmp_dir "write"
    samples <- writeDb dir
    io_equal (write dir [mkNote "patch" dur 0 NN.c4]) (Right (3, 3))
    io_equal (readSamples dir) (replicate 8 0 ++ samples)

test_write_freq = do
    dir <- Testing.tmp_dir "write"
    writeDb dir
    -- Freq*2 is half as many samples.
    io_equal (write dir [mkNote "patch" 0 0 NN.c5]) (Right (1, 1))
    io_equal (length <$> listWavs dir) 1
    io_equal (readSamples dir) [0, 1, 3, 3, 1, 1, 3, 3]
    -- [0, 1, 2, 3, 4, 3, 2, 1, 0, 1, 2, 3, 4, 3, 2, 1]

test_write_incremental = do
    dir <- Testing.tmp_dir "write"
    writeDb dir
    -- 0   8   16  24  32
    -- c4----->
    --     c4----->
    let oldNotes = [mkNote "patch" 0 0 NN.c4, mkNote "patch" dur 0 NN.c4]
    -- The duration of 1 should have no effect, but change the hash.
    let newNotes = [mkNote "patch" 0 0 NN.c4, mkNote "patch" dur (dur/2) NN.c4]
    io_equal (write dir oldNotes) (Right (3, 3))
    -- io_equal (write dir newNotes) (Right (2, 3))

    -- TODO Resume after changing a later note, results same as rerender from
    -- scratch.

test_overlappingNotes = do
    let f = (\(a, b, c) -> (map extract a, map extract b, map extract c))
            . Render.overlappingNotes 0 1
            . map (\(s, d) -> mkNote "patch" s d NN.c4)
        extract n = (Note.start n, Note.duration n)
    equal (f []) ([], [], [])
    equal (f [(0, 0)]) ([], [(0, 0)], [])
    equal (f [(-1, 1), (-0.5, 1), (0, 1), (1, 1)])
        ([(-0.5, 1)], [(0, 1)], [(1, 1)])


write :: FilePath -> [Note.Note] -> IO (Either Text (Int, Int))
write dir = Render.write_ (mkDb dir) chunkSize Resample.Linear dir

patchDir :: FilePath
patchDir = "patch"

chunkSize :: Audio.Frame
chunkSize = 8

dur :: RealTime
dur = AUtil.toSeconds chunkSize

mkDb :: FilePath -> Patch.Db
mkDb dir = Patch.Db
    { _patches = Map.fromList
        [ ("patch", Patch.patch "."
            [("tri.wav", Patch.pitchedSample NN.c4)])
        ]
    , _rootDir = dir </> patchDir
    }

writeDb :: FilePath -> IO [Audio.Sample]
writeDb dir = do
    let patch = dir </> patchDir
    Directory.createDirectoryIfMissing True patch
    Resource.runResourceT $
        File.write AUtil.outputFormat (patch </> "tri.wav") audio
    toSamples audio
    where
    audio :: AUtil.Audio
    audio = Audio.expandChannels $ Audio.fromSampleLists
        [take (fromIntegral chunkSize * 2) (cycle triangle)]

triangle :: [Audio.Sample]
triangle = [0, 1, 2, 3, 4, 3, 2, 1]


-- * TODO copy paste with Faust.Render_test

mkNote :: Note.PatchName -> RealTime -> RealTime -> Pitch.NoteNumber
    -> Note.Note
mkNote patch start dur nn = Note.setHash $
    Note.withControl Control.volume (Signal.constant 1) $
    Note.withControl Control.dynamic (Signal.constant 1) $
    Note.withPitch nn $ Note.note patch "inst" start dur

listWavs :: FilePath -> IO [FilePath]
listWavs = fmap (List.sort . filter (".wav" `List.isSuffixOf`))
    . Directory.listDirectory

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat . map (dir</>) =<< listWavs dir

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap Vector.toList) . Resource.runResourceT
    . Audio.toSamples . Audio.extractChannel 0
