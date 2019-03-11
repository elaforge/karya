-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Render_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Vector.Storable as Vector
import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample
import qualified Util.Test.Testing as Testing

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Render as Render
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types
import           Util.Test


test_write_noop = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir
    -- no notes produces no output
    io_equal (write []) (Right (0, 0))
    io_equal (Directory.listDirectory (dir </> Checkpoint.checkpointDir)) []
    -- Failed note produces aborts the render.
    result <- write [mkNote "" 0 16 NN.c4]
    left_like result "no patch"
    io_equal (listWavs dir) []

test_write_simple = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir
    writeDb dir
    -- A single note that spans two checkpoints.  Dur is determined by the
    -- sample length.
    io_equal (write [mkNote dir 0 8 NN.c4]) (Right (2, 2))
    io_equal (length <$> listWavs dir) 2
    io_equal (readSamples dir) triangle

test_write_simple_offset = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir
    writeDb dir
    io_equal (write [mkNote dir 8 8 NN.c4]) (Right (4, 4))
    io_equal (readSamples dir) (replicate 8 0 ++ triangle)

test_write_freq = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir
    writeDb dir
    -- Freq*2 is half as many samples.
    io_equal (write [mkNote dir 0 8 NN.c5]) (Right (1, 1))
    io_equal (length <$> listWavs dir) 1
    io_equal (readSamples dir) [1, 2, 4, 2]

test_write_incremental = do
    -- Resume after changing a later note, results same as rerender from
    -- scratch.
    dir <- Testing.tmp_dir "write"
    let write = write_ dir
    writeDb dir
    -- 0   4   8   12   16
    -- 12343210
    --     12343210
    let oldNotes =
            [ mkNote dir 0 8 NN.c4
            , mkNote dir 4 8 NN.c4
            ]
    -- 0   4   8   12   16
    -- 12343210
    --       12343210
    -- ====3210
    --     0012
    let newNotes =
            [ mkNote dir 0 8 NN.c4
            , mkNote dir 6 8 NN.c4
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
    nonIncremental <- renderSamples newNotes
    equal nonIncremental expected

renderSamples :: [Sample.Note] -> IO [Float]
renderSamples notes = do
    dir <- Testing.tmp_dir "renderSamples"
    write_ dir notes
    readSamples dir

test_overlappingNotes = do
    let f start size = extract
            . Render.overlappingNotes (AUtil.toFrame start) (AUtil.toFrame size)
            . map (\(s, d) -> mkNoteS "" s d NN.c4)
        extract (a, b, c) = (map e a, map e b, map e c)
            where
            e n =
                ( AUtil.toSeconds (Sample.start n)
                , AUtil.toSeconds (fromMaybe 0 (Sample.duration n))
                )
    equal (f 0 1 []) ([], [], [])
    equal (f 0 1 [(0, 0)]) ([], [(0, 0)], [])
    equal (f 0 1 [(-4, 4), (-2, 4), (0, 4), (4, 4)])
        ([(-2, 4)], [(0, 4)], [(4, 4)])
    equal (f 4 4 [(0, 5), (1, 1), (3, 2)])
        ([(0, 5), (3, 2)], [], [])


write_ :: FilePath -> [Sample.Note] -> IO (Either Text (Int, Int))
write_ outDir = Render.write_ chunkSize chunkSize Resample.Linear outDir mempty

patchDir :: FilePath
patchDir = "patch"

chunkSize :: Audio.Frame
chunkSize = 4

mkDb :: FilePath -> Patch.Db
mkDb dir = Patch.db (dir </> patchDir)
    [Patch.DbPatch $ Patch.simple "test" "tri.wav" NN.c4]

writeDb :: FilePath -> IO ()
writeDb dbDir = do
    let patch = dbDir </> patchDir
    Directory.createDirectoryIfMissing True patch
    Resource.runResourceT $
        File.write AUtil.outputFormat (triFilename dbDir) audio
    where
    audio :: AUtil.Audio
    audio = Audio.expandChannels $ Audio.fromSampleLists [triangle]

triFilename :: FilePath -> FilePath
triFilename dbDir = dbDir </> patchDir </> "tri.wav"

triangle :: [Audio.Sample]
triangle = [1, 2, 3, 4, 3, 2, 1, 0]

mkNoteS :: FilePath -> RealTime -> RealTime -> Pitch.NoteNumber -> Sample.Note
mkNoteS dbDir start dur = mkNote dbDir (AUtil.toFrame start) (AUtil.toFrame dur)

mkNote :: FilePath -> Audio.Frame -> Audio.Frame -> Pitch.NoteNumber
    -> Sample.Note
mkNote dbDir start dur nn = Sample.Note
    { start = start
    , duration = Just dur
    , hash = Note.hash $ Note.withPitch nn $
        Note.note "patch" "inst" (AUtil.toSeconds start) (AUtil.toSeconds dur)
    , sample = if null dbDir
        then Left "no patch"
        else Right $ (Sample.make (triFilename dbDir))
            { Sample.ratio = Signal.constant $ Sample.pitchToRatio NN.c4 nn }
    }

-- * TODO copy paste with Faust.Render_test

listWavs :: FilePath -> IO [FilePath]
listWavs = fmap (List.sort . filter (".wav" `List.isSuffixOf`))
    . Directory.listDirectory

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat . map (dir</>) =<< listWavs dir

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap Vector.toList) . Resource.runResourceT
    . Audio.toSamples . Audio.extractChannel 0
