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
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Render as Render
import qualified Synth.Sampler.RenderSample as RenderSample
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
    result <- write [mkNote1 "" 0]
    left_like result "No such file"
    io_equal (listWavs dir) []

test_write_simple = do
    (write, dir) <- tmpDb
    -- A single note that spans two checkpoints.  Dur is determined by the
    -- sample length.
    io_equal (write [mkNote1 dir 0]) (Right (2, 2))
    io_equal (length <$> listWavs dir) 2
    io_equal (readSamples dir) triangle

test_write_simple_offset = do
    (write, dir) <- tmpDb
    io_equal (write [mkNote1 dir 8]) (Right (4, 4))
    io_equal (readSamples dir) (replicate 8 0 ++ triangle)

test_write_freq = do
    (write, dir) <- tmpDb
    -- Freq*2 is half as many samples.
    io_equal (write [mkNote dir 0 0.5]) (Right (1, 1))
    io_equal (length <$> listWavs dir) 1
    io_equal (readSamples dir) [1, 2, 4, 2]

-- Can I do it without the whole directory and file?
-- I'd have to replace File.readFrom in RenderSample, and with something that
-- uses a Vector.  That's just Audio.drop . Audio.fromSamples
-- But I'd also need to replace File.writeCheckpoints, which means threading
-- through Checkpoint too.  And I'd still want to keep the file-oriented tests
-- around, to exercise those functions more, so... let's keep the tmp files.

test_write_ratios = do
    (write, dir) <- tmpDb

    let notes = [mkNoteRatios dir 0 [(0, 2), (4, 2), (4, 4), (8, 4), (8, 2)]]
    -- libsamplerate writes an extra sample at the start.  I don't really know
    -- why, but it's "documented" as "Calculate samples before first sample in
    -- input array."
    let expected = [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 3, 3, 2, 2, 1, 1, 0, 0]
    io_equal (write notes) (Right (5, 5))
    io_equal (readSamples dir) expected
    -- No work, all chunks reused.
    io_equal (write notes) (Right (0, 5))
    -- rmPrefixes (dir </> "checkpoint") ["002", "003", "004"]
    -- io_equal (write notes) (Right (3, 5))
    -- io_equal (readSamples dir) expected

    -- frame   0   .   4   .   8   .  12   .  16   .  20
    -- input   1 2 3 4 3 2 1 0
    -- chunk   0       1       2       3       4      5
    -- ratio   2-------4-------2------------------------
    -- output  1 1 1 1 2 2 2 2 3 3 4 4 3 3 2 2 1 1 0 0

{-
    . 0 - rat 2: (2, 4) 1 2 -> 1 1 1 1      file: 0
    . 1 - rat 4: (1, 4) 3   -> 2 2 2 2      file: 2
    . 2 - rat 2: (2, 4) 4 3 -> 3 3 4 4      file: 3
    . 3 - rat 2: (2, 4) 2 1 -> 3 3 2 2
    . 4 - rat 2: (1, 2) 0   -> 1 1 0

    Now resume at 3f: rat=4, but I index ratios by output, so should look
    for segment at 4+4=8, which is 2.  Which I do.
    . Fixed now.  Resume at rat 2->2:
    . 2 - rat 2: (2, 4) 4 3 -> 3 3 4 4
    . 3 - rat 2: (2, 4) 2 1 -> 3 3 2 2
    . 4 - rat 2: (1, 3) 0   -> 1 1 0
-}


test_write_incremental = do
    -- Resume after changing a later note, results same as rerender from
    -- scratch.
    (write, dir) <- tmpDb
    -- 0   4   8   12   16
    -- 12343210
    --     12343210
    let oldNotes =
            [ mkNote1 dir 0
            , mkNote1 dir 4
            ]
    -- 0   4   8   12   16
    -- 12343210
    --       12343210
    -- ====3210
    --     0012
    let newNotes =
            [ mkNote1 dir 0
            , mkNote1 dir 6
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

test_resume_at_boundary = do
    (write, dir) <- tmpDb
    let note = mkNoteDur dir
    io_equal (write [note 0 4, note 4 4]) (Right (2, 2))
    io_equal (readSamples dir) [1, 2, 3, 4, 1, 2, 3, 4]
    io_equal (write [note 0 4, note 4 2]) (Right (1, 2))
    io_equal (readSamples dir) [1, 2, 3, 4, 1, 2, 0, 0]

renderSamples :: [Sample.Note] -> IO [Float]
renderSamples notes = do
    dir <- Testing.tmp_dir "renderSamples"
    write_ dir notes
    readSamples dir

test_overlappingNotes = do
    let f start size = extract
            . Render.overlappingNotes (AUtil.toFrame start) (AUtil.toFrame size)
            . map noteS
        noteS (start, dur) =
            mkNoteDur "" (AUtil.toFrame start) (AUtil.toFrame dur)
        extract (a, b, c) = (map e a, map e b, map e c)
            where
            e n =
                ( AUtil.toSeconds (Sample.start n)
                , AUtil.toSeconds (Sample.duration n)
                )
    equal (f 0 1 []) ([], [], [])
    equal (f 0 1 [(0, 0)]) ([], [(0, 0)], [])
    equal (f 0 1 [(-4, 4), (-2, 4), (0, 4), (4, 4)])
        ([(-2, 4)], [(0, 4)], [(4, 4)])
    equal (f 4 4 [(0, 5), (1, 1), (3, 2)])
        ([(0, 5), (3, 2)], [], [])

-- * implementation

tmpDb :: IO ([Sample.Note] -> IO (Either Text (Int, Int)), FilePath)
tmpDb = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir
    writeDb dir
    return (write, dir)

rmPrefixes :: FilePath -> [String] -> IO ()
rmPrefixes dir prefixes =
    mapM_ (Directory.removeFile . (dir</>))
        . filter (\fn -> any (`List.isPrefixOf` fn) prefixes)
        =<< Directory.listDirectory dir

write_ :: FilePath -> [Sample.Note] -> IO (Either Text (Int, Int))
write_ outDir = Render.writeConfig config outDir mempty
    where
    config = Render.Config
        { _quality = Resample.ZeroOrderHold
        , _chunkSize = chunkSize
        , _blockSize = chunkSize
        }

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

mkNoteSec :: FilePath -> RealTime -> Double -> Sample.Note
mkNoteSec dbDir start = mkNote dbDir (AUtil.toFrame start)

-- | mkNote where ratio=1.
mkNote1 :: FilePath -> Audio.Frame -> Sample.Note
mkNote1 dbDir start = mkNote dbDir start 1

-- | Make a note with a constant ratio.
mkNote :: FilePath -> Audio.Frame -> Double -> Sample.Note
mkNote dbDir start ratio = mkNoteRatios dbDir start [(start, ratio)]

mkNoteRatios :: FilePath -> Audio.Frame -> [(Audio.Frame, Double)]
    -> Sample.Note
mkNoteRatios dbDir start ratios_ = Sample.Note
    { start = start
    , duration =
        RenderSample.predictDuration (Signal.shift (- sec start) ratios) dur
    , hash = Note.hash note
    , sample = (Sample.make (triFilename dbDir)) { Sample.ratios = ratios }
    }
    where
    dur = Audio.Frame $ length triangle
    sec = AUtil.toSeconds
    -- I don't put the pitch on, but it just affects hash, which is unlikely to
    -- be relevant in a test.
    note = Note.note "patch" "inst" (sec start) (sec dur)
    ratios = Signal.from_pairs $ map (first sec) ratios_

-- | Like 'mkNoteRatios', except set a shorter duration than the sample.
-- Otherwise since there's no envelope, the duration is always as long as the
-- sample, modulo ratios.
mkNoteDur :: FilePath -> Audio.Frame -> Audio.Frame -> Sample.Note
mkNoteDur dbDir start dur = Sample.Note
    { start = start
    , duration = dur
    , hash = Note.hash note
    , sample = (Sample.make (triFilename dbDir))
        { Sample.envelope = Signal.from_pairs
            [(sec start, 1), (sec (start + dur), 1), (sec (start + dur), 0)]
        }
    }
    where
    sec = AUtil.toSeconds
    note = Note.note "patch" "inst" (sec start) (sec dur)

-- * TODO copy paste with Faust.Render_test

listWavs :: FilePath -> IO [FilePath]
listWavs = fmap (List.sort . filter (".wav" `List.isSuffixOf`))
    . Directory.listDirectory

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat . map (dir</>) =<< listWavs dir

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap Vector.toList) . Resource.runResourceT
    . Audio.toSamples . Audio.extractChannel 0
