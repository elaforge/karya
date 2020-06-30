-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Render_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector.Storable as Vector

import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Resample as Resample
import qualified Util.Test.Testing as Testing

import qualified Synth.Faust.EffectC as EffectC
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Render as Render
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types
import           Util.Test


test_write_noop :: Test
test_write_noop = do
    dir <- Testing.tmp_dir "write"
    let write = write_ dir
    -- no notes produces no output
    io_equal (write []) (Right (0, 0))
    io_equal (Directory.listDirectory (dir </> Checkpoint.checkpointDir)) []
    result <- write [mkNote1 "" 0]
    left_like result "file not found"
    io_equal (listWavs dir) []

test_write_simple :: Test
test_write_simple = do
    (write, dir) <- tmpDb
    -- A single note that spans two checkpoints.  Dur is determined by the
    -- sample length.
    io_equal (write [mkNote1 dir 0]) (Right (2, 2))
    io_equal (length <$> listWavs dir) 2
    io_equal (readSamples dir) triangle

test_effect :: Test
test_effect = do
    (dir, write) <- testEffect
    let note start val = mkNoteEffect dir start "delay" [(0, val)]
    io_equal (write [note 0 0]) $ Right (2, 2)
    io_equal (readSamples dir) triangle
    -- Changing the effect control changes the note hashes.
    io_equal (write [note 0 2]) $ Right (3, 3)
    io_equal (readSamples dir) ([0, 0] ++ triangle ++ [0, 0])

test_effect_gain :: Test
test_effect_gain = do
    (dir, write) <- testEffect
    let note start val = mkNoteEffect dir start "gain" [(0, val)]
    io_equal (write [note 0 2]) $ Right (2, 2)
    io_equal (readSamples dir) $ map (*2) triangle

test_effect_2notes :: Test
test_effect_2notes = do
    (dir, write) <- testEffect
    let note start val = mkNoteEffect dir start "gain" [(0, val)]
    let zeros n = replicate n 0
    -- 2 notes, and the effect control changes.

    io_equal (write [note 0 2, note 8 3]) (Right (4, 4))
    io_equal (readSamples dir) $ zipWith (+)
        (map (*2) $ triangle ++ repeat 0)
        (map (*3) $ zeros 8 ++ triangle)

    io_equal (write [note 0 2, note 8 4]) (Right (2, 4))
    io_equal (readSamples dir) $ zipWith (+)
        (map (*2) $ triangle ++ repeat 0)
        (map (*4) $ zeros 8 ++ triangle)

    io_equal (write [note 0 2, note 4 4]) (Right (2, 3))
    io_equal (readSamples dir) $ zipWith (+)
        (map (*2) (take 4 triangle) ++ map (*4) (drop 4 triangle) ++ repeat 0)
        (map (*4) $ zeros 4 ++ triangle)

testEffect :: IO (FilePath, [Sample.Note] -> IO (Either Text (Int, Int)))
testEffect = do
    (_, dir) <- tmpDb
    effect <- makeEffect "test-delay"
    let write = writeQuality Resample.ZeroOrderHold (Just effect) dir
    return (dir, write)

_test_state_deterministic :: IO ()
_test_state_deterministic = do
    -- Ensure successive runs with the same inputs have the same state from
    -- libsamplerate.  Disabled because I have to run it in separate processes
    -- by hand to be sure.  Previously libsamplerate was non-deterministic, but
    -- it only showed up across process changes, due to memory layout.
    st1 <- getStates
    st2 <- getStates
    prettyp $ zip st1 st2

getStates :: IO [Either Render.Error Render.State]
getStates = do
    (_, dir) <- tmpDb
    let write = writeQuality Resample.SincMediumQuality Nothing dir
    io_equal (write [mkNote dir 0 2]) (Right (4, 4))
    let checkpoint = dir </> Checkpoint.checkpointDir
    fns <- List.sort . filter (".state." `List.isInfixOf`) <$>
        Directory.listDirectory checkpoint
    mapM (loadState . (checkpoint</>)) fns

loadState :: FilePath -> IO (Either Render.Error Render.State)
loadState fname = Render.unserializeState . Checkpoint.State <$>
    ByteString.readFile fname

test_write_simple_offset :: Test
test_write_simple_offset = do
    (write, dir) <- tmpDb
    io_equal (write [mkNote1 dir 3]) (Right (3, 3))
    io_equal (readSamples dir) (replicate 3 0 ++ triangle ++ [0])

test_write_silent_chunk :: Test
test_write_silent_chunk = do
    (write, dir) <- tmpDb
    io_equal (write [mkNote1 dir 5]) (Right (4, 4))
    sizes <- durations dir
    equal sizes [0, 4, 4, 4]
    -- If there are no notes in the middle, they go to silent chunks too.
    io_equal (write [mkNote1 dir 0, mkNote1 dir 16]) (Right (6, 6))
    sizes <- durations dir
    equal sizes [4, 4, 0, 0, 4, 4]

durations :: FilePath -> IO [Audio.Frames]
durations dir = mapM (\fname -> File.throwEnoent fname =<< File.duration fname)
    =<< listWavs dir

test_write_freq :: Test
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

test_write_ratios :: Test
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

test_write_incremental :: Test
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

test_resume_at_boundary :: Test
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

test_overlappingNotes :: Test
test_overlappingNotes = do
    let f start size = extract
            . Render.overlappingNotes
                (AUtil.toFrames start) (AUtil.toFrames size)
            . map noteS
        noteS (start, dur) =
            mkNoteDur "" (AUtil.toFrames start) (AUtil.toFrames dur)
        extract (a, b, c) = (map e a, map e b, map e c)
            where
            e n =
                ( AUtil.toSeconds (Sample.start n)
                , AUtil.toSeconds (Sample.duration n)
                )
    equal (f 0 1 []) ([], [], [])
    equal (f 0 1 [(0, 0)]) ([], [(0, 0)], [])
    equal (f 0 1 [(-4, 4), (-2, 4), (0, 4), (4, 4)])
        ([(-4, 4), (-2, 4)], [(0, 4)], [(4, 4)])
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
write_ = writeQuality Resample.ZeroOrderHold Nothing

writeQuality :: Resample.Quality -> Maybe Render.InstrumentEffect
    -> FilePath -> [Sample.Note] -> IO (Either Text (Int, Int))
writeQuality quality mbEffect outDir =
    Render.write config outDir mempty mbEffect
    where
    config = Render.Config
        { _quality = quality
        , _chunkSize = chunkSize
        , _blockSize = chunkSize
        -- In the real world, this is some fraction of chunkSize, but that
        -- makes control change times a bit inaccurate due to the annoying
        -- latency fiddling.
        , _controlsPerBlock = chunkSize
        , _emitProgress = False
        }

patchDir :: FilePath
patchDir = "patch"

chunkSize :: Audio.Frames
chunkSize = 4

-- mkDb :: FilePath -> Patch.Db
-- mkDb dir = Patch.db (dir </> patchDir)
--     [Patch.DbPatch $ Patch.simple "test" "tri.wav" NN.c4]

-- | Write a test sample into the db dir.
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
mkNoteSec dbDir start = mkNote dbDir (AUtil.toFrames start)

-- | mkNote where ratio=1.
mkNote1 :: FilePath -> Audio.Frames -> Sample.Note
mkNote1 dbDir start = mkNote dbDir start 1

-- | Make a note with a constant ratio.
mkNote :: FilePath -> Audio.Frames -> Double -> Sample.Note
mkNote dbDir start ratio = mkNoteRatios dbDir start [(start, ratio)]

mkNoteRatios :: FilePath -> Audio.Frames -> [(Audio.Frames, Double)]
    -> Sample.Note
mkNoteRatios dbDir start ratios_ = mkNoteAll dbDir start ratios_ mempty

-- | Like 'mkNoteRatios', except set a shorter duration than the sample.
-- Otherwise since there's no envelope, the duration is always as long as the
-- sample, modulo ratios.
mkNoteDur :: FilePath -> Audio.Frames -> Audio.Frames -> Sample.Note
mkNoteDur dbDir start dur =
    Sample.note start dur mempty
        ((Sample.make (triFilename dbDir))
            { Sample.envelope = Signal.from_pairs
                [(sec start, 1), (sec (start + dur), 1), (sec (start + dur), 0)]
            })
    where
    sec = AUtil.toSeconds

mkNoteAll :: FilePath -> Audio.Frames -> [(Audio.Frames, Double)]
    -> Map Control.Control Signal.Signal -> Sample.Note
mkNoteAll dbDir start ratios_ effectControls =
    Sample.note start
        (RenderSample.predictDuration (Signal.shift (- sec start) ratios) dur)
        effectControls
        ((Sample.make (triFilename dbDir)) { Sample.ratios = ratios })
    where
    dur = Audio.Frames $ length triangle
    sec = AUtil.toSeconds
    ratios = Signal.from_pairs $ map (first sec) ratios_

-- * effect

mkNoteEffect :: FilePath -> Audio.Frames -> Control.Control
    -> [(Signal.X, Signal.Y)] -> Sample.Note
mkNoteEffect dbDir start control vals =
    mkNoteAll dbDir start [(start, 1)]
        (Map.singleton control (Signal.from_pairs vals))

makeEffect :: Text -> IO Render.InstrumentEffect
makeEffect name = do
    patch <- case Map.lookup name EffectC.patches of
        Just (Right patch) -> return patch
        Just (Left err) -> errorIO $ "effect " <> name <> ": " <> err
        Nothing -> errorIO $ "no effect: " <> name
    return $ Render.InstrumentEffect
        { _effectPatch = patch
        , _effectConfig = Patch.EffectConfig
            { _effectName = name
            , _renameControls = mempty
            }
        }

controlNote :: RealTime -> RealTime -> Control.Control
    -> [(Signal.X, Signal.Y)] -> Note.Note
controlNote start dur control vals =
    Note.withControl control (Signal.from_pairs vals) $
    Note.testNote start dur

-- * TODO copy paste with Faust.Render_test

listWavs :: FilePath -> IO [FilePath]
listWavs dir =
    fmap (map (dir</>) . List.sort . filter (".wav" `List.isSuffixOf`)) $
    Directory.listDirectory dir

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat =<< listWavs dir

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap Vector.toList) . Resource.runResourceT
    . Audio.toSamples . Audio.extractChannel 0
