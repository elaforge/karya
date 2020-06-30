-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Synth.Faust.Render_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Seq as Seq
import qualified Util.Test.Testing as Testing

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Faust.InstrumentC as InstrumentC
import qualified Synth.Faust.Render as Render
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Types
import           Util.Test


-- * write

test_write_silent_chunk = do
    dir <- Testing.tmp_dir "write"
    patch <- getPatch "test"
    let write = Render.write config dir mempty patch
    -- not quite 1 chunk
    io_equal (write [mkNote "test" 0.25 2 42]) (Right (3, 3))
    io_equal (readSamples1 dir) ([0, 0] ++ replicate 16 42 ++ replicate (8-2) 0)

    -- TODO disabled, see useLeadingSilence = False.
    -- -- skip 2 chunks
    -- io_equal (write [mkNote "test" 2.5 2 42]) (Right (5, 5))
    -- sizes <- fmap (map Sndfile.frames) . mapM File.getInfo =<< listWavs dir
    -- equal sizes [0, 0, 8, 8, 8]
    -- -- The first two chunks are empty, so they get skipped.
    -- io_equal (readSamples1 dir)
    --     (replicate 4 0 ++ replicate 16 42 ++ replicate 4 0)

test_write_incremental = do
    dir <- Testing.tmp_dir "write"
    patch <- getPatch "sine"
    let write = Render.write config dir mempty patch
    -- no notes produces no output
    io_equal (write []) (Right (0, 0))
    io_equal (Directory.listDirectory (dir </> Checkpoint.checkpointDir)) []

    let oldNotes = mkNotes "sine" 0.5 [NN.c4, NN.d4, NN.e4, NN.f4, NN.g4]
    let newNotes = mkNotes "sine" 0.5 [NN.c4, NN.d4, NN.e4, NN.f4, NN.c4]

    -- Should be be 2.5 checkpoints.
    io_equal (write oldNotes) (Right (3, 3))
    wavs <- listWavs dir
    equal_on length wavs 3
    io_equal (length <$> listWavs (dir </> Checkpoint.checkpointDir)) 3
    states <- filter (".state." `List.isInfixOf`) <$>
        Directory.listDirectory (dir </> Checkpoint.checkpointDir)
    -- All of them have states, since they are at the end of each chunk.
    io_equal (mapM (Directory.getFileSize
            . ((dir </> Checkpoint.checkpointDir) </>)) states)
        [40, 40, 40]

    let skipCheckpoints =
            Checkpoint.skipCheckpoints dir (Checkpoint.State mempty)
            . Checkpoint.noteHashes chunkSize . map Render.toSpan
    -- Test skipCheckpoints directly.
    (_, remainingHashes, state) <- skipCheckpoints newNotes
    equal_on (fmap fst . Seq.head) remainingHashes (Just 2)
    equal ((/= Checkpoint.State mempty) <$> state) (Just True)

    -- change only last note: only 3rd sample should rerender, but contents
    -- should be the same.
    io_equal (write newNotes) (Right (1, 3))

    (_, remainingHashes, _) <- skipCheckpoints newNotes
    equal remainingHashes []

    -- Only 1 was rerendered, so now there are 4.
    wavs <- listWavs (dir </> Checkpoint.checkpointDir)
    equal_on length wavs 4

    incremental <- readSamples dir
    nonIncremental <- renderSamples patch newNotes
    equal incremental nonIncremental

    -- Switched back to the old ones, nothing new should render.
    io_equal (write oldNotes) (Right (0, 3))

    (_, remainingHashes, state) <- skipCheckpoints oldNotes
    equal remainingHashes []
    equal state Nothing
    -- Should have rendered 0 more files, since Render.renderPatch should exit
    -- immediately, due to start >= end.
    wavs <- listWavs (dir </> Checkpoint.checkpointDir)
    equal_on length wavs 4

test_write_incremental_offset = do
    -- faust always starts rendering at 0, even if the first note doesn't.
    -- So this works trivially.  But later I'll want to skip empty time, so
    -- I want to keep the test.
    dir <- Testing.tmp_dir "write"
    patch <- getPatch "sine"
    let write = Render.write config dir mempty patch
    let notes = drop 1 $ mkNotes "sine" 0.5
            [NN.c4, NN.d4, NN.e4, NN.f4, NN.g4]

    -- Should be be 2.5 checkpoints.
    io_equal (write notes) (Right (3, 3))
    wavs <- listWavs dir
    equal_on length wavs 3

    samples <- readSamples dir
    pprint samples
    equal (take 4 samples) (replicate 4 0)
    -- TODO also test checkpoints are lined up right

test_write_controls = do
    dir <- Testing.tmp_dir "write"
    patch <- getPatch "test"
    let write = Render.write config dir mempty patch
    let cblocks n = replicate $ fromIntegral (Render._controlSize config) * n

    let withPitch = Note.withControl Control.pitch . Signal.from_pairs
            . map (first chunkToTime)
    let note dur pitches = [withPitch pitches $ mkNote "test" 0 dur 0]

    -- Each block is 2 cblocks, and controls change 1 cblock early.
    io_equal (write $ mkNotes "test" 1 [60, 62]) (Right (2, 2))
    io_equal (readSamples1 dir) $ cblocks 1 60 ++ cblocks 3 62

    io_equal (write $ mkNotes "test" 1 [60, 64]) (Right (1, 2))
    io_equal (readSamples1 dir) $ cblocks 1 60 ++ cblocks 1 62 ++ cblocks 2 64

    -- Value interpolates.
    io_equal (write $ note 3 [(0, 40), (1, 40), (2, 42)]) (Right (3, 3))
    io_equal (readSamples1 dir) $ cblocks 2 40 ++ cblocks 1 41 ++ cblocks 3 42

    -- 0 1 2 3 4 5 6 7 |
    -- c       c       |

-- TODO test volume and dyn

renderSamples :: InstrumentC.Patch -> [Note.Note] -> IO [Float]
renderSamples patch notes = do
    dir <- Testing.tmp_dir "renderSamples"
    io_equal (Render.write config dir mempty patch notes) (Right (3, 3))
    readSamples dir

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat =<< listWavs dir

readSamples1 :: FilePath -> IO [Float]
readSamples1 dir = map (/2) . toSamples1 . Audio.mixChannels @_ @_ @2
    . File.concat <$> listWavs dir

listWavs :: FilePath -> IO [FilePath]
listWavs dir =
    fmap (map (dir</>) . List.sort . filter (".wav" `List.isSuffixOf`)) $
    Directory.listDirectory dir

config :: Render.Config
config = Render.Config
    { _chunkSize = 8
    , _blockSize = 8
    , _controlSize = 4
    , _controlsPerBlock = 8 `div` 4
    , _maxDecay = 0
    , _emitProgress = False
    }

chunkSize :: Audio.Frames
chunkSize = Render._chunkSize config

controlSize :: Audio.Frames
controlSize = Render._controlSize config

chunkCount :: Int
chunkCount = Audio.framesCount (Proxy @2) chunkSize

getPatch :: Text -> IO InstrumentC.Patch
getPatch name = do
    patches <- mapM (either errorIO return) =<< InstrumentC.getPatches
    return $ fromMaybe (error $ "no patch: " <> show name) $
        Map.lookup name patches

mkNotes :: Note.PatchName -> RealTime -> [Pitch.NoteNumber] -> [Note.Note]
mkNotes patch durInChunks nns =
    [ mkNote patch start durInChunks nn
    | (start, nn) <- zip (iterate (+durInChunks) 0) nns
    ]

mkNote :: Note.PatchName -> RealTime -> RealTime -> Pitch.NoteNumber
    -> Note.Note
mkNote patch start dur nn =
    Note.withControl Control.volume (Signal.constant 1) $
    Note.withControl Control.dynamic (Signal.constant 1) $
    Note.withPitch nn $
    Note.note patch patch (chunkToTime start) (chunkToTime dur)

chunkToTime :: RealTime -> RealTime
chunkToTime = (* AUtil.toSeconds chunkSize)

toSamples :: AUtil.Audio -> IO [Audio.Sample]
toSamples = fmap (concatMap V.toList) . Resource.runResourceT . Audio.toSamples

toSamples1 :: AUtil.Audio1 -> [Audio.Sample]
toSamples1 = V.toList . mconcat
    . Unsafe.unsafePerformIO . Resource.runResourceT . Audio.toSamples


-- * render

test_renderControls = do
    let f impulseGate start notes =
            filter (not . null . snd) $ Map.toList $ fmap toSamples1 $
                renderControls impulseGate (map mknote notes) start
        mknote (s, e, elem, cs) = (Note.testNote (s / cr) (e / cr))
            { Note.element = elem
            , Note.controls = Signal.from_pairs <$>
                Map.fromList ((Control.dynamic, [(0, 1)]) : cs)
            }
            where cr = fromIntegral $ Render._controlRate Render.defaultConfig
    equal (f False 0 []) []
    equal (f False 0 [(0, 5, "1", [(Control.pan, [(0, 0.5)])])])
        [ (("", Control.pan), [0.5])
        -- Note at 0 starts at +1 control, thanks to tweakNotes.
        , (("1", Control.gate), [0, 1, 1, 1, 1, 1, 0])
        ]
    equal (f True 0 [(0, 5, "1", [])])
        [ (("1", Control.gate), [0, 1, 0])
        ]
    equal (f False 0 [(0, 2, "1", [(Control.pitch, [(0, 42)])])])
        [ (("1", Control.gate), [0, 1, 1, 0])
        , (("1", Control.pitch), [42])
        ]
    let pitch nn = [(Control.pitch, [(0, nn)])]
    equal (f False 0 [(0, 2, "1", pitch 42), (2, 4, "1", pitch 44)])
        [ (("1", Control.gate), [0, 1, 1, 1, 1, 1, 0])
        , (("1", Control.pitch), [42, 44])
        ]
    equal (f True 0 [(1, 2, "1", pitch 42), (3, 4, "1", pitch 44)])
        [ (("1", Control.gate), [0, 1, 0, 1, 0])
        , (("1", Control.pitch), [42, 42, 44])
        ]

renderControls :: Bool -> [Note.Note] -> RealTime
    -> Map InstrumentC.Control AUtil.Audio1
renderControls impulseGate notes start =
    Render.renderControls Render.defaultConfig patch notes start
    where
    patch = InstrumentC.Patch
        { _name = "test"
        , _doc = "doc"
        , _impulseGate = impulseGate
        , _elementFrom = Nothing
        , _controls = Map.fromList $ map (, ((), cconfig))
            [ ("1", Control.gate), ("2", Control.gate)
            , ("1", Control.pitch), ("2", Control.pitch)
            , ("", Control.pan)
            ]
        , _inputControls = []
        , _outputs = 2
        , _ptr = ()
        }
    cconfig = InstrumentC.ControlConfig
        { _constant = False
        , _description = ""
        }

test_gateBreakpoints = do
    let f ns = map (first (AUtil.toFrames . RealTime.seconds)) $
            Render.gateBreakpoints controlSize True
                [mkNote "patch" s d 42 | (s, d) <- ns]
    equal (f []) []

    equal (f [(0, 1)]) [(0, 0), (0, 1), (4, 1), (4, 0)]
    equal (f [(0, 1), (1, 1)])
        [ (0, 0), (0, 1), (4, 1), (4, 0)
        , (8, 0), (8, 1), (12, 1), (12, 0)
        ]

    -- -- TODO if I enable gate combining
    -- equal (f [(0, 1)]) [(0, 0), (0, 1), (1, 1), (1, 0)]
    -- equal (f [(0, 1), (1, 1)]) [(0, 0), (0, 1), (2, 1), (2, 0)]
    -- equal (f [(0, 1), (1, 1), (3, 1)])
    --     [(0, 0), (0, 1), (2, 1), (2, 0), (3, 0), (3, 1), (4, 1), (4, 0)]
    -- equal (f [(1, 1), (3, 1)])
    --     [(1, 0), (1, 1), (2, 1), (2, 0), (3, 0), (3, 1), (4, 1), (4, 0)]

test_controlBreakpoints = do
    let f = Render.controlBreakpoints 1 False "c" . map make
        make (s, e, cs) = (Note.testNote s e)
            { Note.controls = Map.singleton "c" (Signal.from_pairs cs) }
    -- No controls means don't set anything, which will keep the defaults.
    equal (f []) []
    equal (f [(0, 1, []), (1, 1, [])]) []
    equal (f [(0, 1, [(0, 1)]), (1, 1, [(0, 1)])]) [(0, 1), (1, 1)]

    equal (f [(0, 1, [(0, 1)])]) [(0, 1)]
    -- Signals stay constant.
    equal (f [(0, 1, [(0, 1)]), (2, 1, [(2, 2)])])
        [(0, 1), (2, 1), (2, 2)]
    -- Notes clip other notes.
    equal (f [(0, 1, [(0, 0), (6, 6)]), (2, 1, [(0, 0)])])
        [(0, 0), (2, 2), (2, 0)]
