-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Synth.Faust.Render_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector.Storable as Vector

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Test.Testing as Testing

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Synth.Faust.DriverC as DriverC
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

test_write_incremental = do
    dir <- Testing.tmp_dir "write"
    patch <- getPatch "sine"
    let write = Render.write_ config dir mempty patch
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

    let skipCheckpoints = Checkpoint.skipCheckpoints dir
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
    let write = Render.write_ config dir mempty patch
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
    let write = Render.write_ config dir mempty patch
    let chunks n = replicate (Num.assertIntegral (fromIntegral chunkCount * n))

    let withPitch = Note.withControl Control.pitch . Signal.from_pairs
            . map (first chunkToTime)
    let note2 pitches = [withPitch pitches $ mkNote "test" 0 2 0]

    io_equal (write $ mkNotes "test" 1 [60, 62]) (Right (2, 2))
    io_equal (readSamples dir) $ chunks 1 60 ++ chunks 1 62

    io_equal (write $ mkNotes "test" 1 [60, 64]) (Right (1, 2))
    io_equal (readSamples dir) $ chunks 1 60 ++ chunks 1 64

    -- The value changes but only at control blocks, so at 0.5.
    io_equal (write $ note2 [(0, 40), (0.35, 42)]) (Right (2, 2))
    io_equal (readSamples dir) $ chunks 0.5 40 ++ chunks 1.5 42

    -- Value interpolates.
    io_equal (write $ note2 [(0, 40), (1, 42)]) (Right (2, 2))
    io_equal (readSamples dir) $ chunks 0.5 40 ++ chunks 0.5 41 ++ chunks 1 42

    -- 0 1 2 3 4 5 6 7 |
    -- c       c       |

-- TODO test volume and dyn

renderSamples :: DriverC.Patch -> [Note.Note] -> IO [Float]
renderSamples patch notes = do
    dir <- Testing.tmp_dir "renderSamples"
    io_equal (Render.write_ config dir mempty patch notes) (Right (3, 3))
    readSamples dir

readSamples :: FilePath -> IO [Float]
readSamples dir = toSamples . File.concat . map (dir</>) =<< listWavs dir

listWavs :: FilePath -> IO [FilePath]
listWavs = fmap (List.sort . filter (".wav" `List.isSuffixOf`))
    . Directory.listDirectory

config :: Render.Config
config = Render.Config
    { _chunkSize = 8
    , _controlSize = 4
    , _controlsPerBlock = 8 `div` 4
    , _maxDecay = 0
    }

chunkSize :: Audio.Frame
chunkSize = Render._chunkSize config

chunkCount :: Int
chunkCount = Audio.framesCount (Proxy @2) chunkSize

getPatch :: Text -> IO DriverC.Patch
getPatch name = do
    patches <- mapM (either errorIO return) =<< DriverC.getPatches
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
toSamples = fmap (concatMap Vector.toList) . Resource.runResourceT
    . Audio.toSamples


-- * render

test_renderControls = do
    let f notes start = filter (not . null . snd) $ Map.toList $
            fmap toSamples1 $
            Render.renderControls 1 controls (map make notes) start
        controls = Set.fromList
            [ ("1", Control.gate), ("2", Control.gate)
            , ("1", Control.pitch), ("2", Control.pitch)
            , ("", Control.pan)
            ]
        make (s, e, elem, cs) = (Note.note "" "" s e)
            { Note.element = elem
            , Note.controls = Signal.from_pairs <$> Map.fromList cs
            }
    -- ensure that controls end, per-element and global controls are correct,
    -- controls have the right block size
    equal (f [] 0) []
    equal (f [(0, 5, "1", [(Control.pan, [(0, 0.5)])])] 0)
        [ (("", Control.pan), [0.5])
        , (("1", Control.gate), [1, 0.8, 0.6, 0.4, 0.2, 0])
        ]
    equal (f [(0, 2, "1", [(Control.pitch, [(0, 42)])])] 0)
        [ (("1", Control.gate), [1, 0.5, 0])
        , (("1", Control.pitch), [42])
        ]
    let pitch nn = [(Control.pitch, [(0, nn)])]
    equal (f [(0, 2, "1", pitch 42), (2, 4, "1", pitch 44)] 0)
        [ (("1", Control.gate), [1, 0.5, 1, 0.75, 0.5, 0.25, 0])
        , (("1", Control.pitch), [42, 42, 44])
        ]

toSamples1 :: AUtil.Audio1 -> [Audio.Sample]
toSamples1 = Vector.toList . mconcat
    . Unsafe.unsafePerformIO . Resource.runResourceT . Audio.toSamples

test_gateBreakpoints = do
    let f = Render.gateBreakpoints . map (uncurry (Note.note "" ""))
    equal (f []) []
    equal (f [(0, 1)]) [(0, 0), (0, 1), (1, 0)]
    equal (f [(0, 1), (1, 1)]) [(0, 0), (0, 1), (1, 0), (1, 0), (1, 1), (2, 0)]

    -- -- TODO if I enable gate combining
    -- equal (f [(0, 1)]) [(0, 0), (0, 1), (1, 1), (1, 0)]
    -- equal (f [(0, 1), (1, 1)]) [(0, 0), (0, 1), (2, 1), (2, 0)]
    -- equal (f [(0, 1), (1, 1), (3, 1)])
    --     [(0, 0), (0, 1), (2, 1), (2, 0), (3, 0), (3, 1), (4, 1), (4, 0)]
    -- equal (f [(1, 1), (3, 1)])
    --     [(1, 0), (1, 1), (2, 1), (2, 0), (3, 0), (3, 1), (4, 1), (4, 0)]

test_controlBreakpoints = do
    let f = Render.controlBreakpoints "c" . map make
        make (s, e, cs) = (Note.note "" "" s e)
            { Note.controls = Map.singleton "c" (Signal.from_pairs cs) }
    equal (f []) []
    equal (f [(0, 1, []), (1, 1, [])]) [(0, 0), (1, 0)]
    -- Audio.linear should optimize away duplicate and flat breakpoints.
    equal (f [(0, 1, [(0, 1)]), (1, 1, [(0, 1)])]) [(0, 1), (1, 1), (1, 1)]

    equal (f [(0, 1, [(0, 1)])]) [(0, 1)]
    -- Signals stay constant.
    equal (f [(0, 1, [(0, 1)]), (2, 1, [(2, 2)])])
        [(0, 1), (2, 1), (2, 2)]
    -- Notes clip other notes.
    equal (f [(0, 1, [(0, 0), (6, 6)]), (2, 1, [(0, 0)])])
        [(0, 0), (2, 2), (2, 0)]
