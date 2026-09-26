-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Debugging utilities.
module Cmd.Repl.LDebug where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified System.IO as IO

import qualified Util.Log as Log
import qualified Util.Memory as Memory
import qualified Util.Num as Num
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl.LPerf as LPerf
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import qualified Derive.Cache as Cache
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Warp as Warp

import qualified Perform.Midi.Types as Types
import qualified Ui.Id as Id
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui

import           Global
import           Types


-- | GHC's opinion on allocated memory.
rtsAllocated :: MonadIO m => m Memory.Size
rtsAllocated = liftIO Memory.rtsAllocated

-- | OS opinion on RSS and VSIZE.
rssVsize :: MonadIO m => m (Memory.Size, Memory.Size)
rssVsize = liftIO Memory.rssVsize

-- Also see 'LPerf.extract_debug' and similar functions.

-- * block

-- | Save state in a format that can be copy-pasted into a test, and loaded
-- with 'Derive.DeriveTest.derive_dump'.
dumpBlocks :: FilePath -> Cmd.CmdL ()
dumpBlocks fname = do
    state <- Simple.dumpState
    liftIO $ writeDump fname state

-- | Like 'dump_blocks', but only dump a single block.
dumpBlock :: FilePath -> BlockId -> Cmd.CmdL ()
dumpBlock fname blockId = do
    state <- Ui.get
    block <- Ui.getBlock blockId
    state <- Ui.evalRethrow "dump_block"
        (state { Ui.stateBlocks = Map.singleton blockId block })
        Simple.dumpState
    liftIO $ writeDump fname state

writeDump :: FilePath -> Simple.State -> IO ()
writeDump fname = writeFile fname . PPrint.pshow

-- * perf events

dumpBlockPerfEvents :: FilePath -> BlockId -> Cmd.CmdL ()
dumpBlockPerfEvents fname blockId = do
    events <- LPerf.midiConvert . LEvent.eventsOf
        =<< LPerf.blockEvents blockId
    dumpPerfEvents fname (LEvent.eventsOf events)

dumpPerfEvents :: FilePath -> [Types.Event] -> Cmd.CmdL ()
dumpPerfEvents fname events = liftIO $ IO.writeFile fname $
    PPrint.pshow (map Simple.dumpExactPerfEvent events)

-- * undo

showHistory :: Cmd.CmdL Text
showHistory = do
    saveFile <- ("save file: "<>) . showt <$> Cmd.gets Cmd.stateSaveFile
    hist <- Pretty.formatted <$> Cmd.gets Cmd.stateHistory
    config <- Pretty.formatted <$> Cmd.gets Cmd.stateHistoryConfig
    collect <- Pretty.formatted <$> Cmd.gets Cmd.stateHistoryCollect
    pure $ Text.unlines $ map Text.strip [saveFile, hist, config, collect]

-- * extract from Performance

trackSignal :: Cmd.CmdL (Maybe Track.TrackSignal)
trackSignal = do
    (blockId, _, trackId, _) <- Selection.getInsert
    perf <- LPerf.get blockId
    pure $ Map.lookup (blockId, trackId) (Cmd.perfTrackSignals perf)

-- | Get collected warps of a specific track.
getWarps :: Cmd.M m => BlockId -> TrackId -> m [TrackWarp.TrackWarp]
getWarps blockId trackId = do
    perf <- LPerf.getRoot
    let trackWarps = Cmd.perfWarps perf
        warps = [tw | tw <- trackWarps, TrackWarp.twBlock tw == blockId,
            Set.member trackId (TrackWarp.twTracks tw)]
    pure $ map (\w -> w { TrackWarp.twWarp = Warp.identity }) warps

-- | Get all raw uncollected TrackWarps from the root, and strip out the
-- signals so they don't take up tons of space.
getTrackWarps :: Cmd.M m => m (Map Stack.Stack TrackWarp.Track)
getTrackWarps = do
    result <- LPerf.derive =<< Ui.getRootId
    let wmap = Derive.collectWarpMap $ Derive.stateCollect $
            Derive.rState result
    let strip (TrackWarp.Track s e _warp bid tid) =
            TrackWarp.Track s e Warp.identity bid tid
    pure $ strip <$> wmap

-- ** cache

-- | Extract the cache logs, with no summarizing.
cacheLogs :: BlockId -> Cmd.CmdL Text
cacheLogs blockId = do
    perf <- Cmd.getPerformance blockId
    let logs = filter wanted $ Cmd.perfLogs perf
    pure $ Text.unlines
        [ Texts.join2 ": " (formatStack msg) (Log.msgText msg)
        | msg <- logs
        ]
    where
    wanted log = Cache.isCacheLog log || PlayUtil.isScoreDamageLog log
    formatStack = maybe "" Stack.prettyUi_ . Log.msgStack

-- | Stats for both block and track caches from the given block.
cacheStats :: BlockId -> Cmd.CmdL String
cacheStats blockId = do
    block <- blockCache blockId
    track <- trackCache blockId
    pure $ unlines
        ["block:", formatStats block, "track:", formatStats track]

-- | Get summarized stats for cached blocks.
blockCache :: BlockId -> Cmd.CmdL ([(Text, [BlockId])], [(BlockId, Int)])
blockCache blockId =
    Play.extractCacheStats Play.getBlockId . Cmd.perfLogs <$>
        Cmd.getPerformance blockId

-- | Get summarized stats for cached tracks on the given block.
trackCache :: BlockId -> Cmd.CmdL ([(Text, [TrackId])], [(TrackId, Int)])
trackCache blockId = do
    (rederived, cached) <- (bimap rederivedBlock cachedBlock)
        . Play.extractCacheStats Play.getTrackId . Cmd.perfLogs
        <$> Cmd.getPerformance blockId
    pure (rederived, cached)
    where
    rederivedBlock = map (second $ map snd . filter ((==blockId) . fst))
    cachedBlock = map (first snd) . filter ((==blockId) . fst . fst)

formatStats :: Id.Ident id => ([(Text, [id])], [(id, Int)]) -> String
formatStats (rederived, cached) =
    "cached: " <> formatCached cached <> "\n"
        <> unlines (map formatRederived rederived)
    where
    formatRederived (because, ids) =
        untxt because <> ": [" <> show (length ids) <> "] "
        <> unwords (map (untxt . Id.identName) ids)
    formatCached cached =
        show (length cached) <> " [" <> show (Num.sum (map snd cached)) <> "] "
        <> unwords (map (untxt . Id.identName . fst) cached)
