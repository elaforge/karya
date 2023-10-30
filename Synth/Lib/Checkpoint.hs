-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Functions to do incremental render.  It hashes 'Note.Note's to skip
-- rerendering when possible.
module Synth.Lib.Checkpoint (
    checkpointDir
    , State(..)
    , skipCheckpoints
    , findLastState
    , write
    , linkOutput
    , clearRemainingOutput
    -- * hash
    , noteHashes
    , Span(..)
    , hashOverlapping
    , groupOverlapping
    , overlappingHashes
#ifdef TESTING
    , module Synth.Lib.Checkpoint
#endif
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Data.Vector.Storable as Vector.Storable

import qualified GHC.TypeLits as TypeLits
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Files as Files
import qualified Util.Lists as Lists
import qualified Util.Streams as Streams

import qualified Derive.Stack as Stack
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import qualified Ui.Id as Id

import           Global
import           Synth.Types


-- | This subdirectory in the outputDirectory </> instrument has the
-- fingerprinted audio files.
checkpointDir :: FilePath
checkpointDir = "checkpoint"

-- * state

-- | This is the opaque state for a synthesizer or signal processor.  It should
-- be possible to resume synthesis by saving and restoring it.
--
-- TODO maybe [ByteString] for multiple states
newtype State = State ByteString.ByteString
    deriving (Eq, Show)

instance Pretty State where
    pretty = txt . stateFingerprint

stateFingerprint :: State -> String
stateFingerprint (State bytes) = Note.fingerprintBytes bytes

-- * checkpoints

-- | Find where the checkpoints begin to differ from the given 'Note.Hash's.
skipCheckpoints :: FilePath -> State
    -> [(Config.ChunkNum, Note.Hash)]
    -> IO ([FilePath], [(Config.ChunkNum, Note.Hash)], Maybe State)
    -- ^ (skipped chunks, remaining notes, state at that point)
skipCheckpoints outputDir initialState hashes = do
    -- Debug.put "hashes" (map (second Note.encodeHash) hashes)
    Directory.createDirectoryIfMissing False (outputDir </> checkpointDir)
    files <- Directory.listDirectory (outputDir </> checkpointDir)
    let (skipped, (remainingHashes, stateFname)) =
            findLastState (Set.fromList files) initialState hashes
    mbState <- if null stateFname
        then return Nothing
        else Just . State
            <$> ByteString.readFile (outputDir </> checkpointDir </> stateFname)
    return (skipped, remainingHashes, mbState)

-- | Find the first 'Note.Hash' that doesn't have a matching filename.
--
-- Since the output state of the previous filename needs to match the input
-- state of the next one as described in 'writeState', this has to follow the
-- files in sequence.
findLastState :: Set FilePath -> State -> [(Config.ChunkNum, Note.Hash)]
    -> ([FilePath], ([(Config.ChunkNum, Note.Hash)], FilePath))
    -- ^ ([skipped], (remainingHashes, resumeState))
findLastState files = go "" . stateFingerprint
    where
    go prevStateFname state ((chunknum, hash) : hashes)
        | fname `Set.member` files = case Set.lookupGT prefix files of
            Just stateFname | prefix `List.isPrefixOf` stateFname ->
                first (fname:) $ go stateFname nextState $
                    -- I ran out of notes, but there are still chunks.  This
                    -- indicates that there is a decay after the last note,
                    -- so keep following chunks with empty note hash.  They
                    -- were rendered in the first place beceause 'extendHash'
                    -- does the same thing for 'write'.
                    if null hashes then [(chunknum+1, mempty)] else hashes
                where nextState = drop (length prefix) stateFname
            -- I didn't find a corresponding .state file for the .wav.  This
            -- can happen if a previous render was killed while writing them.
            -- Since the files are written atomically, the .state file marks
            -- the end of the transaction, so I should just be able to ignore
            -- an orphaned .wav.
            _ -> done
        | otherwise = done
        where
        done
            -- This means I'm "in the decay", as above, so don't return one
            -- of my made-up empty note hashes.  This way 'write' will notice
            -- null hashes, and skip all work.
            | hash == mempty && null hashes = ([], ([], ""))
            | otherwise = ([], ((chunknum, hash) : hashes, prevStateFname))
        prefix = FilePath.replaceExtension fname ".state."
        fname = filenameOf2 chunknum hash state
    go _ _ [] = ([], ([], ""))

-- ** write

-- | Write the audio with checkpoints.
write :: Bool -> FilePath -> Set Id.TrackId -> Config.ChunkNum -> Audio.Frames
    -> [(Config.ChunkNum, Note.Hash)]
    -> IO State -- ^ get current audio state, see NOTE [audio-state]
    -> AUtil.Audio
    -> IO (Either Text (Config.ChunkNum, Config.ChunkNum))
    -- ^ Either Error (writtenChunks, total)
write emitProgress outputDir trackIds skippedCount chunkSize hashes getState
        audio
    | null hashes = return $ Right (0, skippedCount)
    | otherwise = do
        result <- AUtil.catchSndfile $ Resource.runResourceT $
            Audio.File.writeCheckpoints
                chunkSize (getFilename outputDir getState) chunkComplete
                AUtil.outputFormat (extendHashes hashes) $
                checkLevel emitWarning (fromIntegral skippedCount * chunkSize)
                    maxLevel audio
        return $ case result of
            Left err -> Left err
            Right written -> Right (written, written + skippedCount)
    where
    chunkComplete fname = do
        writeState getState fname
        chunknum <- linkOutput False outputDir (FilePath.takeFileName fname)
        when emitProgress $ emit $ Config.WaveformsCompleted [chunknum]
    emitWarning frame val = liftIO $ emit $ Config.Warn Stack.empty $
        pretty (AUtil.toSeconds frame) <> ": sample " <> pretty val <> " > "
        <> pretty maxLevel <> ", this may causae a DAW to automute"
        -- At least Reaper does this.
    maxLevel = 1
    emit payload = Config.emitMessage $ Config.Message
        { _blockId = Config.pathToBlockId outputDir
        , _trackIds = trackIds
        , _instrument = Config.dirToInstrument outputDir
        , _payload = payload
        }

-- | Pass audio stream unchanged, but emit warnings if a abs val of a sample
-- goes over the limit.
checkLevel :: forall m rate chan. (TypeLits.KnownNat chan, Monad m)
    => (Audio.Frames -> Audio.Sample -> m ()) -> Audio.Frames -> Audio.Sample
    -> Audio.Audio m rate chan -> Audio.Audio m rate chan
checkLevel emitWarning startFrame maxLevel =
    Audio.apply (Streams.mapAccumL check startFrame)
    where
    check frame block = do
        let peak = blockMax block
        -- TODO find the actual frame, not just start of block.
        when (abs peak > maxLevel) $ emitWarning frame peak
        return (frame + Audio.blockFrames (Proxy @chan) block, block)
    blockMax = \case
        Audio.Constant _ val -> val
        Audio.Block v
            | Vector.Storable.null v -> 0
            | otherwise -> Vector.Storable.maximum v

getFilename :: FilePath -> IO State -> (Config.ChunkNum, Note.Hash)
    -> IO FilePath
getFilename outputDir getState (chunknum, hash)
    -- This can happen if tempo is set really slow.
    | chunknum >= maxChunk =
        Audio.throwIO $ "chunk num over limit: " <> showt chunknum
    | otherwise = do
        state <- getState
        let fname = outputDir </> checkpointDir
                </> filenameOf chunknum hash state
        -- XXX 'state' is actually an unsafe pointer to the underlying C state,
        -- so I have to make sure I'm done with it before returning.  This is
        -- super sketchy, but it works now and it is non-copying.
        fname `DeepSeq.deepseq` return fname

{- | Write synth state to the checkpointDir.  The filename is derived from the
    audio chunk filename, which presumably has already been written.

    Each chunk writes two files:

    -- $hash over the chunk, and $state at beginning of .wav
    000.$hash.$state.wav
    -- file contains the state at the end of the .wav, fingerprint is $endState
    000.$hash.$state.state.$endState

    001.$hash.$state.wav -- $state == previous $endState
    001.$hash.$state.state.$endState -- as before
-}
writeState :: IO State -> FilePath -> IO ()
writeState getState fname = do
    state@(State stateBs) <- getState
    Files.writeAtomic
        (FilePath.replaceExtension fname (".state." <> stateFingerprint state))
        stateBs

-- | Link the audio chunk output (presumably already written) from the
-- checkpointDir to its position in the output sequence.
--
-- > 000.wav -> checkpoint/000.$hash.$state.wav
linkOutput :: Bool -> FilePath -> FilePath -> IO Config.ChunkNum
linkOutput updateMtime outputDir fname = do
    let current = outputDir </> filenameToOutput fname
    Files.symlink (checkpointDir </> fname) current
    -- Bump mtime to protect it from ImGc for a while after it becomes dead.
    when updateMtime $
        Directory.setModificationTime (outputDir </> checkpointDir </> fname)
            =<< Time.getCurrentTime
    return $ fromMaybe (error $ "no parse: " <> show current) $
        Config.isOutputLink $ FilePath.takeFileName current

-- | Remove any remaining output symlinks past the final chunk.
clearRemainingOutput :: FilePath -> Config.ChunkNum -> IO ()
clearRemainingOutput outputDir start = do
    mapM_ (Directory.removeFile . (outputDir</>)) . outputPast start
        =<< Directory.listDirectory outputDir
    -- Uptime timestamps for tools/im-gc.py.
    now <- Time.getCurrentTime
    mapM_ (flip Directory.setModificationTime now . (outputDir</>))
        =<< Directory.listDirectory outputDir

outputPast :: Config.ChunkNum -> [FilePath] -> [FilePath]
outputPast start =
    map snd . filter ((>=start) . fst) . Lists.keyOnJust Config.isOutputLink

filenameToOutput :: FilePath -> FilePath
filenameToOutput fname = case Lists.split "." fname of
    [num, _hash, _state, "wav"] -> num <> ".wav"
    _ -> fname

-- | 000.$hash.$state.wav
filenameOf :: Config.ChunkNum -> Note.Hash -> State -> FilePath
filenameOf chunknum hash state =
    filenameOf2 chunknum hash (stateFingerprint state)

-- | 'filenameOf' but with 'State' already encoded.
filenameOf2 :: Config.ChunkNum -> Note.Hash -> String -> FilePath
filenameOf2 chunknum hash encodedState =
    ByteString.Char8.unpack (ByteString.Char8.intercalate "."
        [ zeroPad 3 chunknum
        , ByteString.Char8.pack $ Note.encodeHash hash
        ]) <> "." <> encodedState <> ".wav"

-- | Crash after this chunk number.  It's not an inherent limitation, but
-- it indicates that something has probably gone off the rails.  Also
-- 'Config.isOutputLink' doesn't want to parse more than 3 digits.
maxChunk :: Config.ChunkNum
maxChunk = 500

-- | 'Num.zeroPad' for ByteString.
zeroPad :: Show a => Int -> a -> ByteString.ByteString
zeroPad digits n =
    ByteString.Char8.replicate (digits - ByteString.length s) '0' <> s
    where s = ByteString.Char8.pack (show n)


-- * hash

-- | Extend the [(index, hash)] list with mempty hashes.
--
-- 'Audio.File.writeCheckpoints' needs this because it still wants states
-- while rendering the decay of the last note.  Previously, I just had
-- 'hashOverlapping' return an infinite list with 0s on the end, but I want
-- 'skipCheckpoints' to be able to detect when it ran out of notes so I can
-- avoid rerendering the decay in that case, and it's hard to do that when it
-- can't tell the difference between out of notes, and just no notes at this
-- moment in time.
extendHashes :: [(Int, Note.Hash)] -> [(Int, Note.Hash)]
extendHashes = go
    where
    go [] = []
    go [(i, h)] = (i, h) : zip [i+1 ..] (repeat mempty)
    go (h : hs) = h : go hs

noteHashes :: Audio.Frames -> [Span] -> [(Int, Note.Hash)]
noteHashes chunkSize = zip [0..] . hashOverlapping 0 (AUtil.toSeconds chunkSize)

data Span = Span {
    _start :: RealTime
    , _duration :: RealTime
    , _hash :: Note.Hash
    } deriving (Show)

instance Pretty Span where
    pretty (Span start dur hash) = pretty start <> "+" <> pretty dur
        <> "(" <> pretty hash <> ")"

hashOverlapping :: RealTime -> RealTime -> [Span] -> [Note.Hash]
hashOverlapping start size =
    map (mconcat . map fst) . groupOverlapping start size
    . Lists.keyOn _hash
    -- Pair each Note with its Hash, then group Notes and combine the Hashes.

overlappingHashes :: RealTime -> RealTime -> [Span] -> [[Note.Hash]]
overlappingHashes start size =
    map (map fst) . groupOverlapping start size . Lists.keyOn _hash


{- | Group all Spans that overlap the given range.  So:

    > 0   1   2   3   4   5   6   7   8
    > |=======|=======|=======|
    >     a------
    >         b---c-----
    >                  d---

    Should be: [[a], [a, b, c], [c, d]]
-}
groupOverlapping :: RealTime -> RealTime -> [(a, Span)] -> [[(a, Span)]]
groupOverlapping start size = go (Lists.range_ start size)
    -- Use Lists.range_ instead of successive addition to avoid accumulating
    -- error.  Size should integral, but let's just be careful.
    where
    go (t1 : ts@(t2 : _)) spans
        | null spans = []
        | null overlapping && null rest = []
        | otherwise = overlapping : go ts rest
        where (overlapping, rest) = splitOverlapping t1 t2 spans
    go _ _ = []

splitOverlapping :: RealTime -> RealTime -> [(a, Span)]
    -> ([(a, Span)], [(a, Span)])
splitOverlapping start end spans = (overlapping, overlapping ++ rest)
    where
    overlapping = filter (not . passed . snd) here
    (here, rest) = span ((<end) . _start . snd) $
        dropWhile (passed . snd) spans
    passed n = _start n + _duration n <= start && _start n < start
