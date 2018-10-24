-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to do incremental render.  It hashes 'Note.Note's to skip
-- rerendering when possible.
module Synth.Lib.Checkpoint where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Set as Set

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Text.Read as Read

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Note as Note

import Global
import Synth.Lib.Global


cacheDir :: FilePath
cacheDir = "cache"

-- * state

-- | This is the opaque state for a synthesizer or signal processor.  It should
-- be possible to resume synthesis by saving and restoring it.
--
-- TODO maybe [ByteString] for multiple states
newtype State = State ByteString.ByteString
    deriving (Eq, Show)

instance Pretty State where
    pretty = txt . encodeState

encodeState :: State -> String
encodeState = ByteString.Char8.unpack . fingerprint . CRC32.crc32 . unstate
    where unstate (State bs) = bs

fingerprint :: Serialize.Serialize a => a -> ByteString.ByteString
fingerprint = fst . ByteString.Char8.spanEnd (=='=') . Base64.URL.encode
    . Serialize.encode

-- * checkpoints

-- | Find where the checkpoints begin to differ from the given 'Note.Hash's.
skipCheckpoints :: FilePath -> [(Int, Note.Hash)]
    -> IO ([FilePath], [(Int, Note.Hash)], Maybe State)
    -- ^ (skipped chunks, remaining notes, state at that point)
skipCheckpoints outputDir hashes = do
    Directory.createDirectoryIfMissing False (outputDir </> cacheDir)
    files <- Directory.listDirectory (outputDir </> cacheDir)
    (skipped, (hashes, stateFname)) <- either errorIO return $
        findLastState (Set.fromList files) hashes
    mbState <- if null stateFname
        then return Nothing
        else Just . State
            <$> ByteString.readFile (outputDir </> cacheDir </> stateFname)
    return (skipped, hashes, mbState)

-- | Find the first 'Note.Hash' that doesn't have a matching filename.
--
-- Since the output state of the previous filename needs to match the input
-- state of the next one as described in 'writeState', this has to follow the
-- files in sequence.
findLastState :: Set FilePath -> [(Int, Note.Hash)]
    -> Either Text ([FilePath], ([(Int, Note.Hash)], FilePath))
findLastState files = go "" initialState
    where
    initialState = encodeState $ State mempty
    go prevStateFname state ((i, hash) : hashes)
        | fname `Set.member` files = do
            let prefix = FilePath.replaceExtension fname ".state."
            (stateFname, nextState) <- case Set.lookupGT prefix files of
                Just stateFname | prefix `List.isPrefixOf` stateFname ->
                    Right (stateFname, drop (length prefix) stateFname)
                _ -> Left $ "no state: " <> txt prefix
            first (fname:) <$> go stateFname nextState hashes
        | otherwise = Right ([], ((i, hash) : hashes, prevStateFname))
        where
        fname = filenameOf2 i hash state
    go _ _ [] = Right ([], ([], ""))

-- ** write

-- | Write the audio with checkpoints.
write :: FilePath -> Int -> Audio.Frame -> [(Int, Note.Hash)]
    -> IORef.IORef State -> AUtil.Audio
    -> IO (Either Text (Int, Int))
    -- ^ Either Error (writtenChunks, total)
write outputDir skippedCount chunkSize hashes stateRef audio
    | null hashes = return $ Right (0, skippedCount)
    | otherwise = do
        result <- AUtil.catchSndfile $ Resource.runResourceT $
            Audio.File.writeCheckpoints chunkSize
                (getFilename outputDir stateRef)
                (\fn -> writeState stateRef fn
                    >> linkOutput outputDir fn)
                AUtil.outputFormat (extendHashes hashes)
                audio
        case result of
            Left err -> return $ Left err
            Right written -> do
                clearRemainingOutput outputDir (written + skippedCount)
                return $ Right (written, written + skippedCount)

getFilename :: FilePath -> IORef.IORef State -> (Int, Note.Hash)
    -> IO FilePath
getFilename outputDir stateRef (i, hash) = do
    state <- IORef.readIORef stateRef
    let fname = outputDir </> cacheDir </> filenameOf i hash state
    -- XXX 'state' is actually an unsafe pointer to the underlying C state, so
    -- I have to make sure I'm done with it before returning.  This is super
    -- sketchy, but it works now and it is non-copying.
    fname `DeepSeq.deepseq` return fname

{- | Write synth state to the cache.  The filename is derived from the audio
    chunk filename, which presumably has already been written.

    Each chunk writes two files:

    -- $hash and $state at beginning of .wav
    000.$hash.$state.wav
    -- file contains the state at the end of the .wav, cached in $endState
    000.$hash.$state.state.$endState

    001.$hash.$state.wav -- $state == previous $endState
    001.$hash.$state.state.$endState -- as before
-}
writeState :: IORef.IORef State -> FilePath -> IO ()
writeState stateRef fname = do
    state@(State stateBs) <- IORef.readIORef stateRef
    File.writeAtomic
        (FilePath.replaceExtension fname (".state." <> encodeState state))
        stateBs

-- | Link the audio chunk output (presumably already written) from the cache to
-- its position in the output sequence.
--
-- > 000.wav -> cache/000.$hash.$state.wav
linkOutput :: FilePath -> FilePath -> IO ()
linkOutput outputDir fname = do
    let current = outputDir </> filenameToOutput (FilePath.takeFileName fname)
    -- Atomically replace the old link, if any.
    Directory.createFileLink (cacheDir </> FilePath.takeFileName fname)
        (current <> ".tmp")
    Directory.renameFile (current <> ".tmp") current

-- | Remove any remaining output symlinks past the final chunk.
clearRemainingOutput :: FilePath -> Int -> IO ()
clearRemainingOutput outputDir start =
    mapM_ (Directory.removeFile . (outputDir</>)) . outputPast start
        =<< Directory.listDirectory outputDir

outputPast :: Int -> [FilePath] -> [FilePath]
outputPast start = map snd . filter ((>=start) . fst) . Seq.key_on_just isOutput
    where
    isOutput (c1:c2:c3 : ".wav")
        | Just n <- Read.readMaybe [c1, c2, c3] = Just n
        | otherwise = Nothing
    isOutput _ = Nothing

filenameToOutput :: FilePath -> FilePath
filenameToOutput fname = case Seq.split "." fname of
    [num, _hash, _state, "wav"] -> num <> ".wav"
    _ -> fname

-- | 000.$hash.$state.wav
filenameOf :: Int -> Note.Hash -> State -> FilePath
filenameOf i hash state = filenameOf2 i hash (encodeState state)

-- | 'filenameOf' but with 'State' already encoded.
filenameOf2 :: Int -> Note.Hash -> String -> FilePath
filenameOf2 i hash encodedState =
    ByteString.Char8.unpack (ByteString.Char8.intercalate "."
        [ zeroPad 3 i
        , ByteString.Char8.pack $ Note.encodeHash hash
        ]) <> "." <> encodedState <> ".wav"

-- | 'Num.zeroPad' for ByteString.
zeroPad :: Show a => Int -> a -> ByteString.ByteString
zeroPad digits n =
    ByteString.Char8.replicate (digits - ByteString.length s) '0' <> s
    where s = ByteString.Char8.pack (show n)


-- * hash

-- | Extend the [(index, hash)] list with 0 hashes.
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
    go [(i, h)] = (i, h) : zip [i+1 ..] (repeat (Note.Hash 0))
    go (h : hs) = h : go hs

noteHashes :: Audio.Frame -> [Span] -> [(Int, Note.Hash)]
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
    . Seq.key_on _hash
    -- Pair each Note with its Hash, then group Notes and combine the Hashes.


{- | Group all Spans that overlap the given range.  So:

    > 0   1   2   3   4   5   6   7   8
    > |=======|=======|=======|
    >     a------
    >         b---c-----
    >                  d---

    Should be: [[a], [a, b, c], [c, d]]
-}
groupOverlapping :: RealTime -> RealTime -> [(a, Span)] -> [[(a, Span)]]
groupOverlapping start size = go (Seq.range_ start size)
    -- Use Seq.range_ instead of successive addition to avoid accumulating
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
