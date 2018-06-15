-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to do incremental render.  It hashes 'Note.Note's to skip
-- rerendering when possible.
module Synth.Lib.Checkpoint where
import qualified Control.DeepSeq as DeepSeq
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

import qualified Util.Audio.Audio as Audio
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
    -> IO ([(Int, Note.Hash)], Maybe State)
    -- ^ (remaining notes, state at that point)
skipCheckpoints outputDir hashes = do
    Directory.createDirectoryIfMissing False (outputDir </> cacheDir)
    files <- Directory.listDirectory (outputDir </> cacheDir)
    (hashes, stateFname) <- either errorIO return $
        findLastState (Set.fromList files) hashes
    (hashes,) <$> if null stateFname
        then return Nothing
        else Just . State
            <$> ByteString.readFile (outputDir </> cacheDir </> stateFname)

findLastState :: Set FilePath -> [(Int, Note.Hash)]
    -> Either Text ([(Int, Note.Hash)], FilePath)
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
            go stateFname nextState hashes
        | otherwise = Right ((i, hash) : hashes, prevStateFname)
        where
        fname = filenameOf2 i hash state
    go _ _ [] = Right ([], "")

{-
    Each chunk writes two files:

    -- $hash and $state at beginning of .wav
    000.$hash.$state.wav
    -- file contains the state at the end of the .wav, cached in $stateHash
    000.$hash.$state.state.$stateHash

    001.$hash.$state.wav -- $state == previous $stateHash
    001.$hash.$state.state.$stateHash -- as before
-}

getFilename :: FilePath -> IORef.IORef State -> (Int, Note.Hash)
    -> IO FilePath
getFilename outputDir stateRef (i, hash) = do
    state <- IORef.readIORef stateRef
    let fname = outputDir </> cacheDir </> filenameOf i hash state
    -- XXX 'state' is actually an unsafe pointer to the underlying C state, so
    -- I have to make sure I'm done with it before returning.  This is super
    -- sketchy, but it works now and it is non-copying.
    fname `DeepSeq.deepseq` return fname

writeState :: FilePath -> IORef.IORef State -> FilePath -> IO ()
writeState outputDir stateRef fname = do
    state@(State stateBs) <- IORef.readIORef stateRef
    let stateHash = encodeState state
    ByteString.writeFile
        (FilePath.replaceExtension fname (".state." <> stateHash))
        stateBs
    let current = outputDir </> filenameToCurrent (FilePath.takeFileName fname)
    -- 000.wav -> cache/000.$hash.$state.wav
    -- Atomically replace the old link, if any.
    Directory.createFileLink (cacheDir </> FilePath.takeFileName fname)
        (current <> ".tmp")
    Directory.renameFile (current <> ".tmp") current

-- | 000.$hash.$state.wav
filenameOf :: Int -> Note.Hash -> State -> FilePath
filenameOf i hash state = filenameOf2 i hash (encodeState state)

-- | 'filenameOf' but with 'State' already encoded.
filenameOf2 :: Int -> Note.Hash -> String -> FilePath
filenameOf2 i hash encodedState =
    ByteString.Char8.unpack (ByteString.Char8.intercalate "."
        [ zeroPad 3 i
        , fingerprint hash
        ]) <> "." <> encodedState <> ".wav"

filenameToCurrent :: FilePath -> FilePath
filenameToCurrent fname = case Seq.split "." fname of
    [num, _hash, _state, "wav"] -> num <> ".wav"
    _ -> fname

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

noteHashes :: Audio.Frame -> [Note.Note] -> [(Int, Note.Hash)]
noteHashes chunkSize = zip [0..] . hashOverlapping 0 (AUtil.toSeconds chunkSize)

hashOverlapping :: RealTime -> RealTime -> [Note.Note] -> [Note.Hash]
hashOverlapping start size =
    map (mconcat . map fst) . groupOverlapping start size . Seq.key_on Note.hash
    -- Pair each Note with its Hash, then group Notes and combine the Hashes.
    -- This ensures I only compute each Hash a maximum of once.

groupOverlapping :: RealTime -> RealTime -> [(a, Note.Note)]
    -> [[(a, Note.Note)]]
groupOverlapping start size = go (Seq.range_ start size)
    -- Use Seq.range_ instead of successive addition to avoid accumulating
    -- error.  Size should integral, but let's just be careful.
    where
    go (t1 : ts@(t2 : _)) notes
        | null notes = []
        | null overlapping && null rest = []
        | otherwise = overlapping : go ts rest
        where (overlapping, rest) = splitOverlapping t1 t2 notes
    go _ _ = []

{-
    0   1   2   3   4   5   6   7   8
    +---
        +-------------------
            +---
                    +---
-}
splitOverlapping :: RealTime -> RealTime -> [(a, Note.Note)]
    -> ([(a, Note.Note)], [(a, Note.Note)])
splitOverlapping start end notes = (overlapping, overlapping ++ rest)
    where
    overlapping = filter (not . (<=start) . Note.end . snd) here
    (here, rest) = span ((<end) . Note.start . snd) $
        dropWhile ((<=start) . Note.end . snd) notes
