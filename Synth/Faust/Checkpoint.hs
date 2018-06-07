-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support for rendering in chunks with state checkpoints.
module Synth.Faust.Checkpoint where
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

import qualified UnliftIO.Resource as Resource

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.File as File
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Faust.Hash as Hash
import qualified Synth.Faust.Render as Render
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Note as Note

import Global


type Error = Text

cacheDir :: FilePath
cacheDir = "cache"

write :: FilePath -> DriverC.Patch -> [Note.Note] -> IO (Maybe Error)
write = writeConfig Render.defaultConfig

writeConfig :: Render.Config -> FilePath -> DriverC.Patch -> [Note.Note]
    -> IO (Maybe Error)
writeConfig config outputDir patch notes =
    either Just (const Nothing) <$> do
        let hashes = noteHashes chunkSize notes
        (hashes, mbState) <- skipCheckpoints outputDir hashes
        stateRef <- IORef.newIORef $ DriverC.State mempty
        let notifyState = IORef.writeIORef stateRef
        let start = case hashes of
                (i, _) : _ -> AUtil.toSeconds (fromIntegral i * chunkSize)
                _ -> 0
        if null hashes
            then return (Right ())
            else AUtil.catchSndfile $ Resource.runResourceT $
                Audio.File.writeCheckpoints chunkSize
                    (getFilename outputDir stateRef)
                    (writeState outputDir stateRef)
                    AUtil.outputFormat (extendHashes hashes) $
                Render.renderPatch patch config mbState notifyState notes start
    where
    chunkSize = Render._chunkSize config

-- | Extend the [(index, hash)] list with 0 hashes.
--
-- 'Audio.File.writeCheckpoints' needs this because it still wants states
-- while rendering the decay of the last note.  Previously, I just had
-- 'Hash.overlapping' return an infinite list with 0s on the end, but I want
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
noteHashes chunkSize = zip [0..]
    . Hash.overlapping 0 (AUtil.toSeconds chunkSize)

-- | Find where the checkpoints begin to differ from the given 'Note.Hash's.
skipCheckpoints :: FilePath -> [(Int, Note.Hash)]
    -> IO ([(Int, Note.Hash)], Maybe DriverC.State)
    -- ^ (remaining notes, state at that point)
skipCheckpoints outputDir hashes = do
    Directory.createDirectoryIfMissing False (outputDir </> cacheDir)
    files <- Directory.listDirectory (outputDir </> cacheDir)
    (hashes, stateFname) <- either errorIO return $
        findLastState (Set.fromList files) hashes
    (hashes,) <$> if null stateFname
        then return Nothing
        else Just . DriverC.State
            <$> ByteString.readFile (outputDir </> cacheDir </> stateFname)

findLastState :: Set FilePath -> [(Int, Note.Hash)]
    -> Either Error ([(Int, Note.Hash)], FilePath)
findLastState files = go "" initialState
    where
    initialState = ByteString.Char8.unpack $ encodeState $ DriverC.State mempty
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
    -- 'hashes' should be infinite so this shouldn't happen.
    go _ _ [] = Right ([], "")

{-
    Each chunk writes two files:

    -- hash and state at beginning of .wav
    000.$hash.$state.wav
    -- state at the end of the .wav, hash of the state is cached in $stateHash
    000.$hash.$state.state.$stateHash

    001.$hash.$state.wav -- $state == previous $stateHash
-}

getFilename :: FilePath -> IORef.IORef DriverC.State -> (Int, Note.Hash)
    -> IO FilePath
getFilename outputDir stateRef (i, hash) = do
    state <- IORef.readIORef stateRef
    let fname = outputDir </> cacheDir </> filenameOf i hash state
    -- XXX 'state' is actually an unsafe pointer to the underlying C state, so
    -- I have to make sure I'm done with it before returning.  This is super
    -- sketchy, but it works now and it is non-copying.
    fname `DeepSeq.deepseq` return fname

writeState :: FilePath -> IORef.IORef DriverC.State -> FilePath -> IO ()
writeState outputDir stateRef fname = do
    state@(DriverC.State stateBs) <- IORef.readIORef stateRef
    let stateHash = ByteString.Char8.unpack $ encodeState state
    ByteString.writeFile
        (FilePath.replaceExtension fname (".state." <> stateHash))
        stateBs
    let current = outputDir </> filenameToCurrent (FilePath.takeFileName fname)
    -- 000.wav -> cache/000.$hash.$state.wav
    File.ignoreEnoent $ Directory.removeFile current
    Directory.createFileLink (cacheDir </> FilePath.takeFileName fname) current

-- | 000.$hash.$state.wav
filenameOf :: Int -> Note.Hash -> DriverC.State -> FilePath
filenameOf i hash state =
    -- Base64.URL uses A-Za-z_=-, so separate with dots.
    ByteString.Char8.unpack $ ByteString.Char8.intercalate "."
        [ zeroPad 3 i
        , encodeHash hash
        , encodeState state
        , "wav"
        ]

-- | 'filenameOf' but with 'DriverC.State' already encoded.
filenameOf2 :: Int -> Note.Hash -> String -> FilePath
filenameOf2 i hash encodedState =
    ByteString.Char8.unpack (ByteString.Char8.intercalate "."
        [ zeroPad 3 i
        , encodeHash hash
        ]) <> "." <> encodedState <> ".wav"

filenameToCurrent :: FilePath -> FilePath
filenameToCurrent fname = case Seq.split "." fname of
    [num, _hash, _state, "wav"] -> num <> ".wav"
    _ -> fname

encodeState :: DriverC.State -> ByteString.ByteString
encodeState = encodeHash . Note.Hash . CRC32.crc32 . unstate
    where unstate (DriverC.State bs) = bs

encodeHash :: Note.Hash -> ByteString.ByteString
encodeHash = fst . ByteString.Char8.spanEnd (=='=') . Base64.URL.encode
    . Serialize.encode

-- | 'Num.zeroPad' for ByteString.
zeroPad :: Show a => Int -> a -> ByteString.ByteString
zeroPad digits n =
    ByteString.Char8.replicate (digits - ByteString.length s) '0' <> s
    where s = ByteString.Char8.pack (show n)
