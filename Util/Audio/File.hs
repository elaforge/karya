-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
-- | Functions to read and write audio files.  This should be able to read all
-- formats supported by libsndfile.
module Util.Audio.File (
    -- * read
    check, checkA, getInfo, read, readFrom, read44k, readUnknown
    , concat
    -- * write
    , write, writeCheckpoints
    , wavFormat
) where
import           Prelude hiding (concat, read)
import qualified Control.Exception as Exception
import qualified Control.Monad.Fix as Fix
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Vector.Storable as V
import qualified GHC.Stack as Stack
import qualified GHC.TypeLits as TypeLits
import qualified Sound.File.Sndfile as Sndfile
import qualified Sound.File.Sndfile.Buffer.Vector as Sndfile.Buffer.Vector
import qualified Streaming.Prelude as S
import qualified System.Directory as Directory
import qualified System.IO.Error as IO.Error

import qualified Util.Audio.Audio as Audio
import qualified Util.Num as Num

import           Global


-- | Check if rate and channels match the file.
check :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Proxy rate -> Proxy channels -> FilePath -> IO (Maybe String)
check rate channels fname = checkInfo rate channels <$> getInfo fname

-- | Like 'check', but take 'Audio.Audio' instead of Proxy.
checkA :: forall m rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Proxy (Audio.Audio m rate channels) -> FilePath -> IO (Maybe String)
checkA _ = check (Proxy :: Proxy rate) (Proxy :: Proxy channels)

getInfo :: FilePath -> IO Sndfile.Info
getInfo fname = Exception.bracket (openRead fname) Sndfile.hClose
    (return . Sndfile.hInfo)

-- | Since the file is opened only when samples are demanded, a sample rate or
-- channels mismatch will turn into an exception then, not when this is called.
--
-- As a special case, if the file channels is 1, it will be expanded to
-- fit whatever channel count was requested.
read :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    FilePath -> Audio.AudioIO rate channels
read = readFrom (Audio.Frames 0)

readFrom :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Audio.Duration -> FilePath -> Audio.AudioIO rate channels
readFrom (Audio.Seconds secs) fname =
    readFrom (Audio.Frames (Audio.secondsToFrame rate secs)) fname
    where rate = Audio.natVal (Proxy :: Proxy rate)
readFrom (Audio.Frames (Audio.Frame frame)) fname = Audio.Audio $ do
    (key, handle) <- lift $ Resource.allocate (openRead fname) Sndfile.hClose
    let info = Sndfile.hInfo handle
        fileChan = Sndfile.channels info
    when (Sndfile.samplerate info /= rate || fileChan `notElem` [1, chan]) $
        throw $ formatError rate chan info
    when (frame > 0) $
        liftIO $ do
            -- Otherwise libsndfile will throw a much more confusing error:
            -- "Internal psf_fseek() failed."
            Audio.assert (0 <= frame && frame < Sndfile.frames info) $
                "tried to seek to " <> pretty frame <> " in " <> showt fname
                <> ", but it only has " <> pretty (Sndfile.frames info)
            void $ Sndfile.hSeek handle Sndfile.AbsoluteSeek frame
    let size = fromIntegral Audio.blockSize
    Fix.fix $ \loop -> liftIO (Sndfile.hGetBuffer handle size) >>= \case
        Nothing -> lift (Resource.release key) >> return ()
        Just buf -> do
            let block = Sndfile.Buffer.Vector.fromBuffer buf
            -- Sndfile should enforce this, but let's be sure.
            when (fileChan /= 1 && V.length block `mod` chan /= 0) $
                throw $ "block length " <> show (V.length block)
                    <> " not a multiple of channels " <> show chan
            S.yield $ if fileChan == chan
                then block
                else Audio.expandV chan block
            loop
    where
    rate = Audio.natVal (Proxy :: Proxy rate)
    channels = Proxy :: Proxy channels
    chan = Audio.natVal channels
    throw msg = liftIO $
        Exception.throwIO $ IO.Error.mkIOError IO.Error.userErrorType
            ("reading file: " <> msg) Nothing (Just fname)

read44k :: FilePath -> Audio.AudioIO 44100 2
read44k = read

readUnknown :: FilePath -> IO (Sndfile.Format, Audio.UnknownAudioIO)
readUnknown input = do
    info <- getInfo input
    case (someNat (Sndfile.samplerate info), someNat (Sndfile.channels info)) of
        (TypeLits.SomeNat (_::Proxy rate), TypeLits.SomeNat (_::Proxy chan)) ->
            return (Sndfile.format info,
                Audio.UnknownAudio $ read @rate @chan input)

someNat :: Int -> TypeLits.SomeNat
someNat int = case TypeLits.someNatVal (fromIntegral int) of
    Nothing -> error $ "not a natural: " <> show int
    Just n -> n

-- | Concatenate multiple files.
concat :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    [FilePath] -> Audio.AudioIO rate channels
concat = Audio.Audio . mconcat . map (Audio._stream . read @rate @channels)

-- ** util

checkInfo :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Proxy rate -> Proxy channels -> Sndfile.Info -> Maybe String
checkInfo rate_ channels_ info
    | Sndfile.samplerate info == rate && Sndfile.channels info == channels =
        Nothing
    | otherwise = Just $ formatError rate channels info
    where
    rate = Audio.natVal rate_
    channels = Audio.natVal channels_

formatError :: Audio.Rate -> Audio.Channels -> Sndfile.Info -> String
formatError rate channels info =
    "requested (rate, channels) " ++ show (rate, channels)
    ++ " but file had " ++ show (Sndfile.samplerate info, Sndfile.channels info)

openRead :: FilePath -> IO Sndfile.Handle
openRead fname = annotate fname $
    Sndfile.openFile fname Sndfile.ReadMode Sndfile.defaultInfo

-- Sndfile's errors don't include the filename.
annotate :: Stack.HasCallStack => FilePath -> IO a -> IO a
annotate fname = Exception.handle $ \exc ->
    Audio.throwIO $ txt $ "opening " <> show fname
        <> ": " <> Sndfile.errorString exc

-- * write

write :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Sndfile.Format -> FilePath -> Audio.AudioIO rate channels
    -> Resource.ResourceT IO ()
write format fname audio = do
    (key, handle) <- Resource.allocate (openWrite format tmp audio)
        Sndfile.hClose
    S.mapM_ (liftIO . Sndfile.hPutBuffer handle
            . Sndfile.Buffer.Vector.toBuffer)
        (Audio._stream audio)
    Resource.release key
    liftIO $ Directory.renameFile tmp fname
    where
    tmp = fname ++ ".write.tmp"

-- | Write files in chunks to the given directory.  Run actions before
-- and after writing each chunk.
writeCheckpoints :: forall rate chan state.
    (TypeLits.KnownNat rate, TypeLits.KnownNat chan)
    => Audio.Frame
    -> (state -> IO FilePath) -- ^ get filename for this state
    -> (Int -> FilePath -> IO ()) -- ^ write state after the computation
    -> Sndfile.Format -> [state]
    -- ^ Some render-specific state for each checkpoint.  Shouldn't run out
    -- before the audio runs out.
    -> Audio.AudioIO rate chan -> Resource.ResourceT IO Int
    -- ^ number of checkpoints written
writeCheckpoints size getFilename chunkComplete format = go 0
    where
    go !written (state : states) audio = do
        fname <- liftIO $ getFilename state
        (blocks, audio) <- Audio.takeFramesGE size audio
        if null blocks
            then return chunknum
            else do
                -- The blocks should sum to 'size', except the last one, which
                -- could be smaller.  But I can't pull from 'audio' without
                -- changing the state, so I have to wait until the next loop to
                -- see if this one was short.
                Audio.assert (written `mod` size == 0) $
                    "non-final chunk was too short, expected " <> pretty size
                    <> ", but last chunk was " <> pretty (written `mod` size)
                let blockSize = Num.sum $ map V.length blocks
                -- In count, not frames, in case I somehow get an odd count.
                Audio.assert (blockSize <= sizeCount) $
                    "chunk too long, expected " <> pretty sizeCount
                    <> ", but got " <> pretty (map V.length blocks)
                let tmp = fname ++ ".write.tmp"
                liftIO $ do
                    Exception.bracket (openWrite format tmp audio)
                        Sndfile.hClose (\handle -> mapM_ (write handle) blocks)
                    Directory.renameFile tmp fname
                    chunkComplete  chunknum fname
                go (written + size) states audio
        where
        chunknum = fromIntegral $ written `div` size
    go _ [] _ = liftIO $ Exception.throwIO $ Audio.Exception "out of states"
    write handle = Sndfile.hPutBuffer handle . Sndfile.Buffer.Vector.toBuffer
    sizeCount = Audio.framesCount chan size
    chan = Proxy @chan

openWrite :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Sndfile.Format -> FilePath -> Audio.AudioIO rate channels
    -> IO Sndfile.Handle
openWrite format fname _audio =
    annotate fname $ Sndfile.openFile fname Sndfile.WriteMode info
    where
    info = Sndfile.defaultInfo
        { Sndfile.samplerate = Audio.natVal (Proxy :: Proxy rate)
        , Sndfile.channels = Audio.natVal (Proxy :: Proxy channels)
        , Sndfile.format = format
        }

wavFormat :: Sndfile.Format
wavFormat = Sndfile.Format
    { headerFormat = Sndfile.HeaderFormatWav
    , sampleFormat = Sndfile.SampleFormatFloat
    , endianFormat = Sndfile.EndianFile
    }
