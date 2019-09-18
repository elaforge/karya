-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
-- | Functions to read and write audio files.  This should be able to read all
-- formats supported by libsndfile.
module Util.Audio.File (
    -- * read
    check, checkA, getInfo, duration
    , read, read44k, readUnknown
    , readFrom, readFromClose
    , concat
    -- * write
    , write, writeCheckpoints
    , wavFormat
) where
import           Prelude hiding (concat, read)
import qualified Control.Exception as Exception
import qualified Control.Monad.Fix as Fix
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.IORef as IORef
import qualified Data.Vector.Storable as V
import qualified GHC.Stack as Stack
import qualified GHC.TypeLits as TypeLits
import qualified Sound.File.Sndfile.Buffer.Vector as Sndfile.Buffer.Vector
import qualified Streaming.Prelude as S
import qualified System.Directory as Directory
import qualified System.IO.Error as IO.Error

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.Sndfile as Sndfile
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
getInfo fname = Exception.bracket (openRead fname) close
    (return . Sndfile.hInfo . _handle)

duration :: FilePath -> IO Audio.Frame
duration = fmap (Audio.Frame . Sndfile.frames) . getInfo

-- | Since the file is opened only when samples are demanded, a sample rate or
-- channels mismatch will turn into an exception then, not when this is called.
--
-- As a special case, if the file channels is 1, it will be expanded to
-- fit whatever channel count was requested.
read :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    FilePath -> Audio.AudioIO rate channels
read = readFrom 0

-- | Like 'readFrom', but return an action that closes the handle.  This is
-- for Audio.takeClose, so it can close the file early if it terminates the
-- stream early.
readFromClose :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Audio.Frame -> FilePath -> IO (IO (), Audio.AudioIO rate channels)
readFromClose frame fname = do
    handle <- openRead fname
    return $ (close handle,) $ Audio.Audio $ do
        lift $ Resource.register (close handle)
        S.map Audio.Block $ readHandle rate chan frame fname handle
    where
    rate = Audio.natVal (Proxy :: Proxy rate)
    channels = Proxy :: Proxy channels
    chan = Audio.natVal channels

readFrom :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Audio.Frame -> FilePath -> Audio.AudioIO rate channels
readFrom frame fname = Audio.Audio $ do
    (_, handle) <- lift $ Resource.allocate (openRead fname) close
    S.map Audio.Block $ readHandle rate chan frame fname handle
    where
    rate = Audio.natVal (Proxy :: Proxy rate)
    channels = Proxy :: Proxy channels
    chan = Audio.natVal channels


readHandle :: Trans.MonadIO m => Audio.Rate -> Audio.Channels -> Audio.Frame
    -> FilePath -> Handle
    -> S.Stream (S.Of (V.Vector Audio.Sample)) m ()
readHandle rate chan (Audio.Frame frame) fname hdl = do
    let info = Sndfile.hInfo $ _handle hdl
        fileChan = Sndfile.channels info
    when (Sndfile.samplerate info /= rate || fileChan `notElem` [1, chan]) $
        throw $ formatError rate chan info
    when (frame > 0) $
        liftIO $ do
            -- Otherwise libsndfile will throw a much more confusing error:
            -- "Internal psf_fseek() failed."
            -- It's ok to seek to the end of the file though, and that
            -- happens when the resample consumed all samples, but they're
            -- in its internal buffer.
            Audio.assert (0 <= frame && frame <= Sndfile.frames info) $
                "tried to seek to " <> pretty frame <> " in " <> showt fname
                <> ", but it only has " <> pretty (Sndfile.frames info)
            void $ Sndfile.hSeek (_handle hdl) Sndfile.AbsoluteSeek frame
    let size = fromIntegral Audio.blockSize
    Fix.fix $ \loop -> liftIO (Sndfile.hGetBuffer (_handle hdl) size) >>= \case
        Nothing -> liftIO $ close hdl
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
    throw msg = liftIO $
        Exception.throwIO $ IO.Error.mkIOError IO.Error.userErrorType
            ("reading file: " <> msg) Nothing (Just fname)

read44k :: FilePath -> Audio.AudioIO 44100 2
read44k = read

readUnknown :: FilePath -> IO (Sndfile.Format, Audio.UnknownAudioIO)
readUnknown fname = do
    info <- getInfo fname
    case (Audio.someNat (Sndfile.samplerate info),
            Audio.someNat (Sndfile.channels info)) of
        (TypeLits.SomeNat (_::Proxy rate), TypeLits.SomeNat (_::Proxy chan)) ->
            return
                ( Sndfile.format info
                , Audio.UnknownAudio $ read @rate @chan fname
                )

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
    "requested (rate, channels) " <> show (rate, channels)
    <> " but file had " <> show (Sndfile.samplerate info, Sndfile.channels info)

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
    (key, handle) <- Resource.allocate (openWrite format tmp audio) close
    S.mapM_ (write handle) (Audio._stream audio)
    Resource.release key
    liftIO $ Directory.renameFile tmp fname
    where
    write handle = liftIO
        . mapM_ (Sndfile.hPutBuffer (_handle handle)
            . Sndfile.Buffer.Vector.toBuffer)
        . Audio.blockSamples
    tmp = fname <> ".write.tmp"

-- | Write files in chunks to the given directory.  Run actions before
-- and after writing each chunk.
writeCheckpoints :: forall rate chan state.
    (TypeLits.KnownNat rate, TypeLits.KnownNat chan)
    => Audio.Frame
    -> (state -> IO FilePath) -- ^ get filename for this state
    -> (FilePath -> IO ()) -- ^ write state after the computation
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
        -- TODO I should be able to have a special file format for constant 0
        -- blocks <- return $ concatMap Audio.blockSamples blocks
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
                let blockCount = Num.sum $ map Audio.blockCount blocks
                -- Show the error with count, not frames, in case I somehow get
                -- an odd count.
                Audio.assert (blockCount <= sizeCount) $
                    "chunk too long, expected " <> pretty sizeCount
                    <> ", but got " <> pretty (map Audio.blockCount blocks)
                let tmp = fname ++ ".write.tmp"
                -- Debug.tracepM "write" (fname, blocks)
                liftIO $ do
                    Exception.bracket (openWrite format tmp audio) close
                        (\hdl -> writeBlock (_handle hdl) blocks)
                    Directory.renameFile tmp fname
                    chunkComplete fname
                go (written + size) states audio
        where
        chunknum = fromIntegral $ written `div` size
    go _ [] _ = liftIO $ Exception.throwIO $ Audio.Exception "out of states"
    sizeCount = Audio.framesCount chan size
    chan = Proxy @chan

-- | Because writeCheckpoints writes equal sized chunks, except the last one,
-- I can abbreviate a constant 0 chunk as an empty file.  play_cache has
-- special logic to detect that, and other programs will just consider it
-- empty.
writeBlock :: Sndfile.Handle -> [Audio.Block] -> IO ()
writeBlock hdl blocks
    | all isZero blocks = return ()
    | otherwise = mapM_ write $ concatMap Audio.blockSamples blocks
    where
    isZero (Audio.Constant _ 0) = True
    isZero _ = False
    write = Sndfile.hPutBuffer hdl . Sndfile.Buffer.Vector.toBuffer


-- * Handle

-- | libsndfile has no protection against multiple closes on the same handle
-- and happily double frees memory, and hsndfile provides no protection either.
data Handle = Handle !(IORef.IORef Bool) !Sndfile.Handle

_handle :: Handle -> Sndfile.Handle
_handle (Handle _ hdl) = hdl

openRead :: FilePath -> IO Handle
openRead fname = annotate fname $ do
    hdl <- Sndfile.openFile fname Sndfile.ReadMode Sndfile.defaultInfo
    open <- IORef.newIORef True
    return $ Handle open hdl

close :: Handle -> IO ()
close (Handle open hdl) = whenM (IORef.readIORef open)
    (IORef.atomicWriteIORef open False >> Sndfile.hClose hdl)

openWrite :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Sndfile.Format -> FilePath -> Audio.AudioIO rate channels
    -> IO Handle
openWrite format fname _audio = do
    open <- IORef.newIORef True
    annotate fname $
        Handle open <$> Sndfile.openFile fname Sndfile.WriteMode info
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
