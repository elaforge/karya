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
    , concat, readCheckpoints
    -- * write
    , write, writeCheckpoints
    , wavFormat
    -- * misc
    , throwEnoent
) where
import           Prelude hiding (concat, read)
import qualified Control.Exception as Exception
import qualified Control.Monad.Fix as Fix
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Vector.Storable as V
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
check rate channels fname =
    maybe (Just $ "file not found: " <> fname) (checkInfo rate channels) <$>
        getInfo fname

-- | Like 'check', but take 'Audio.Audio' instead of Proxy.
checkA :: forall m rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Proxy (Audio.Audio m rate channels) -> FilePath -> IO (Maybe String)
checkA _ = check (Proxy :: Proxy rate) (Proxy :: Proxy channels)

getInfo :: FilePath -> IO (Maybe Sndfile.Info)
getInfo fname =
    Exception.bracket (openRead fname) (maybe (return ()) Sndfile.hClose)
        (return . fmap Sndfile.hInfo)

duration :: FilePath -> IO (Maybe Audio.Frames)
duration = fmap (fmap (Audio.Frames . Sndfile.frames)) . getInfo

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
    Audio.Frames -> FilePath -> IO (IO (), Audio.AudioIO rate channels)
readFromClose frame fname = do
    handle <- openReadThrow fname
    return $ (Sndfile.hClose handle,) $ Audio.Audio $ do
        lift $ Resource.register (Sndfile.hClose handle)
        S.map Audio.Block $ readHandle rate chan frame fname handle
    where
    rate = Audio.natVal (Proxy :: Proxy rate)
    channels = Proxy :: Proxy channels
    chan = Audio.natVal channels

readFrom :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Audio.Frames -> FilePath -> Audio.AudioIO rate channels
readFrom frame fname = Audio.Audio $ do
    (_, handle) <- lift $ Resource.allocate (openReadThrow fname) Sndfile.hClose
    S.map Audio.Block $ readHandle rate chan frame fname handle
    where
    rate = Audio.natVal (Proxy :: Proxy rate)
    channels = Proxy :: Proxy channels
    chan = Audio.natVal channels

readHandle :: MonadIO m => Audio.Rate -> Audio.Channels -> Audio.Frames
    -> FilePath -> Sndfile.Handle
    -> S.Stream (S.Of (V.Vector Audio.Sample)) m ()
readHandle rate chan (Audio.Frames frame) fname hdl = do
    let info = Sndfile.hInfo hdl
        fileChan = Sndfile.channels info
    when (Sndfile.samplerate info /= rate || fileChan `notElem` [1, chan]) $
        throw $ formatError rate chan info
    when (frame > 0) $
        liftIO $ void $ Sndfile.hSeek hdl frame
    let size = fromIntegral Audio.blockSize
    Fix.fix $ \loop -> liftIO (Sndfile.hGetBuffer hdl size) >>= \case
        Nothing -> liftIO $ Sndfile.hClose hdl
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
    info <- throwEnoent fname =<< getInfo fname
    case (Audio.someNat (Sndfile.samplerate info),
            Audio.someNat (Sndfile.channels info)) of
        (TypeLits.SomeNat (_::Proxy rate), TypeLits.SomeNat (_::Proxy chan)) ->
            return
                ( Sndfile.format info
                , Audio.UnknownAudio $ read @rate @chan fname
                )

-- | Concatenate multiple files.
concat :: forall rate chan. (TypeLits.KnownNat rate, TypeLits.KnownNat chan)
    => [FilePath] -> Audio.AudioIO rate chan
concat = Audio.Audio . mconcat . map (Audio._stream . read @rate @chan)

-- | This is like 'concat', but it understands the 0-duration files written
-- by 'writeCheckpoints', and turns them back into silence.
readCheckpoints :: forall rate chan.
    (TypeLits.KnownNat rate, TypeLits.KnownNat chan)
    => Audio.Frames -> [FilePath] -> Audio.AudioIO rate chan
readCheckpoints chunkSize = Audio.Audio . go
    where
    go :: [FilePath] -> S.Stream (S.Of Audio.Block) (Resource.ResourceT IO) ()
    go [] = mempty
    go (fname : fnames) = liftIO (duration fname) >>= \case
        Nothing -> mempty
        Just dur -> (<> go fnames) $ Audio._stream $
            if dur == 0 then Audio.take chunkSize Audio.silence
            else read @rate @chan fname

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

-- * write

write :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Sndfile.Format -> FilePath -> Audio.AudioIO rate channels
    -> Resource.ResourceT IO ()
write format fname audio = do
    (key, hdl) <- Resource.allocate (openWrite format tmp audio) Sndfile.hClose
    S.mapM_ (write hdl) (Audio._stream audio)
    Resource.release key
    liftIO $ Directory.renameFile tmp fname
    where
    write hdl = liftIO
        . mapM_ (Sndfile.hPutBuffer hdl . Sndfile.Buffer.Vector.toBuffer)
        . Audio.blockSamples
    tmp = fname <> ".write.tmp"

-- | Write files in chunks to the given directory.  Run actions before
-- and after writing each chunk.
writeCheckpoints :: forall rate chan state.
    (TypeLits.KnownNat rate, TypeLits.KnownNat chan)
    => Audio.Frames
    -> (state -> IO FilePath) -- ^ get filename for this state
    -> (FilePath -> IO ()) -- ^ write state after the computation
    -> Sndfile.Format -> [state]
    -- ^ Some render-specific state for each checkpoint.  Shouldn't run out
    -- before the audio runs out.
    -> Audio.AudioIO rate chan -> Resource.ResourceT IO Int
    -- ^ number of checkpoints written
writeCheckpoints chunkSize getFilename chunkComplete format = go 0
    where
    go !written (state : states) audio = do
        fname <- liftIO $ getFilename state
        (blocks, audio) <- Audio.takeFramesGE chunkSize audio
        if null blocks
            then return chunknum
            else do
                -- The blocks should sum to 'chunkSize', except the last one,
                -- which could be smaller.  But I can't pull from 'audio'
                -- without changing the state, so I have to wait until the next
                -- loop to see if this one was short.
                Audio.assert (written `mod` chunkSize == 0) $
                    "non-final chunk was too short, expected "
                    <> pretty chunkSize <> ", but last chunk was "
                    <> pretty (written `mod` chunkSize)
                let blockCount = Num.sum $ map Audio.blockCount blocks
                -- Show the error with count, not frames, in case I somehow get
                -- an odd count.
                Audio.assert (blockCount <= sizeCount) $
                    "chunk too long, expected " <> pretty sizeCount
                    <> ", but got " <> pretty (map Audio.blockCount blocks)
                let tmp = fname ++ ".write.tmp"
                liftIO $ do
                    Exception.bracket (openWrite format tmp audio)
                        Sndfile.hClose (\hdl -> writeBlock hdl blocks)
                    Directory.renameFile tmp fname
                    chunkComplete fname
                go (written + chunkSize) states audio
        where
        chunknum = fromIntegral $ written `div` chunkSize
    go _ [] _ = Audio.throwIO "out of states"
    sizeCount = Audio.framesCount chan chunkSize
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

throwEnoent :: FilePath -> Maybe a -> IO a
throwEnoent fname =
    maybe (Audio.throwIO $ "file not found: " <> showt fname) return

openReadThrow :: FilePath -> IO Sndfile.Handle
openReadThrow fname = throwEnoent fname =<< openRead fname

openRead :: FilePath -> IO (Maybe Sndfile.Handle)
openRead fname = Sndfile.ignoreEnoent $
    Sndfile.openFile fname Sndfile.ReadMode Sndfile.defaultInfo

openWrite :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Sndfile.Format -> FilePath -> Audio.AudioIO rate channels
    -> IO Sndfile.Handle
openWrite format fname _audio = Sndfile.openFile fname Sndfile.WriteMode info
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
