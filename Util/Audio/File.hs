-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
-- | Functions to read and write audio files.  This should be able to read all
-- formats supported by libsndfile.
module Util.Audio.File (
    -- * read
    check, checkA, read, readFrom, read44k
    -- * write
    , write, writeCheckpoints
    , wavFormat
) where
import Prelude hiding (read)
import qualified Control.Exception as Exception
import qualified Control.Monad.Fix as Fix
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Vector.Storable as V
import qualified GHC.TypeLits as TypeLits
import qualified Sound.File.Sndfile as Sndfile
import qualified Sound.File.Sndfile.Buffer.Vector as Sndfile.Buffer.Vector
import qualified Streaming.Prelude as S
import qualified System.IO.Error as IO.Error

import qualified Util.Audio.Audio as Audio
import Global


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
read :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    FilePath -> Audio.AudioIO rate channels
read = readFrom (Audio.Frames 0)

readFrom :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels) =>
    Audio.Duration -> FilePath -> Audio.AudioIO rate channels
readFrom (Audio.Seconds secs) fname =
    readFrom (Audio.Frames (Audio.secondsToFrame rate secs)) fname
    where rate = fromIntegral $ TypeLits.natVal (Proxy :: Proxy rate)
readFrom (Audio.Frames (Audio.Frame frame)) fname = Audio.Audio $ do
    (key, handle) <- lift $
        Resource.allocate (openRead fname) Sndfile.hClose
    whenJust (checkInfo rate channels (Sndfile.hInfo handle)) throw
    when (frame > 0) $
        liftIO $ void $ Sndfile.hSeek handle Sndfile.AbsoluteSeek frame
    Fix.fix $ \loop ->
        liftIO (Sndfile.hGetBuffer handle size) >>= \case
            Nothing -> lift (Resource.release key) >> return ()
            Just buf -> do
                let chunk = Sndfile.Buffer.Vector.fromBuffer buf
                -- Sndfile should enforce this, but let's be sure.
                when (V.length chunk `mod` chan /= 0) $
                    throw $ "chunk length " <> show (V.length chunk)
                        <> " not a multiple of channels " <> show chan
                S.yield chunk
                loop
    where
    size = Audio.framesCount channels Audio.chunkSize
    rate = Proxy :: Proxy rate
    channels = Proxy :: Proxy channels
    chan = fromIntegral $ TypeLits.natVal channels
    throw msg = liftIO $
        Exception.throwIO $ IO.Error.mkIOError IO.Error.userErrorType
            ("reading file: " <> msg) Nothing (Just fname)

read44k :: FilePath -> Audio.AudioIO 44100 2
read44k = read

checkInfo :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Proxy rate -> Proxy channels -> Sndfile.Info -> Maybe String
checkInfo rate_ channels_ info
    | int (Sndfile.samplerate info) == rate
            && int (Sndfile.channels info) == channels = Nothing
    | otherwise = Just $
        "(rate, channels) " ++ show (rate, channels) ++ " /= "
            ++ show (Sndfile.samplerate info, Sndfile.channels info)
    where
    int = fromIntegral
    rate = TypeLits.natVal rate_
    channels = TypeLits.natVal channels_

openRead :: FilePath -> IO Sndfile.Handle
openRead fname = annotate fname $
    Sndfile.openFile fname Sndfile.ReadMode Sndfile.defaultInfo

-- Sndfile's errors don't include the filename.
annotate :: FilePath -> IO a -> IO a
annotate fname = Exception.handle $ \exc -> Exception.throwIO $
    exc { Sndfile.errorString = fname <> ": " <> Sndfile.errorString exc }

-- * write

write :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Sndfile.Format -> FilePath -> Audio.AudioIO rate channels
    -> Resource.ResourceT IO ()
write format fname audio = do
    (key, handle) <- Resource.allocate (openWrite format fname audio)
        Sndfile.hClose
    S.mapM_ (liftIO . Sndfile.hPutBuffer handle
            . Sndfile.Buffer.Vector.toBuffer)
        (Audio._stream audio)
    Resource.release key

-- | Write files in chunks to the given directory.  Run an action before
-- writing each chunk.  It's expected to query and save audio generator state.
writeCheckpoints :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Audio.Frame -> (FilePath -> IO ()) -- ^ run before rendering each chunk
    -> Sndfile.Format -> [FilePath] -- ^ should be infinite
    -> Audio.AudioIO rate channels
    -> Resource.ResourceT IO ()
writeCheckpoints size checkpoint format = go
    where
    go (fname : fnames) audio = do
        liftIO (checkpoint fname)
        (chunks, audio) <- Audio.takeFrames size audio
        liftIO $ Exception.bracket (openWrite format fname audio) Sndfile.hClose
            (\handle -> mapM_
                (Sndfile.hPutBuffer handle . Sndfile.Buffer.Vector.toBuffer)
                chunks)
        unless (null chunks) $
            go fnames audio
    go [] _ = liftIO $ Exception.throwIO $ Audio.Exception "out of fnames"

openWrite :: forall rate channels.
    (TypeLits.KnownNat rate, TypeLits.KnownNat channels)
    => Sndfile.Format -> FilePath -> Audio.AudioIO rate channels
    -> IO Sndfile.Handle
openWrite format fname _audio =
    annotate fname $ Sndfile.openFile fname Sndfile.WriteMode info
    where
    info = Sndfile.defaultInfo
        { Sndfile.samplerate = fromIntegral $
            TypeLits.natVal (Proxy :: Proxy rate)
        , Sndfile.channels = fromIntegral $
            TypeLits.natVal (Proxy :: Proxy channels)
        , Sndfile.format = format
        }

wavFormat :: Sndfile.Format
wavFormat = Sndfile.Format
    { headerFormat = Sndfile.HeaderFormatWav
    , sampleFormat = Sndfile.SampleFormatFloat
    , endianFormat = Sndfile.EndianFile
    }
