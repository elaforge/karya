-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Wrapper around PortAudio.  This only supports output.
module Util.Audio.PortAudio (
    initialize
    , play
    -- * Device
    , Device(..)
    , getDefaultOutput
    , getOutputDevices
    -- * Error
    , Error(..)
) where
import qualified Bindings.PortAudio as B
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Vector.Storable as Vector
import qualified Foreign
import qualified Foreign.C as C
import           GHC.TypeLits (KnownNat)

import qualified Util.Audio.Audio as Audio

import           Global


initialize :: IO a -> IO a
initialize =
    Exception.bracket_ (check B.c'Pa_Initialize) (check B.c'Pa_Terminate)

play :: forall rate chan. (KnownNat rate, KnownNat chan) => IO Bool
    -> Device -> Audio.AudioIO rate chan -> Resource.ResourceT IO ()
play pollQuit device audio = do
    -- I could use bracket, but I'd need the one from 'exceptions'.
    (streamK, stream) <- Resource.allocate
        (open (_info device)) (check . B.c'Pa_CloseStream)
    (startK, _) <- Resource.allocate
        (check $ B.c'Pa_StartStream stream)
        (\_ -> check $ B.c'Pa_StopStream stream)
    write pollQuit stream framesPerBuffer audio
    Resource.release startK
    Resource.release streamK
    where
    flags = 0 -- paClipOff?
    framesPerBuffer = 1024
    open devInfo = Foreign.alloca $ \streamp ->
        Foreign.with (outputParams devInfo) $ \outputParamsp -> do
            check $ B.c'Pa_OpenStream
                streamp
                Foreign.nullPtr
                outputParamsp
                (fromIntegral rate)
                (fromIntegral framesPerBuffer)
                flags
                -- No callback, use blocking API.  It's more convenient, and
                -- latency seems to be low enough.
                Foreign.nullFunPtr
                Foreign.nullPtr
            Foreign.peek streamp
    outputParams devInfo = B.C'PaStreamParameters
        { c'PaStreamParameters'device = _id device
        , c'PaStreamParameters'channelCount = fromIntegral chan
        -- #define  paFloat32   ((PaSampleFormat) 0x00000001)
        , c'PaStreamParameters'sampleFormat = 1
        -- High latency since it's the blocking API.
        , c'PaStreamParameters'suggestedLatency =
            B.c'PaDeviceInfo'defaultHighOutputLatency devInfo
        , c'PaStreamParameters'hostApiSpecificStreamInfo = Foreign.nullPtr
        }
    chan = Audio.natVal (Proxy :: Proxy chan)
    rate = Audio.natVal (Proxy :: Proxy rate)

write :: forall m rate chan. (MonadIO m, KnownNat chan)
    => IO Bool -> Foreign.Ptr B.C'PaStream -> Audio.Frames
    -> Audio.Audio m rate chan -> m ()
write pollQuit stream framesPerBuffer = go
    where
    -- go audio = Audio.next audio >>= \case
    --     Nothing -> return ()
    --     Just (block, audio) -> do
    --         liftIO $ mapM_ writeVector (Audio.blockSamples block)
    --         go audio
    go audio = liftIO pollQuit >>= \case
        True -> return ()
        False -> do
            (blocks, audio) <- Audio.splitAt framesPerBuffer audio
            if null blocks then return () else do
                -- I could probably mapm_ writeVector, but this gives
                -- predictable interrupt time and is the size I promised
                -- Pa_OpenStream.
                let samples = mconcat $ concatMap Audio.blockSamples blocks
                liftIO $ writeVector samples
                go audio
    writeVector v = Vector.unsafeWith v $ \vp ->
        check $ B.c'Pa_WriteStream stream (Foreign.castPtr vp)
            (fromIntegral (Audio.vectorFrames chan v))
    chan = Proxy :: Proxy chan

-- * Device

data Device = Device {
    _name :: String
    , _id :: C.CInt
    , _info :: B.C'PaDeviceInfo
    } deriving (Show)

isOutput :: Device -> Bool
isOutput = (>0) . B.c'PaDeviceInfo'maxOutputChannels . _info

type DeviceInfo = B.C'PaDeviceInfo

getDefaultOutput :: IO Device
getDefaultOutput = getDevice =<< B.c'Pa_GetDefaultOutputDevice

getOutputDevices:: IO [Device]
getOutputDevices= do
    count <- B.c'Pa_GetDeviceCount
    filter isOutput <$> mapM getDevice [0 .. count - 1]

getDevice :: C.CInt -> IO Device
getDevice id = do
    info <- getDeviceInfo id
    name <- C.peekCAString $ B.c'PaDeviceInfo'name info
    return $ Device { _name = name, _id = id, _info = info }

getDeviceInfo :: C.CInt -> IO DeviceInfo
getDeviceInfo = maybe (Exception.throwIO InvalidDevice) return <=< lookupDevice

lookupDevice :: C.CInt -> IO (Maybe DeviceInfo)
lookupDevice deviceId =
    Foreign.maybePeek Foreign.peek =<< B.c'Pa_GetDeviceInfo deviceId


-- * Error

check :: MonadIO m => IO C.CInt -> m ()
check action = liftIO $ do
  err <- action
  unless (err == 0) $ Exception.throwIO $ fromErrorCode err

data Error = NotInitialized
    | UnanticipatedHostError
    | InvalidChannelCount
    | InvalidSampleRate
    | InvalidDevice
    | InvalidFlag
    | SampleFormatNotSupported
    | BadIODeviceCombination
    | InsufficientMemory
    | BufferTooBig
    | BufferTooSmall
    | NullCallback
    | BadStreamPtr
    | TimedOut
    | InternalError
    | DeviceUnavailable
    | IncompatibleHostApiSpecificStreamInfo
    | StreamIsStopped
    | StreamIsNotStopped
    | InputOverflowed
    | OutputUnderflowed
    | HostApiNotFound
    | InvalidHostApi
    | CanNotReadFromACallbackStream
    | CanNotWriteToACallbackStream
    | CanNotReadFromAnOutputOnlyStream
    | CanNotWriteToAnInputOnlyStream
    | IncompatibleStreamHostApi
    | BadBufferPtr
    deriving (Show, Eq, Ord, Enum)

instance Exception.Exception Error

fromErrorCode :: C.CInt -> Error
fromErrorCode n = toEnum (fromIntegral n + 10000)
