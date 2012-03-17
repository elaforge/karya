{-# LANGUAGE ForeignFunctionInterface #-}
module Midi.JackMidi where
import Control.Applicative ((<$>))
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Data.IORef as IORef
import qualified Data.Word as Word
import Foreign.C
import Foreign

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified Midi.Midi as Midi
import qualified Midi.Parse as Parse
import qualified Midi.Interface as Interface
import qualified Perform.RealTime as RealTime
import Types

#include "Midi/jack.h"


initialize :: String -> (Either String Interface.Interface -> IO a) -> IO a
initialize app_name app = do
    chan <- TChan.newTChanIO
    result <- withCString app_name $ \namep -> alloca $ \clientpp -> do
        statusp <- c_create_client namep clientpp
        clientp <- peek clientpp
        if clientp == nullPtr then Left <$> peekCString statusp
            else do
                read <- IORef.newIORef Set.empty
                write <- IORef.newIORef Set.empty
                return $ Right $ Client clientp read write
    case result of
        Left err -> app (Left err)
        Right client -> do
            Thread.start $ forever $ do
                event <- read_event client
                STM.atomically $ TChan.writeTChan chan event
            app (Right (interface client chan))
                `Exception.finally` close_client client

interface :: Client -> Interface.ReadChan -> Interface.Interface
interface client chan = Interface.Interface
    { Interface.name = "JACK"
    , Interface.read_channel = chan
    , Interface.read_devices =
        map Midi.ReadDevice <$> get_ports client jackportisoutput
    , Interface.write_devices =
        map Midi.WriteDevice <$> get_ports client jackportisinput
    , Interface.connect_read_device = connect_read_device client
    , Interface.disconnect_read_device = disconnect_read_device client
    , Interface.connect_write_device = connect_write_device client
    , Interface.write_message = undefined
    , Interface.abort = undefined
    , Interface.now = now client
    }

close_client :: Client -> IO ()
close_client client =
    Control.Monad.void . c_jack_client_close =<< jack_client client

foreign import ccall "create_client"
    c_create_client :: CString -> Ptr (Ptr CClient) -> IO CString
foreign import ccall "jack_client_close"
    c_jack_client_close :: Ptr CJackClient -> IO CInt

read_event :: Client -> IO Midi.ReadMessage
read_event client = do
    alloca $ \portp -> alloca $ \timep -> alloca $ \eventp -> do
        -- The storage is static inside the C function, so I don't need to
        -- worry about deallocating anything here.
        size <- c_read_event (client_ptr client) portp timep eventp
        bytesp <- peek eventp
        bytes <- ByteString.packCStringLen (bytesp, fromIntegral size)
        rdev <- Midi.ReadDevice <$> (peekCString =<< peek portp)
        time <- decode_time <$> peek timep
        return $ Midi.ReadMessage rdev time (Parse.decode bytes)


foreign import ccall "read_event"
    c_read_event :: Ptr CClient -> Ptr CString -> Ptr CJackTime
        -> Ptr (Ptr CChar) -> IO CInt

-- | Get current timestamp.
now :: Client -> IO RealTime
now client = decode_time <$> c_now (client_ptr client)

foreign import ccall "now" c_now :: Ptr CClient -> IO CJackTime

connect_read_device :: Client -> Midi.ReadDevice -> IO Bool
connect_read_device client dev@(Midi.ReadDevice name) = do
    IORef.modifyIORef (client_wanted_reads client) (Set.insert dev)
    withCString name $ \namep ->
        check =<< c_create_read_port (client_ptr client) namep

foreign import ccall "create_read_port"
    c_create_read_port :: Ptr CClient -> CString -> IO CString

disconnect_read_device :: Client -> Midi.ReadDevice -> IO Bool
disconnect_read_device client dev@(Midi.ReadDevice name) = do
    IORef.modifyIORef (client_wanted_reads client) (Set.delete dev)
    withCString name $ \namep ->
        check =<< c_remove_read_port (client_ptr client) namep

foreign import ccall "remove_read_port"
    c_remove_read_port :: Ptr CClient -> CString -> IO CString

-- Create a port for 'dev'
connect_write_device :: Client -> Midi.WriteDevice -> IO Bool
connect_write_device client msg = undefined



-- * implementation

type CJackTime = Word.Word64
data CClient
data Client = Client {
    client_ptr :: Ptr CClient
    , client_wanted_reads :: IORef.IORef (Set.Set Midi.ReadDevice)
    , client_wanted_writes :: IORef.IORef (Set.Set Midi.WriteDevice)
    }

-- | This is the underlying jack_client_t, as opposed to jack.cc's Client.
data CJackClient
jack_client :: Client -> IO (Ptr CJackClient)
jack_client = (#peek Client, client) . client_ptr

decode_time :: CJackTime -> RealTime
decode_time = RealTime.microseconds . fromIntegral

get_ports :: Client -> CULong -> IO [String]
get_ports client flags = do
    array <- c_get_midi_ports (client_ptr client) flags
    namesp <- if array == nullPtr then return [] else peekArray0 nullPtr array
    names <- mapM peekCString namesp
    -- I'm not supposed to free the port names.
    free array
    return names

foreign import ccall "get_midi_ports"
    c_get_midi_ports :: Ptr CClient -> CULong -> IO (Ptr CString)

#enum CULong, id, JackPortIsInput, JackPortIsOutput, JackPortIsPhysical, \
    JackPortCanMonitor, JackPortIsTerminal

-- | Log any error and return False if there was one.
check :: CString -> IO Bool
check err
    | err == nullPtr = return True
    | otherwise = do
        msg <- peekCString err
        Log.error $ "JACK error: " ++ msg
        return False
