-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.JackMidi (initialize) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception

import qualified Data.ByteString as ByteString
import qualified Data.Set as Set
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Word as Word
import Foreign.C
import Foreign hiding (void)

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified Midi.Midi as Midi
import qualified Midi.Encode as Encode
import qualified Midi.Interface as Interface
import qualified Perform.RealTime as RealTime
import Types
import Global

#include "Midi/jack.h"


type Error = Text

-- * initialize

initialize :: String -- ^ register this name with JACK
    -> (Midi.Message -> Bool) -- ^ read msgs that return false are filtered
    -> (Either Error (Interface.RawInterface Midi.WriteMessage) -> IO a)
    -> IO a
initialize app_name want_message app = do
    chan <- TChan.newTChanIO
    reads <- IORef.newIORef Set.empty
    writes <- IORef.newIORef Set.empty
    notify <- make_notify_callback (notify_callback reads writes)
    result <- withCString app_name $ \namep -> alloca $ \clientpp -> do
        statusp <- c_create_client namep notify clientpp
        clientp <- peek clientpp
        if clientp == nullPtr then Left <$> peekCString statusp
            else return $ Right $ Client clientp reads writes
    case result of
        Left err -> app (Left (txt err))
        Right client -> do
            Thread.start $ forever $ do
                msg <- read_event client
                when (want_message (Midi.rmsg_msg msg)) $
                    STM.atomically $ TChan.writeTChan chan msg
            app (Right (interface app_name client chan))
                `Exception.finally` do
                    freeHaskellFunPtr notify
                    close_client client

interface :: String -> Client -> Interface.ReadChan
  -> Interface.RawInterface Midi.WriteMessage
interface app_name client chan = Interface.Interface
    { Interface.name = "JACK"
    , Interface.read_channel = chan
    , Interface.read_devices =
        -- Ignore my own write ports.
        map (convert (Midi.read_device . txt)) . filter (not . is_local . fst)
            <$> get_ports client jackportisoutput
    , Interface.write_devices =
        -- If I don't filter local ports, I wind up seeing my own read ports.
        map (convert (Midi.write_device . txt)) . filter (not . is_local . fst)
            <$> get_ports client jackportisinput
    , Interface.connect_read_device = connect_read_device client
    , Interface.disconnect_read_device = disconnect_read_device client
    , Interface.connect_write_device = connect_write_device client
    , Interface.write_message = write_message client
    , Interface.abort = abort client
    , Interface.now = now client
    }
    where
    is_local = ((app_name ++ ":") `List.isPrefixOf`)
    convert f (port, aliases) = (f port, map f aliases)

close_client :: Client -> IO ()
close_client client = void . c_jack_client_close =<< jack_client client

foreign import ccall "create_client"
    c_create_client :: CString -> FunPtr NotifyCallback -> Ptr (Ptr CClient)
        -> IO CString
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
        rdev <- Midi.peek_rdev =<< peek portp
        time <- decode_time <$> peek timep
        return $ Midi.ReadMessage rdev time (Encode.decode bytes)

foreign import ccall "read_event"
    c_read_event :: Ptr CClient -> Ptr CString -> Ptr CJackTime
        -> Ptr (Ptr CChar) -> IO CInt

notify_callback :: IORef.IORef (Set Midi.ReadDevice)
    -> IORef.IORef (Set Midi.WriteDevice) -> NotifyCallback
notify_callback wanted_reads wanted_writes clientp namep c_is_add c_is_read =
    when (toBool c_is_add) $ if toBool c_is_read
        then notify_read =<< Midi.peek_rdev namep
        else notify_write =<< Midi.peek_wdev namep
    where
    notify_read dev = do
        b <- Set.member dev <$> IORef.readIORef wanted_reads
        when b $ void $ Concurrent.forkIO $
            void $ Midi.with_rdev dev $ \devp ->
                check ("create_read_port " <> showt dev)
                    =<< c_create_read_port clientp devp
    notify_write dev = do
        b <- Set.member dev <$> IORef.readIORef wanted_writes
        when b $ void $ Concurrent.forkIO $
            void $ Midi.with_wdev dev $ \devp ->
                check ("create_write_port " <> showt dev)
                    =<< c_create_write_port clientp devp

type NotifyCallback = Ptr CClient -> CString -> CInt -> CInt -> IO ()
-- typedef void (*NotifyCallback)(
--     Client *client, const char *port, int is_add, int is_read);
foreign import ccall "wrapper"
    make_notify_callback :: NotifyCallback -> IO (FunPtr NotifyCallback)


-- * connect / disconnect

foreign import ccall "now" c_now :: Ptr CClient -> IO CJackTime

connect_read_device :: Client -> Midi.ReadDevice -> IO Bool
connect_read_device client dev = do
    IORef.modifyIORef (client_wanted_reads client) (Set.insert dev)
    Midi.with_rdev dev $ \devp ->
        check ("create_read_port " <> showt dev)
            =<< c_create_read_port (client_ptr client) devp

foreign import ccall "create_read_port"
    c_create_read_port :: Ptr CClient -> CString -> IO CString

disconnect_read_device :: Client -> Midi.ReadDevice -> IO Bool
disconnect_read_device client dev = do
    IORef.modifyIORef (client_wanted_reads client) (Set.delete dev)
    Midi.with_rdev dev $ \devp ->
        check ("remove_read_port " <> showt dev)
            =<< c_remove_read_port (client_ptr client) devp

foreign import ccall "remove_read_port"
    c_remove_read_port :: Ptr CClient -> CString -> IO CString

connect_write_device :: Client -> Midi.WriteDevice -> IO Bool
connect_write_device client dev = do
    IORef.modifyIORef (client_wanted_writes client) (Set.insert dev)
    Midi.with_wdev dev $ \devp ->
        check ("create_write_port " <> showt dev)
            =<< c_create_write_port (client_ptr client) devp

foreign import ccall "create_write_port"
    c_create_write_port :: Ptr CClient -> CString -> IO CString


-- * write

write_message :: Client -> Midi.WriteMessage -> IO (Maybe Error)
write_message client (Midi.WriteMessage dev time msg) =
    Midi.with_wdev dev $ \devp ->
    ByteString.useAsCStringLen (Encode.encode msg) $ \(bytesp, len) ->
    error_str ("write_message " <> showt (dev, time, msg))
        =<< c_write_message (client_ptr client) devp
            (fromIntegral (RealTime.to_microseconds time))
            bytesp (fromIntegral len)

foreign import ccall "write_message"
    c_write_message :: Ptr CClient -> CString -> CJackTime -> Ptr CChar
        -> CInt -> IO CString

abort :: Client -> IO ()
abort = c_abort . client_ptr

foreign import ccall "jack_abort" c_abort :: Ptr CClient -> IO ()

-- | Get current timestamp.
now :: Client -> IO RealTime
now client = decode_time <$> c_now (client_ptr client)


-- * implementation

type CJackTime = Word.Word64
data CClient
data Client = Client {
    client_ptr :: Ptr CClient
    , client_wanted_reads :: IORef.IORef (Set Midi.ReadDevice)
    , client_wanted_writes :: IORef.IORef (Set Midi.WriteDevice)
    }

-- | This is the underlying jack_client_t, as opposed to jack.cc's Client.
data CJackClient
jack_client :: Client -> IO (Ptr CJackClient)
jack_client = (#peek Client, client) . client_ptr

decode_time :: CJackTime -> RealTime
decode_time = RealTime.microseconds . fromIntegral

-- | Get all the ports, and the aliases of each port.
get_ports :: Client -> CULong -> IO [(String, [String])]
get_ports client flags = do
    array <- c_get_midi_ports (client_ptr client) flags
    namesp <- if array == nullPtr then return [] else peekArray0 nullPtr array
    names <- mapM peekCString namesp
    -- I'm not supposed to free the port names.
    free array
    aliases <- mapM (get_port_aliases client) names
    return $ zip names aliases

foreign import ccall "get_midi_ports"
    c_get_midi_ports :: Ptr CClient -> CULong -> IO (Ptr CString)

#enum CULong, id, JackPortIsInput, JackPortIsOutput
    -- Avoid unused warnings for these.
    -- JackPortIsPhysical, JackPortCanMonitor, JackPortIsTerminal

get_port_aliases :: Client -> String -> IO [String]
get_port_aliases client name = do
    alloca $ \alias1p -> alloca $ \alias2p -> withCString name $ \namep -> do
        check ("get_port_aliases " <> showt name)
            =<< c_get_port_aliases (client_ptr client) namep alias1p alias2p
        (++) <$> maybe_peek alias1p <*> maybe_peek alias2p
    where
    maybe_peek pp = do
        p <- peek pp
        if p == nullPtr then return []
            else (:[]) <$> peekCString p

foreign import ccall "get_port_aliases"
    c_get_port_aliases :: Ptr CClient -> CString -> Ptr CString -> Ptr CString
        -> IO CString

-- | Log any error and return False if there was one.
check :: Text -> CString -> IO Bool
check msg errp = error_str msg errp >>= \case
    Nothing -> return True
    Just msg -> do
        Log.error msg
        return False

error_str :: Text -> CString -> IO (Maybe Error)
error_str msg errp
    | errp == nullPtr = return Nothing
    | otherwise = do
        error_msg <- peekCString errp
        return $ Just $ "JACK error: " <> msg <> ": " <> txt error_msg
