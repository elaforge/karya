-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.JackMidi (initialize) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Word as Word
import qualified Foreign
import           Foreign (FunPtr, Ptr, alloca, peek, peekByteOff, nullPtr)
import           Foreign.C
    (CChar(..), CInt(..), CString, CULong(..), peekCString)
import qualified Foreign.C as C

import qualified Midi.Encode as Encode
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.RealTime as RealTime
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import           Global

#include "Midi/jack.h"


type Error = Text

-- * initialize

initialize :: Interface.Initialize a
initialize appName wantMessage app = do
    chan <- TChan.newTChanIO
    reads <- IORef.newIORef Set.empty
    writes <- IORef.newIORef Set.empty
    notify <- makeNotifyCallback (notifyCallback reads writes)
    result <- C.withCString appName $ \namep -> alloca $ \clientpp -> do
        statusp <- cCreateClient namep notify clientpp
        clientp <- peek clientpp
        if clientp == nullPtr then Left <$> peekCString statusp
            else return $ Right $ Client clientp reads writes
    case result of
        Left err -> app (Left (txt err))
        Right client -> do
            Thread.start $ forever $
                whenJustM (readEvent client wantMessage) $ \msg ->
                    STM.atomically $ TChan.writeTChan chan msg
            app (Right (interface appName client chan))
                `Exception.finally` do
                    Foreign.freeHaskellFunPtr notify
                    closeClient client

interface :: String -> Client -> Interface.ReadChan
  -> Interface.RawInterface Midi.WriteMessage
interface appName client chan = Interface.Interface
    { name = "JACK"
    , readChannel = chan
    , readDevices =
        -- Ignore my own write ports.
        map (convert (Midi.readDevice . txt)) . filter (not . isLocal . fst)
            <$> getPorts client jackportisoutput
    , writeDevices =
        -- If I don't filter local ports, I wind up seeing my own read ports.
        map (convert (Midi.writeDevice . txt)) . filter (not . isLocal . fst)
            <$> getPorts client jackportisinput
    , connectReadDevice = connectReadDevice client
    , disconnectReadDevice = disconnectReadDevice client
    , connectWriteDevice = connectWriteDevice client
    , writeMessage = writeMessage client
    , abort = abort client
    , now = now client
    }
    where
    isLocal = ((appName ++ ":") `List.isPrefixOf`)
    convert f (port, aliases) = (f port, map f aliases)

closeClient :: Client -> IO ()
closeClient client = void . cJackClientClose =<< jackClient client

foreign import ccall "create_client"
    cCreateClient :: CString -> FunPtr NotifyCallback -> Ptr (Ptr CClient)
        -> IO CString
foreign import ccall "jack_client_close"
    cJackClientClose :: Ptr CJackClient -> IO CInt

readEvent :: Client -> Interface.WantMessage -> IO (Maybe Midi.ReadMessage)
readEvent client wantMessage =
    alloca $ \portp -> alloca $ \timep -> alloca $ \eventp -> do
        -- The storage is static inside the C function, so I don't need to
        -- worry about deallocating anything here.
        size <- cReadEvent (clientPtr client) portp timep eventp
        bytesp <- peek eventp
        bytes <- ByteString.packCStringLen (bytesp, fromIntegral size)
        if wantMessage bytes
            then return Nothing
            else do
                rdev <- Midi.peekRdev =<< peek portp
                time <- decodeTime <$> peek timep
                return $ Just $ Midi.ReadMessage rdev time (Encode.decode bytes)

foreign import ccall "read_event"
    cReadEvent :: Ptr CClient -> Ptr CString -> Ptr CJackTime
        -> Ptr (Ptr CChar) -> IO CInt

notifyCallback :: IORef.IORef (Set Midi.ReadDevice)
    -> IORef.IORef (Set Midi.WriteDevice) -> NotifyCallback
notifyCallback wantedReads wantedWrites clientp namep cIsAdd cIsRead =
    when (Foreign.toBool cIsAdd) $ if Foreign.toBool cIsRead
        then notifyRead =<< Midi.peekRdev namep
        else notifyWrite =<< Midi.peekWdev namep
    where
    notifyRead dev = do
        b <- Set.member dev <$> IORef.readIORef wantedReads
        when b $ void $ Concurrent.forkIO $
            void $ Midi.withRdev dev $ \devp ->
                check ("create_read_port " <> showt dev)
                    =<< cCreateReadPort clientp devp
    notifyWrite dev = do
        b <- Set.member dev <$> IORef.readIORef wantedWrites
        when b $ void $ Concurrent.forkIO $
            void $ Midi.withWdev dev $ \devp ->
                check ("create_write_port " <> showt dev)
                    =<< cCreateWritePort clientp devp

type NotifyCallback = Ptr CClient -> CString -> CInt -> CInt -> IO ()
-- typedef void (*NotifyCallback)(
--     Client *client, const char *port, int is_add, int is_read);
foreign import ccall "wrapper"
    makeNotifyCallback :: NotifyCallback -> IO (FunPtr NotifyCallback)


-- * connect / disconnect

foreign import ccall "now" cNow :: Ptr CClient -> IO CJackTime

connectReadDevice :: Client -> Midi.ReadDevice -> IO Bool
connectReadDevice client dev = do
    IORef.modifyIORef (clientWantedReads client) (Set.insert dev)
    Midi.withRdev dev $ \devp ->
        check ("create_read_port " <> showt dev)
            =<< cCreateReadPort (clientPtr client) devp

foreign import ccall "create_read_port"
    cCreateReadPort :: Ptr CClient -> CString -> IO CString

disconnectReadDevice :: Client -> Midi.ReadDevice -> IO Bool
disconnectReadDevice client dev = do
    IORef.modifyIORef (clientWantedReads client) (Set.delete dev)
    Midi.withRdev dev $ \devp ->
        check ("remove_read_port " <> showt dev)
            =<< cRemoveReadPort (clientPtr client) devp

foreign import ccall "remove_read_port"
    cRemoveReadPort :: Ptr CClient -> CString -> IO CString

connectWriteDevice :: Client -> Midi.WriteDevice -> IO Bool
connectWriteDevice client dev = do
    IORef.modifyIORef (clientWantedWrites client) (Set.insert dev)
    Midi.withWdev dev $ \devp ->
        check ("create_write_port " <> showt dev)
            =<< cCreateWritePort (clientPtr client) devp

foreign import ccall "create_write_port"
    cCreateWritePort :: Ptr CClient -> CString -> IO CString


-- * write

writeMessage :: Client -> Midi.WriteMessage -> IO (Maybe Error)
writeMessage client (Midi.WriteMessage dev time msg)
    | not (Midi.validMsg msg) = return $ Just $ "invalid msg: " <> showt msg
    | otherwise =  Midi.withWdev dev $ \devp ->
        ByteString.useAsCStringLen (Encode.encode msg) $ \(bytesp, len) ->
        errorStr ("write_message " <> showt (dev, time, msg))
            =<< cWriteMessage (clientPtr client) devp
                (fromIntegral (RealTime.toMicroseconds time))
                bytesp (fromIntegral len)

foreign import ccall "write_message"
    cWriteMessage :: Ptr CClient -> CString -> CJackTime -> Ptr CChar
        -> CInt -> IO CString

abort :: Client -> IO ()
abort = cAbort . clientPtr

foreign import ccall "jack_abort" cAbort :: Ptr CClient -> IO ()

-- | Get current timestamp.
now :: Client -> IO RealTime.RealTime
now client = decodeTime <$> cNow (clientPtr client)


-- * implementation

type CJackTime = Word.Word64
data CClient
data Client = Client
    { clientPtr :: Ptr CClient
    , clientWantedReads :: IORef.IORef (Set Midi.ReadDevice)
    , clientWantedWrites :: IORef.IORef (Set Midi.WriteDevice)
    }

-- | This is the underlying jack_client_t, as opposed to jack.cc's Client.
data CJackClient
jackClient :: Client -> IO (Ptr CJackClient)
jackClient = (#peek Client, client) . clientPtr

decodeTime :: CJackTime -> RealTime.RealTime
decodeTime = RealTime.microseconds . fromIntegral

-- | Get all the ports, and the aliases of each port.
getPorts :: Client -> CULong -> IO [(String, [String])]
getPorts client flags = do
    array <- cGetMidiPorts (clientPtr client) flags
    namesp <- if array == nullPtr
        then return [] else Foreign.peekArray0 nullPtr array
    names <- mapM peekCString namesp
    -- I'm not supposed to free the port names.
    Foreign.free array
    aliases <- mapM (getPortAliases client) names
    return $ zip names aliases

foreign import ccall "get_midi_ports"
    cGetMidiPorts :: Ptr CClient -> CULong -> IO (Ptr CString)

#enum CULong, id, JackPortIsInput, JackPortIsOutput
    -- Avoid unused warnings for these.
    -- JackPortIsPhysical, JackPortCanMonitor, JackPortIsTerminal

getPortAliases :: Client -> String -> IO [String]
getPortAliases client name = do
    alloca $ \alias1p -> alloca $ \alias2p -> C.withCString name $ \namep -> do
        check ("get_port_aliases " <> showt name)
            =<< cGetPortAliases (clientPtr client) namep alias1p alias2p
        (++) <$> maybePeek alias1p <*> maybePeek alias2p
    where
    maybePeek pp = do
        p <- peek pp
        if p == nullPtr then return []
            else (:[]) <$> peekCString p

foreign import ccall "get_port_aliases"
    cGetPortAliases :: Ptr CClient -> CString -> Ptr CString -> Ptr CString
        -> IO CString

-- | Log any error and return False if there was one.
check :: Text -> CString -> IO Bool
check msg errp = errorStr msg errp >>= \case
    Nothing -> return True
    Just msg -> do
        Log.error msg
        return False

errorStr :: Text -> CString -> IO (Maybe Error)
errorStr msg errp
    | errp == nullPtr = return Nothing
    | otherwise = do
        errorMsg <- peekCString errp
        return $ Just $ "JACK error: " <> msg <> ": " <> txt errorMsg
