-- | A midi implementation module that does nothing.
module Midi.StubMidi (
    ReadChan, ReadDeviceId, WriteDeviceId, ReadMap, WriteMap
    , initialize, get_devices

    , connect_read_device, write_message
    , abort, now
) where
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified Data.Time as Time

import qualified Midi.Midi as Midi

import qualified Perform.RealTime as RealTime


type ReadChan = STM.TChan Midi.ReadMessage
type ReadDeviceId = ()
type WriteDeviceId = ()

type ReadMap = Map.Map Midi.ReadDevice ()
type WriteMap = Map.Map Midi.WriteDevice ()

initialize :: (ReadChan -> IO a) -> IO a
initialize app = app =<< STM.newTChanIO

get_devices :: IO (ReadMap, WriteMap)
get_devices = return (Map.empty, Map.empty)

connect_read_device :: Midi.ReadDevice -> ReadDeviceId -> IO ()
connect_read_device _ _ = return ()

write_message :: WriteDeviceId -> RealTime.RealTime -> Midi.Message -> IO ()
write_message _ _ _ = return ()

abort :: IO ()
abort = return ()

now :: IO RealTime.RealTime
now = do
    t <- Time.getCurrentTime
    return $ RealTime.seconds (realToFrac (Time.utctDayTime t))
