-- | A midi implementation module that does nothing.
module Midi.StubMidi (initialize, interface) where
import qualified Control.Concurrent.STM as STM
import qualified Data.Time as Time

import qualified Midi.Interface as Interface
import qualified Perform.RealTime as RealTime


initialize :: String -> (Either String Interface.Interface -> IO a) -> IO a
initialize _app_name app = app . Right =<< interface

interface :: IO Interface.Interface
interface = do
    chan <- STM.newTChanIO
    return $ Interface.Interface
        { Interface.read_channel = chan
        , Interface.read_devices = return []
        , Interface.write_devices = return []
        , Interface.connect_read_device = const (return False)
        , Interface.disconnect_read_device = const (return False)
        , Interface.connect_write_device = const (return False)
        , Interface.write_message = const (return False)
        , Interface.abort = return ()
        , Interface.now = do
            t <- Time.getCurrentTime
            return $ RealTime.seconds (realToFrac (Time.utctDayTime t))
        }
