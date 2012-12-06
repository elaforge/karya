-- | A midi implementation module that does nothing.
module Midi.StubMidi (initialize, interface) where
import qualified Control.Concurrent.STM as STM
import qualified Data.Time as Time

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.RealTime as RealTime


initialize :: String
    -> (Midi.Message -> Bool) -- ^ read msgs that return false are filtered
    -> (Either String (Interface.RawInterface Midi.WriteMessage) -> IO a)
    -> IO a
initialize _app_name _want_message app = app . Right =<< interface

interface :: IO (Interface.RawInterface a)
interface = do
    chan <- STM.newTChanIO
    return $ Interface.Interface
        { Interface.name = "Stub"
        , Interface.read_channel = chan
        , Interface.read_devices = return []
        , Interface.write_devices = return []
        , Interface.connect_read_device = const (return False)
        , Interface.disconnect_read_device = const (return False)
        , Interface.connect_write_device = const (return False)
        -- Return True, otherwise I get lots of spam in the logs.
        , Interface.write_message = const (return True)
        , Interface.abort = return ()
        , Interface.now = do
            t <- Time.getCurrentTime
            return $ RealTime.seconds (realToFrac (Time.utctDayTime t))
        }
