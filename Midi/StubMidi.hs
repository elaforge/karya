-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A midi implementation module that does nothing.
module Midi.StubMidi (initialize, interface) where
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.RealTime as RealTime


initialize :: String
    -> (Midi.Message -> Bool) -- ^ read msgs that return false are filtered
    -> (Either Text.Text (Interface.RawInterface Midi.WriteMessage) -> IO a)
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
        -- Return no error, otherwise I get lots of spam in the logs.
        , Interface.write_message = const (return Nothing)
        , Interface.abort = return ()
        , Interface.now = do
            t <- Time.getCurrentTime
            return $ RealTime.seconds (realToFrac (Time.utctDayTime t))
        }
