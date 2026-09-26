-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A midi implementation module that does nothing.
module Midi.StubMidi (initialize, interface) where
import qualified Control.Concurrent.STM as STM
import qualified Data.Time as Time

import qualified Midi.Interface as Interface
import qualified Perform.RealTime as RealTime


initialize :: Interface.Initialize a
initialize _appName _wantMessage app = app . Right =<< interface

interface :: IO (Interface.RawInterface a)
interface = do
    chan <- STM.newTChanIO
    pure $ Interface.Interface
        { Interface.name = "Stub"
        , Interface.readChannel = chan
        , Interface.readDevices = pure []
        , Interface.writeDevices = pure []
        , Interface.connectReadDevice = const (pure False)
        , Interface.disconnectReadDevice = const (pure False)
        , Interface.connectWriteDevice = const (pure False)
        -- Return no error, otherwise I get lots of spam in the logs.
        , Interface.writeMessage = const (pure Nothing)
        , Interface.abort = pure ()
        , Interface.now = do
            t <- Time.getCurrentTime
            pure $ RealTime.seconds (realToFrac (Time.utctDayTime t))
        }
