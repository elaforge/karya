-- | Test jack midi bindings, automatically and manually.
module Midi.TestJackMidi where
import qualified Control.Concurrent.STM.TChan as TChan
import Control.Monad
import qualified Control.Monad.STM as STM

import Util.Test
import qualified Midi.Interface as Interface
import qualified Midi.JackMidi as JackMidi
import qualified Midi.Midi as Midi


test_rdev = Midi.ReadDevice "IAC Driver IAC Bus 1"
test_wdev = Midi.WriteDevice "IAC Driver IAC Bus 1"

main :: IO ()
main = JackMidi.initialize "test_jack_midi" test

test :: Either String Interface.Interface -> IO ()
test (Left err) = error $ "error: " ++ err
test (Right int) = do
    print =<< Interface.now int
    rdevs <- Interface.read_devices int
    putStrLn $ "rdevs: " ++ show rdevs
    print =<< Interface.write_devices int

    mapM_ (Interface.connect_read_device int) [rdevs!!1]
    dump (Interface.read_channel int)

dump chan = forever $ do
    msg <- STM.atomically $ TChan.readTChan chan
    print msg
