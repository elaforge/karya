module Midi.TestCoreMidi where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map

import qualified Midi.Midi as Midi
import qualified Midi.CoreMidi as CoreMidi

import Util.PPrint
import qualified Perform.Timestamp as Timestamp


main = CoreMidi.initialize test

test chan = do
    (rdevs, wdevs) <- CoreMidi.get_devices
    pprint rdevs
    pprint wdevs
    let out_dev = (Map.keys wdevs) !! 1

    putStrLn "connect"
    mapM_ (uncurry CoreMidi.connect_read_device) (drop 2 (Map.toList rdevs))

    let write_msg = make_write_msg wdevs

    -- CoreMidi.write_message out_dev Timestamp.immediately
    --     (Midi.ChannelMessage 0 (Midi.NoteOn 74 66))
    -- write_sysex (iac 4) write_msg
    -- test_merge write_msg out_dev chan
    thru_loop write_msg out_dev chan

iac n = Midi.WriteDevice ("IAC Driver IAC Bus " ++ show n)

test_merge write_msg out_dev read_chan = do
    Midi.ReadMessage _ (Timestamp.Timestamp ts) _
        <- STM.atomically (STM.readTChan read_chan)
    play_melody ts write_msg out_dev
    thru_melody write_msg out_dev

make_write_msg wdevs (dev, ts, msg) = do
    putStrLn $ "wr: " ++ show (dev, ts, msg)
    CoreMidi.write_message (wdevs Map.! dev) ts msg

thru_loop write_msg wdev read_chan = forever $ do
    Midi.ReadMessage dev ts msg <- STM.atomically (STM.readTChan read_chan)
    print (ts, dev, msg)
    let msgs = thru (ts, msg)
    sequence_ [write_msg (wdev, ts, msg) | (ts, msg) <- msgs]

thru (ts, msg) = case msg of
    Midi.ChannelMessage ch (Midi.NoteOn key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOn key vel))]

    Midi.ChannelMessage ch (Midi.NoteOff key vel)
        -> [(ts1, Midi.ChannelMessage ch (Midi.NoteOff key vel))]
    _ -> []
    where
    ts1 = ts -- + Timestamp.Timestamp 500

write_sysex wdev write_msg = do
    write_msg (wdev, Timestamp.immediately, big_sysex)

big_sysex = Midi.CommonMessage
    (Midi.SystemExclusive 42 (take 10000 (cycle [0..9]) ++ [0xf7]))


thru_melody write_msg out_dev = do
    let play msg = write_msg (mknote out_dev (0, msg))
    let notes = [70, 68 .. 60]
    forM_ (zip (notes++[0]) (head notes : notes)) $ \(key, last) -> do
        play (Midi.NoteOff last 0)
        when (key /= 0) $
            play (Midi.NoteOn key 100)
        Concurrent.threadDelay (4 * 100000)

play_melody ts_offset write_msg wdev = mapM_ write_msg (melody ts_offset wdev)

melody ts_offset wdev = map (mknote wdev) $ concat
    [[(ts, Midi.NoteOn key vel), (ts+400, Midi.NoteOff key vel)]
    | (ts, key) <- zip (map (+ts_offset) [0, 500..]) score]
    where
    vel = 70
    score = [53, 55 .. 61]

mknote wdev (ts, msg) =
    (wdev, Timestamp.Timestamp ts, Midi.ChannelMessage 0 msg)
