-- | Test core midi bindings, automatically and manually.
module Midi.TestCoreMidi where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified System.Environment

import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Midi as Midi
import qualified Midi.CoreMidi as CoreMidi

import qualified Perform.RealTime as RealTime


test_rdev = Midi.ReadDevice "IAC Driver IAC Bus 1"
test_wdev = Midi.WriteDevice "IAC Driver IAC Bus 1"

main = CoreMidi.initialize test

type ReadMsg = IO (Maybe Midi.ReadMessage)
type WriteMsg = (RealTime.RealTime, Midi.Message) -> IO ()

test :: CoreMidi.ReadChan -> IO ()
test read_chan = do
    (rdev_map, wdev_map) <- CoreMidi.get_devices
    putStrLn "read devs:"
    pprint (Map.assocs rdev_map)
    putStrLn "write devs:"
    pprint (Map.assocs wdev_map)

    let open_devs blocking rdevs maybe_wdev = do
        forM_ rdevs $ \rdev -> case Map.lookup rdev rdev_map of
            Nothing -> error $ "required rdev " ++ show rdev ++ " not found"
            Just devid -> CoreMidi.connect_read_device rdev devid
        let read_msg = (if blocking then blocking_get else nonblocking_get)
                read_chan
        case maybe_wdev of
            Nothing -> return
                (const (error "write device not opened"), read_msg)
            Just wdev -> do
                when (wdev `Map.notMember` wdev_map) $
                    error $ "required wdev " ++ show wdev ++ " not found"
                return (make_write_msg wdev wdev_map, read_msg)

    let all_rdevs = Map.keys rdev_map
    args <- System.Environment.getArgs
    case args of
        [] -> do
            putStrLn "monitoring (pass arg 'help' for help)"
            (_, read_msg) <- open_devs True all_rdevs Nothing
            monitor read_msg
        ["help"] -> putStrLn usage
        ["thru", out_dev] -> do
            putStrLn "playing thru"
            (write_msg, read_msg) <- open_devs True
                all_rdevs (Just (Midi.WriteDevice out_dev))
            thru_loop write_msg read_msg
        ["melody", out_dev] -> do
            putStrLn "playing melody + thru"
            (write_msg, read_msg) <- open_devs True
                all_rdevs (Just (Midi.WriteDevice out_dev))
            thru_melody write_msg read_msg
        ["spam", out_dev, n_str] -> do
            putStrLn $ "spamming " ++ n_str ++ " msgs"
            (write_msg, _) <- open_devs True
                all_rdevs (Just (Midi.WriteDevice out_dev))
            n <- readIO n_str
            spam write_msg n
            getChar
            return ()
        ["test"] -> do
            putStrLn "testing"
            (write_msg, read_msg) <- open_devs False
                [test_rdev] (Just test_wdev)
            run_tests write_msg read_msg
        _ -> do
            putStrLn "unknown command"
            putStrLn usage

usage = unlines
    [ "(no arg)     monitor all inputs"
    , "help         print this usage"
    , "thru <out>   msgs from any input are relayed to <out>"
    , "melody <out> play a melody on <out>, also relaying msgs thru"
    , "spam <out> n spam <out> with 'n' msgs in rapid succession"
    , "test         run some semi-automatic tests"
    ]


-- * monitor

monitor :: ReadMsg -> IO ()
monitor read_msg = forever $ do
    Just (Midi.ReadMessage (Midi.ReadDevice dev) ts msg) <- read_msg
    print (ts, dev, msg)


-- * thru

thru_loop :: WriteMsg -> ReadMsg -> IO ()
thru_loop write_msg read_msg = forever $ do
    Just (Midi.ReadMessage dev ts msg) <- read_msg
    putStrLn $ "thru: " ++ show (ts, dev, msg)
    write_msg (0, msg)


-- * melody

thru_melody :: WriteMsg -> ReadMsg -> IO ()
thru_melody write_msg read_msg = do
    now <- CoreMidi.now
    mapM_ write_msg (melody now)
    thru_loop write_msg read_msg

melody :: RealTime.RealTime -> [(RealTime.RealTime, Midi.Message)]
melody start_ts = concat
    [[(ts, note_on nn), (ts + RealTime.seconds 0.4, note_off nn)]
        | (ts, nn) <- zip (Seq.range_ start_ts (RealTime.seconds 0.5)) score]
    where score = [53, 55 .. 61]


-- * spam

spam :: WriteMsg -> Int -> IO ()
spam write_msg n = do
    now <- CoreMidi.now
    mapM_ write_msg
        [(now + RealTime.milliseconds (i*10), msg) | (i, msg) <- zip [0..] msgs]
    where
    msgs = take n [Midi.ChannelMessage 0 msg | nn <- cycle [0..127],
        msg <- [Midi.NoteOn nn 127, Midi.NoteOff nn 127]]

-- * tests

run_tests :: WriteMsg -> ReadMsg -> IO ()
run_tests write_msg read_msg = do
    test_abort write_msg read_msg
    test_merge write_msg read_msg
    void $ test_sysex write_msg read_msg

test_abort write_msg read_msg = do
    now <- CoreMidi.now
    let msgs = [note_on 10, note_on 20, chan_msg (Midi.PitchBend 42)]
    mapM_ write_msg [(now + RealTime.milliseconds (i*100), msg)
        | (i, msg) <- zip [5..] msgs]
    sleep 0.2
    CoreMidi.abort
    sleep 1
    msgs <- read_all read_msg
    equal msgs []
    -- TODO: is the pitchbend bug gone?
    putStr "msgs after abort: "
    pprint msgs

test_merge write_msg read_msg = do
    now <- CoreMidi.now
    let notes1 = [(10, note_on 10), (20, note_on 11), (30, note_on 12)]
    let notes2 = [(15, note_on 20), (25, note_on 21), (35, note_on 22)]
    mapM_ write_msg [(now + (ts*10), msg) | (ts, msg) <- notes1]
    mapM_ write_msg [(now + (ts*10), msg) | (ts, msg) <- notes2]
    sleep 1
    msgs <- read_all read_msg
    equal (map Midi.rmsg_msg msgs)
        [note_on 10, note_on 20, note_on 11, note_on 21, note_on 12, note_on 22]
    putStrLn "interleaved msgs:"
    pprint [(Midi.rmsg_ts rmsg - now, Midi.rmsg_msg rmsg) | rmsg <- msgs]

test_sysex write_msg read_msg = do
    let size = 20
    let msg = Midi.CommonMessage $ Midi.SystemExclusive 42
            (ByteString.pack (take (size*1024) (cycle [0..9]) ++ [0xf7]))
    write_msg (0, msg)
    putStrLn "waiting for sysex to arrive..."
    Just (out, secs) <- read_until 10 read_msg
    putStrLn $ show secs ++ " seconds for " ++ show size ++ "k"
    let out_msg = Midi.rmsg_msg out
    if out_msg == msg then success "sysex equal"
        else failure $ "got sysex: " ++ show out_msg


-- * util

chan_msg = Midi.ChannelMessage 0
note_on nn = chan_msg (Midi.NoteOn nn 70)
note_off nn = chan_msg (Midi.NoteOff nn 70)

sleep = Concurrent.threadDelay . floor . (*1000000)

read_until timeout read_msg = do
    started <- Time.getCurrentTime
    let abort_at = timeout `Time.addUTCTime` started
    let go = do
            msg <- read_msg
            now <- Time.getCurrentTime
            case msg of
                Just m -> return (Just (m, now `Time.diffUTCTime` started))
                Nothing -> do
                    if now > abort_at
                        then return Nothing
                        else sleep 0.25 >> go
    go

read_all read_msg = do
    msg <- read_msg
    case msg of
        Just m -> do
            rest <- read_all read_msg
            return (m:rest)
        Nothing -> return []

make_write_msg wdev wdev_map (ts, msg) = do
    -- putStrLn $ "write: " ++ show (wdev, ts, msg)
    CoreMidi.write_message dev_id ts msg
    where dev_id = wdev_map Map.! wdev

nonblocking_get read_chan = STM.atomically $
    fmap Just (STM.readTChan read_chan) `STM.orElse` return Nothing
blocking_get read_chan = STM.atomically $ fmap Just (STM.readTChan read_chan)
