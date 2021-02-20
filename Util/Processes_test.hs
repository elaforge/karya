-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Processes_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import           Control.Monad (replicateM)

import qualified Data.List as List
import qualified System.Process as Process

import qualified Util.Processes as Processes
import qualified Util.Thread as Thread

import           Util.Test


-- It's kind of annoying to test this automatically, so just make sure it looks
-- right when run by hand.
manual_test_supervised :: IO ()
manual_test_supervised = do
    tid <- Concurrent.forkIO $
        Processes.supervised (Process.shell "sleep 1; echo sub done")
    -- With the kill, I shouldn't see "sub done".
    Thread.delay 0.1
    Concurrent.killThread tid

manual_test_supervised_no_binary :: IO ()
manual_test_supervised_no_binary =
    Processes.supervised (Process.proc "aoeu" [])

test_conversation_stdout :: Test
test_conversation_stdout = do
    input <- Chan.newChan
    Processes.conversation "cat" ["-u"] Nothing input $ \output -> do
        Chan.writeChan input "hi there"
        io_equal (Chan.readChan output) (Processes.Stdout "hi there")
        Chan.writeChan input Processes.EOF
        io_equal (Chan.readChan output) (Processes.Exit (Processes.ExitCode 0))

test_conversation_stderr :: Test
test_conversation_stderr = do
    input <- noInput
    Processes.conversation "cat" ["no-such-file"] Nothing input $ \output -> do
        io_equal (Chan.readChan output)
            (Processes.Stderr "cat: no-such-file: No such file or directory")
        io_equal (Chan.readChan output) (Processes.Exit (Processes.ExitCode 1))

test_conversation_not_found :: Test
test_conversation_not_found = do
    input <- noInput
    Processes.conversation "no-such-bin" [] Nothing input $ \output ->
        io_equal (Chan.readChan output)
            (Processes.Exit Processes.BinaryNotFound)

noisySleep :: Int -> (FilePath, [String])
noisySleep seconds =
    ("sh", ["-c", "sleep " <> show seconds <> "; echo done " <> show seconds])

test_multiple_supervised :: Test
test_multiple_supervised = do
    -- If the thread that started the processes is killed, it kills them in
    -- turn.
    outputVar <- MVar.newEmptyMVar
    tid <- Concurrent.forkIO $
        Processes.multipleOutput [noisySleep 1, noisySleep 2] $ \output -> do
            MVar.putMVar outputVar output
            Thread.delay 4
    output <- MVar.takeMVar outputVar
    Concurrent.killThread tid
    let sigterm = Processes.Exit (Processes.ExitCode (-15))
    io_equal (snd <$> Chan.readChan output) sigterm
    io_equal (snd <$> Chan.readChan output) sigterm

noInput :: IO (Chan.Chan Processes.TalkIn)
noInput = do
    input <- Chan.newChan
    Chan.writeChan input Processes.EOF
    return input

test_multiple :: Test
test_multiple = do
    Processes.multipleOutput [("cat", ["nofile1"]), ("cat", ["nofile2"])] $
        \output -> do
            outs <- replicateM 4 (Chan.readChan output)
            equal (List.sort outs)
                [ (("cat", ["nofile1"]),
                    Processes.Stderr "cat: nofile1: No such file or directory")
                , (("cat", ["nofile1"]), Processes.Exit (Processes.ExitCode 1))
                , (("cat", ["nofile2"]),
                    Processes.Stderr "cat: nofile2: No such file or directory")
                , (("cat", ["nofile2"]), Processes.Exit (Processes.ExitCode 1))
                ]
