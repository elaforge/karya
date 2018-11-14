-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Process_test where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Chan as Chan
import Control.Monad (replicateM)

import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified System.Process

import qualified Util.Process as Process
import qualified Util.Thread as Thread

import Util.Test


-- It's kind of annoying to test this automatically, so just make sure it looks
-- right when run by hand.
manual_test_supervised = do
    tid <- Concurrent.forkIO $
        Process.supervised (System.Process.shell "sleep 1; echo sub done")
    -- With the kill, I shouldn't see "sub done".
    Thread.delay 0.1
    Concurrent.killThread tid

manual_test_supervised_no_binary =
    Process.supervised (System.Process.proc "aoeu" [])

test_conversation_stdout = do
    input <- Chan.newChan
    Process.conversation "cat" ["-u"] Nothing input $ \output -> do
        Chan.writeChan input "hi there"
        io_equal (Chan.readChan output) (Process.Stdout "hi there")
        Chan.writeChan input Process.EOF
        io_equal (Chan.readChan output) (Process.Exit (Process.ExitCode 0))

test_conversation_stderr = do
    input <- noInput
    Process.conversation "cat" ["no-such-file"] Nothing input $ \output -> do
        io_equal (Chan.readChan output)
            (Process.Stderr "cat: no-such-file: No such file or directory")
        io_equal (Chan.readChan output) (Process.Exit (Process.ExitCode 1))

test_conversation_not_found = do
    input <- noInput
    Process.conversation "no-such-bin" [] Nothing input $ \output ->
        io_equal (Chan.readChan output) (Process.Exit Process.BinaryNotFound)

noisySleep :: Int -> (FilePath, [String])
noisySleep seconds =
    ("sh", ["-c", "sleep " <> show seconds <> "; echo done " <> show seconds])

test_multiple_supervised = do
    -- If the thread that started the processes is killed, it kills them in
    -- turn.
    outputRef <- IORef.newIORef Nothing
    tid <- Concurrent.forkIO $
        Process.multipleOutput [noisySleep 1, noisySleep 2] $ \output -> do
            IORef.writeIORef outputRef (Just output)
            Thread.delay 4
    Thread.delay 0.1
    Just output <- IORef.readIORef outputRef
    Concurrent.killThread tid
    let sigterm = Process.Exit (Process.ExitCode (-15))
    io_equal (snd <$> Chan.readChan output) sigterm
    io_equal (snd <$> Chan.readChan output) sigterm

noInput :: IO (Chan.Chan Process.TalkIn)
noInput = do
    input <- Chan.newChan
    Chan.writeChan input Process.EOF
    return input

test_multiple = do
    Process.multipleOutput [("cat", ["nofile1"]), ("cat", ["nofile2"])] $
        \output -> do
            outs <- replicateM 4 (Chan.readChan output)
            equal (List.sort outs)
                [ (("cat", ["nofile1"]),
                    Process.Stderr "cat: nofile1: No such file or directory")
                , (("cat", ["nofile1"]), Process.Exit (Process.ExitCode 1))
                , (("cat", ["nofile2"]),
                    Process.Stderr "cat: nofile2: No such file or directory")
                , (("cat", ["nofile2"]), Process.Exit (Process.ExitCode 1))
                ]
