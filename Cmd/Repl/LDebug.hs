-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Debugging utilities.
module Cmd.Repl.LDebug where
import qualified System.IO as IO

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Repl.LPerf as LPerf
import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.PerformTest as PerformTest
import Types


-- * block

-- | Save state in a format that can be copy-pasted into a test, and loaded
-- with 'UiTest.read_blocks'.
dump_blocks :: FilePath -> Cmd.CmdL ()
dump_blocks fname = do
    state <- State.get
    liftIO $ UiTest.dump_blocks fname state

-- | Save a block in a format that can be copy-pasted into a test, and loaded
-- with 'UiTest.read_blocks'.
dump_block :: FilePath -> BlockId -> Cmd.CmdL ()
dump_block fname block_id = do
    st <- State.get
    liftIO $ IO.writeFile fname $ PPrint.pshow [UiTest.show_block block_id st]

-- * perf events

dump_block_perf_events :: FilePath -> BlockId -> Cmd.CmdL ()
dump_block_perf_events fname block_id = do
    events <- LPerf.convert . LEvent.events_of =<< LPerf.block_events block_id
    dump_perf_events fname (LEvent.events_of events)

dump_perf_events :: FilePath -> [Perform.Event] -> Cmd.CmdL ()
dump_perf_events fname events =
    liftIO $ PerformTest.dump_perf_events fname events

-- * undo

show_history :: Cmd.CmdL String
show_history = do
    save_file <- ("save file: "++) . show <$> Cmd.gets Cmd.state_save_file
    hist <- Pretty.formatted <$> Cmd.gets Cmd.state_history
    config <- Pretty.formatted <$> Cmd.gets Cmd.state_history_config
    collect <- Pretty.formatted <$> Cmd.gets Cmd.state_history_collect
    return $ unlines $ map Seq.strip [save_file, hist, config, collect]
