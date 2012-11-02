{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Debugging utilities.
module Cmd.Lang.LDebug where
import qualified Control.Monad.Trans as Trans
import qualified System.IO as IO

import qualified Util.PPrint as PPrint
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Lang.LPerf as LPerf
import qualified Derive.LEvent as LEvent
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.PerformTest as PerformTest
import Types


-- * block

-- | Save state in a format that can be copy-pasted into a test.
dump_blocks :: FilePath -> Cmd.CmdL ()
dump_blocks fname = do
    state <- State.get
    Trans.liftIO $ UiTest.dump_blocks fname state

-- | Save the given block in a format that can be copy-pasted into a test.
dump_block :: FilePath -> BlockId -> Cmd.CmdL ()
dump_block fname block_id = do
    st <- State.get
    Trans.liftIO $ IO.writeFile fname $
        PPrint.pshow [UiTest.show_block block_id st]

-- * perf events

dump_block_perf_events :: FilePath -> BlockId -> Cmd.CmdL ()
dump_block_perf_events fname block_id = do
    events <- LPerf.convert =<< LPerf.block_events block_id
    dump_perf_events fname (LEvent.events_of events)

dump_perf_events :: FilePath -> [Perform.Event] -> Cmd.CmdL ()
dump_perf_events fname events =
    Trans.liftIO $ PerformTest.dump_perf_events fname events
