-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utils for trace-oriented logging.
module Util.Trace where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Debug.Trace as Trace
import Global


force :: (MonadIO m, DeepSeq.NFData a) => a -> m ()
force = liftIO . Exception.evaluate . DeepSeq.rnf
{-# INLINABLE force #-}

-- | Write a trace entry.  This goes in the eventlog, and can be read by
-- threadscope or chrome, after App.ConvertEventLog
trace :: MonadIO m => String -> m ()
trace = liftIO . Trace.traceMarkerIO
{-# INLINABLE trace #-}
