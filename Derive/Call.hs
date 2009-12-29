-- | Utilities for writing calls.  This is higher-level than TrackLang, so
-- it can import "Derive.Derive".
module Derive.Call where
import Control.Monad
import qualified Data.Map as Map
import qualified Util.Map as Map

import Ui
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal


-- | Transform an event list.  As a convenience, you can optionally pass a list
-- of signals which will be looked up at each event start.  The mapped function
-- can return any number of events in any order.  This is flexible but destroys
-- laziness.
map_events :: (Monad m) => [Score.Event] -> [TrackLang.Signal]
    -> (Score.Event -> [Signal.Y] -> Derive.DeriveT m [Score.Event])
    -> Derive.DeriveT m [Score.Event]
map_events events sigs f = do
    sorted <- foldM go Map.empty events
    return (Map.elems sorted)
    where
    go emap event = do
        sig_vals <- get_signals (Score.event_start event) sigs
        out <- f event sig_vals
        return $ Map.insert_list [(Score.event_start e, e) | e <- out] emap

get_signal :: (Monad m) => TrackPos -> TrackLang.Signal
    -> Derive.DeriveT m Signal.Y
get_signal pos (TrackLang.Signal (deflt, control)) = case control of
    Nothing -> return deflt
    Just cont -> Derive.control_at pos cont deflt

get_signals :: (Monad m) => TrackPos -> [TrackLang.Signal]
    -> Derive.DeriveT m [Signal.Y]
get_signals pos = mapM (get_signal pos)
