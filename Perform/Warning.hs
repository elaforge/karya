-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- Monad.Error
module Perform.Warning where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Error as Error

import qualified Derive.Stack as Stack
import Types


data Warning = Warning {
    warn_msg :: String
    , warn_event :: Stack.Stack
    -- | Range that the warning covers.  It should be within the event's
    -- range.  It's in real time, so it needs to be converted back to
    -- score time, and it's (start, end) rather than (start, dur).
    -- TODO: convert these back to ScoreTime with the tempo map
    -- then I can put them in the stack
    , warn_pos :: Maybe (RealTime, RealTime)
    } deriving (Eq, Show)
warning = Warning

instance Error.Error Warning where
    strMsg msg = Warning msg Stack.empty Nothing

instance DeepSeq.NFData Warning where
    rnf (Warning msg stack pos) = DeepSeq.rnf msg `seq` DeepSeq.rnf stack
        `seq` DeepSeq.rnf pos
