-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Display time elapsed between Timer log msgs.
module LogView.ShowTimers where
import Control.Monad
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified System.Environment as Environment
import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.SrcPos as SrcPos
import qualified LogView.Tail as Tail


minimumDiff :: Double
minimumDiff = 0.0

main :: IO ()
main = do
    args <- Environment.getArgs
    hdl <- case args of
        [fn] -> Tail.open fn Nothing
        _ -> error "usage: show_timers filename"
    loop hdl Nothing
    where
    loop hdl last_date = do
        (msg, hdl) <- Tail.tail hdl
        case Log.msg_priority msg of
            Log.Timer -> do
                let date = Log.msg_date msg
                let diff = realToFrac $
                        maybe 0 (Time.diffUTCTime date) last_date
                when (diff >= minimumDiff) $
                    Printf.printf "%.03f %s %s\n" diff
                        (SrcPos.show_srcpos (Log.msg_caller msg))
                        (Text.unpack (Log.msg_text msg))
                loop hdl (Just date)
            _ -> loop hdl last_date
