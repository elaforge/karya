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
import qualified Text.Read as Read

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified LogView.Tail as Tail


minimumDiff :: Double
minimumDiff = 0.0

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [threshold, fn] | Just threshold <- Read.readMaybe threshold -> do
            hdl <- Tail.open fn Nothing
            loop threshold hdl Nothing
        _ -> error "usage: show_timers threshold filename"

loop :: Double -> Tail.Handle -> Maybe Time.UTCTime -> IO ()
loop threshold = go
    where
    go hdl last_date = do
        (msg, hdl) <- Tail.tail hdl
        case Log.msg_priority msg of
            Log.Timer -> do
                let date = Log.msg_date msg
                let diff = realToFrac $
                        maybe 0 (Time.diffUTCTime date) last_date
                when (diff >= minimumDiff) $ do
                    when (diff >= threshold) $
                        putStr vt100_red
                    Printf.printf "%.03f %s %s" diff
                        (Text.unpack
                            (CallStack.showCaller (Log.msg_caller msg)))
                        (Text.unpack (Log.msg_text msg))
                    when (diff >= threshold) $
                        putStr vt100_normal
                    putChar '\n'
                go hdl (Just date)
            _ -> go hdl last_date

-- | These codes should probably come from termcap, but I can't be bothered.
vt100_red :: String
vt100_red = "\ESC[31m"

vt100_normal :: String
vt100_normal = "\ESC[m\ESC[m"
