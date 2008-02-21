{-
Listen on a port for OSC messages, and return them as msgs.
-}
module Interface.OscMsg where

data OscMsg = OscMsg
    deriving (Show)

take_osc_msgs :: IO [OscMsg]
take_osc_msgs = return []
