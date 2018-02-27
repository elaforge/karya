-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Convert ghc eventlog to json for <chrome://tracing>.
module App.ConvertEventLog (main) where
import qualified System.Environment as Environment
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text as Text
import qualified Data.Word as Word

import qualified GHC.RTS.Events as Events
import Global


main :: IO ()
main = do
    log <- either error return =<< Events.readEventLogFromFile "seq.eventlog"
    let events = Events.events $ Events.dat log
    args <- Environment.getArgs
    case args of
        [] -> do
            print (length events)
            mapM_ print $ events
        [out] -> write out $ concatMap convertEvent events
        _ -> putStrLn "usage"

write :: FilePath -> [Event] -> IO ()
write fname events = ByteString.Lazy.writeFile fname $
    Aeson.encode events

convertEvent :: Events.Event -> [Event]
convertEvent e = do
    capability <- mapMaybe Events.evCap [e]
    (phase, name) <- case Events.evSpec e of
        Events.UserMessage msg -> case msg of
            "respond" -> [(End, "wait"), (Begin, "respond")]
            "wait" -> [(End, "respond"), (Begin, "wait")]
            _ -> [(Instant, Text.pack msg)]
        _ -> []
    return $ Event
        { _category = "user"
        , _name = name
        , _args = []
        , _processId = 1
        , _threadId = capability
        , _timestamp = Events.evTime e `div` 1000
        , _phase = phase
        }

data Event = Event {
    _category :: !Text
    , _name :: !Text
    , _args :: ![(Text, Text)]
    , _processId :: !Int
    , _threadId :: !Int
    , _timestamp :: !Word.Word64 -- millis?
    , _phase :: !Phase
    } deriving (Eq, Show)

data Phase = None | Begin | End
    -- | Begin End that cross threads or processes.
    | AsyncBegin | AsyncEnd
    | Instant
    deriving (Eq, Show)

instance Aeson.ToJSON Event where
    toEncoding (Event cat name args pid tid ts phase) = Aeson.pairs $ mconcat
        [ "cat" .= cat
        , "name" .= name
        , "args" .= args
        , "pid" .= pid
        , "tid" .= tid
        , "ts" .= ts
        , "ph" .= phase
        ]
    -- TODO how to remove the duplication?
    toJSON (Event cat name args pid tid ts phase) = Aeson.object
        [ "cat" .= cat
        , "name" .= name
        , "args" .= args
        , "pid" .= pid
        , "tid" .= tid
        , "ts" .= ts
        , "ph" .= phase
        ]

instance Aeson.ToJSON Phase where
    toJSON = \case
        None -> "X" -- I don't know what X is, but chrome uses it.
        Begin -> "B"
        End -> "E"
        AsyncBegin -> "S"
        AsyncEnd -> "F"
        Instant -> "I"
