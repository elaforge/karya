-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
-- | Define the protocol between the sequencer's repl port and the repl client.
module App.ReplProtocol (
    -- * types
    Query(..), Response(..), CmdResult(..), Result(..)
    , empty_result, error_result, raw
    -- * protocol
    , initialize
    , query, query_cmd
    , server_receive, server_send
    -- * format
    , format_result
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text

import qualified Network
import qualified System.IO as IO
import qualified System.Posix as Posix

import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put, get_tag, put_tag)

import qualified App.Config as Config
import Global


-- | This is a simple RPC mechanism.
data Query = QSaveFile -- ^ ask for a RSaveFile
    | QCommand Text -- ^ send a cmd, expect a RCommand
    deriving (Eq, Show)

data Response =
    RSaveFile FilePath -- ^ current save file
    | RCommand CmdResult
    deriving (Eq, Show)

data CmdResult = CmdResult !Result ![Log.Msg]
    deriving (Eq, Show)

data Result = Raw !Text -- ^ print this text directly, without formatting it
    | Format !Text -- ^ format and print
    deriving (Eq, Show)

empty_result :: Result
empty_result = Raw ""

error_result :: Text -> CmdResult
error_result msg = CmdResult empty_result [Log.msg Log.Error Nothing msg]

raw :: Text -> CmdResult
raw msg = CmdResult (Raw msg) []


-- * protocol

initialize :: IO a -> IO a
initialize app = Network.withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE (Posix.Catch sigpipe) Nothing
    app
    where
    sigpipe = IO.hPutStrLn IO.stderr
        "caught SIGPIPE, reader must have closed the socket"

-- | Client send and receive.
query :: FilePath -> Query -> IO Response
query socket query = do
    hdl <- Network.connectTo "localhost" (Network.UnixSocket socket)
    send hdl query
    IO.hFlush hdl
    receive hdl

-- | Specialized 'query'.
query_cmd :: Text -> IO Text
query_cmd cmd = do
    response <- query Config.repl_port (QCommand cmd)
        `Exception.catch` \(exc :: Exception.SomeException) ->
            return $ RCommand $ raw $ "error: " <> Text.pack (show exc)
    return $ case response of
        RCommand result -> format_result result
        _ -> "unexpected response: " <> showt response

server_receive :: IO.Handle -> IO Query
server_receive = receive

server_send :: IO.Handle -> Response -> IO ()
server_send = send

-- ** implementation

-- | Write a msg size and then the msg.
send :: Serialize.Serialize a => IO.Handle -> a -> IO ()
send hdl msg = do
    ByteString.hPut hdl (Serialize.encode (ByteString.length bytes))
    ByteString.hPut hdl bytes
    where bytes = Serialize.encode msg

receive :: Serialize.Serialize a => IO.Handle -> IO a
receive hdl = do
    size <- either (errorIO . txt) return . Serialize.decode
        =<< ByteString.hGet hdl int_bytes
    either (errorIO . txt) return . Serialize.decode
        =<< ByteString.hGet hdl size
    where
    int_bytes = ByteString.length (Serialize.encode (0 :: Int))

-- * format

-- | Format the response and strip trailing whitespace.
format_result :: CmdResult -> Text
format_result (CmdResult response logs_) =
    Text.stripEnd $ Text.unlines $
        (if null logs then [] else "Logs:" : map pretty logs ++ [""])
            ++ [format response]
    where logs = abbreviate_logs logs_

format :: Result -> Text
format (Raw val) = val
format (Format val) = txt $ PPrint.format_str $ untxt val

abbreviate_logs :: [Log.Msg] -> [Log.Msg]
abbreviate_logs logs = loaded ++ filter (not . package_log) logs
    where
    loaded =
        [ Log.msg Log.Notice Nothing $
            "Loaded " <> Text.pack (show packages) <> " packages"
        | packages > 0
        ]
    packages =
        Seq.count (("Loading package" `Text.isPrefixOf`) . Log.msg_text) logs
    package_log log = any (`Text.isPrefixOf` Log.msg_text log)
        ["Loading package", "linking ...", "done."]

-- * instances

instance Serialize.Serialize Query where
    put QSaveFile = put_tag 0
    put (QCommand a) = put_tag 1 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> return QSaveFile
        1 -> QCommand <$> get
        _ -> Serialize.bad_tag "Query" tag

instance Serialize.Serialize Response where
    put (RSaveFile a) = put_tag 0 >> put a
    put (RCommand a) = put_tag 1 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> RSaveFile <$> get
        1 -> RCommand <$> get
        _ -> Serialize.bad_tag "Response" tag

instance Serialize.Serialize CmdResult where
    put (CmdResult a b) = put a >> put b
    get = CmdResult <$> get <*> get

instance Serialize.Serialize Result where
    put (Raw a) = put_tag 0 >> put a
    put (Format a) = put_tag 1 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> Raw <$> get
        1 -> Format <$> get
        _ -> Serialize.bad_tag "Result" tag

instance DeepSeq.NFData CmdResult where
    rnf (CmdResult a b) = a `seq` b `seq` ()
