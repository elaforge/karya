-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Define the protocol between the sequencer's repl port and the repl client.
module App.ReplProtocol (
    -- * types
    Query(..), Response(..), CmdResult(..), Result(..), Editor(..), File(..)
    , empty_result, error_result, raw
    -- * protocol
    , initialize
    , query_cmd, query_save_file, query_completion
    , server_receive, server_send
    -- * format
    , format_result
    , abbreviate_package_loads
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Network
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Posix as Posix

import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import Util.Serialize (get, put, get_tag, put_tag)

import Global


-- | This is a simple RPC mechanism.
data Query = QSaveFile | QCommand !Text | QCompletion !Text
    deriving (Eq, Show)

data Response =
    RSaveFile !(Maybe FilePath) -- ^ current save file
    | RCommand !CmdResult
    | RCompletion ![Text] -- ^ possible completions for the prefix
    deriving (Eq, Show)

data CmdResult = CmdResult !Result ![Log.Msg]
    deriving (Eq, Show)

data Result =
    Raw !Text -- ^ Print this text directly, without formatting it.
    | Format !Text -- ^ Format and print.
    | Edit !Editor
    deriving (Eq, Show)

-- | Open an editor locally.
data Editor = Editor {
    _file :: !File
    -- | Start editing on this line.
    , _line_number :: !Int
    -- | Send QCommands when the editor saves or quits.  A %s is replaced by
    -- the edited text.
    , _on_save :: !(Maybe Text)
    -- | Send on an explicit send cmd, \'gs\' in vi.
    , _on_send :: !(Maybe Text)
    } deriving (Eq, Show)

data File = FileName !FilePath -- ^ open this file
    | Text !Text -- ^ open this text in a temp file
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
query :: Network.PortID -> Query -> IO (Either Exception.IOException Response)
query socket query = Exception.try $ do
    hdl <- Network.connectTo "localhost" socket
    send hdl query
    IO.hFlush hdl
    receive hdl

-- | Send a 'QCommand'.
query_cmd :: Network.PortID -> Text -> IO CmdResult
query_cmd socket cmd = do
    response <- query socket (QCommand cmd)
    return $ case response of
        Right (RCommand result) -> result
        Right response -> raw $ "unexpected response: " <> showt response
        Left exc -> raw $ "exception: " <> showt exc

-- | Ask for the current save filename.  Nothing for an error, and Just Nothing
-- for no save file.
query_save_file :: Network.PortID -> IO (Maybe (Maybe FilePath))
query_save_file socket = do
    response <- query socket QSaveFile
    case response of
        Right (RSaveFile fname) -> return (Just fname)
        Left exc | IO.Error.isDoesNotExistError exc -> return Nothing
        _ -> do
            Log.error $ "unexpected response to QSaveFile: " <> showt response
            return Nothing

query_completion :: Network.PortID -> Text -> IO [Text]
query_completion socket prefix = do
    response <- query socket (QCompletion prefix)
    case response of
        Right (RCompletion words) -> return words
        Left exc | IO.Error.isDoesNotExistError exc -> return []
        _ -> do
            Log.error $ "unexpected response to QCompletion: " <> showt response
            return []

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
    let int_bytes = ByteString.length (Serialize.encode (0 :: Int))
    size <- ByteString.hGet hdl int_bytes
    -- If the app just quit, say because it got a 'quit' cmd, the accept loop
    -- will continue to live long enough to accept another query, but then
    -- close the socket.  A proper fix might be to block the accept loop while
    -- it handles a cmd, but it's a tiny corner case and this seems to do just
    -- as well.
    when (size == "") $
        Exception.throwIO $ IO.Error.mkIOError IO.Error.doesNotExistErrorType
            "client closed handle" Nothing Nothing
    size <- either (errorIO . txt) return (Serialize.decode size)
    either (errorIO . txt) return . Serialize.decode
        =<< ByteString.hGet hdl size

-- * format

-- | Format the response and strip trailing whitespace.
format_result :: CmdResult -> Text
format_result (CmdResult response logs_) =
    Text.stripEnd $ Text.unlines $
        (if null logs then [] else "Logs:" : map pretty logs ++ [""])
            ++ [format response]
    where logs = abbreviate_package_loads logs_

format :: Result -> Text
format (Raw val) = val
format (Format val) = txt $ PPrint.format_str $ untxt val
format (Edit editor) = "Edit: " <> showt (_file editor)

abbreviate_package_loads :: [Log.Msg] -> [Log.Msg]
abbreviate_package_loads logs = loaded ++ filter (not . package_log) logs
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
    put (QCompletion a) = put_tag 2 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> return QSaveFile
        1 -> QCommand <$> get
        2 -> QCompletion <$> get
        _ -> Serialize.bad_tag "Query" tag

instance Serialize.Serialize Response where
    put (RSaveFile a) = put_tag 0 >> put a
    put (RCommand a) = put_tag 1 >> put a
    put (RCompletion a) = put_tag 2 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> RSaveFile <$> get
        1 -> RCommand <$> get
        2 -> RCompletion <$> get
        _ -> Serialize.bad_tag "Response" tag

instance Serialize.Serialize CmdResult where
    put (CmdResult a b) = put a >> put b
    get = CmdResult <$> get <*> get

instance Serialize.Serialize Result where
    put (Raw a) = put_tag 0 >> put a
    put (Format a) = put_tag 1 >> put a
    put (Edit a) = put_tag 2 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> Raw <$> get
        1 -> Format <$> get
        2 -> Edit <$> get
        _ -> Serialize.bad_tag "Result" tag

instance Serialize.Serialize Editor where
    put (Editor a b c d) = put a >> put b >> put c >> put d
    get = Editor <$> get <*> get <*> get <*> get

instance Serialize.Serialize File where
    put (FileName a) = put_tag 0 >> put a
    put (Text a) = put_tag 1 >> put a
    get = get_tag >>= \tag -> case tag of
        0 -> FileName <$> get
        1 -> Text <$> get
        _ -> Serialize.bad_tag "File" tag

instance DeepSeq.NFData CmdResult where
    rnf (CmdResult a b) = a `seq` b `seq` ()
