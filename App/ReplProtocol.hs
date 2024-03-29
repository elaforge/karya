-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Define the protocol between the sequencer's repl port and the repl client.
module App.ReplProtocol (
    -- * types
    Query(..), NotifySeq(..)
    , Response(..)
    , CmdResult(..), Result(..), Editor(..), File(..)
    , FileType(..), file_type_extension
    , empty_result, error_result, raw

    -- * repl protocol
    , initialize
    , query_cmd, query_save_file, query_completion
    , notify
    , query_cmd_simple

    -- * seq protocol
    , seq_receive, seq_send

    -- * format
    , format_result
    , abbreviate_package_loads
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Network.Socket as Socket
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Posix as Posix

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Network as Network
import qualified Util.PPrint as PPrint
import qualified Util.Serialize as Serialize
import           Util.Serialize (get, get_tag, put, put_tag)

import qualified App.Config as Config

import           Global


-- | This is a simple RPC mechanism.  'Query' goes to the server, which
-- responds with the matching 'Response'.
data Query =
    QSaveFile
    | QCommand !Text
    | QCompletion !Text
    -- | A notification doesn't have a corrpesponding 'Response'.
    | QNotify !NotifySeq
    deriving (Eq, Show)

data NotifySeq =
    -- | These are so the sequencer knows if there are open editors, so it
    -- can refuse to quit and hence orphan them.
    NEditorOpened | NEditorClosed
    deriving (Eq, Show)

data Response =
    RSaveFile !(Maybe FilePath) -- ^ current save file
    | RCommand !CmdResult
    | RCompletion ![Text] -- ^ possible completions for the prefix
    deriving (Eq, Show)

instance Pretty Query where
    pretty = \case
        QSaveFile -> "SaveFile"
        QCommand t -> "Command: " <> t
        QCompletion t -> "Completion: " <> t
        QNotify notify -> showt notify

data CmdResult = CmdResult !Result ![Log.Msg]
    deriving (Eq, Show)

data Result =
    Raw !Text -- ^ Print this text directly, without formatting it.
    | Format !Text -- ^ Format and print.
    | Edit !(NonEmpty Editor) -- ^ Edit one or more files.
    deriving (Eq, Show)

-- | Open an editor locally.
--
-- How this works is that a Cmd will return the special 'Edit' value, which
-- will be specially interpreted in the repl to open an editor.  The editor
-- will be configured such that when it saves, it will send its buffer to
-- the sequencer via a REPL function with a string argument (configured by
-- '_on_save').  The text goes back separately via the @send@ command, since
-- the REPL itself will be blocked waiting for the editor to exit.
data Editor = Editor {
    _file :: !File
    -- | Start editing on this line.
    , _line_number :: !Int
    -- | Send QCommands when the editor saves or quits.  A %s is replaced by
    -- the edited text.
    , _on_save :: !(Maybe Text)
    -- | Send on an explicit send cmd, @gs@ in vim.
    , _on_send :: !(Maybe Text)
    } deriving (Eq, Show)

data File =
    FileName !FilePath -- ^ open this file
    | Text !FileType !Text -- ^ open this text in a temp file
    deriving (Eq, Show)

data FileType = NoType | Ky | TScore
    deriving (Eq, Show, Bounded, Enum)

file_type_extension :: FileType -> String
file_type_extension = \case
    NoType -> ""
    Ky -> ".ky"
    TScore -> ".tscore"

empty_result :: Result
empty_result = Raw ""

error_result :: Text -> CmdResult
error_result msg = CmdResult empty_result [Log.msg Log.Error Nothing msg]

raw :: Text -> CmdResult
raw msg = CmdResult (Raw msg) []

-- * repl protocol

initialize :: IO a -> IO a
initialize app = Socket.withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE (Posix.Catch sigpipe) Nothing
    app
    where
    sigpipe = IO.hPutStrLn IO.stderr
        "caught SIGPIPE, reader must have closed the socket"

-- | Client send and receive.
query :: Network.Addr -> Query -> IO (Either Exception.IOException Response)
query addr query = Exception.try $ Network.withHandle addr $ \hdl -> do
    repl_send hdl query
    IO.hFlush hdl
    repl_receive hdl

-- | Like 'query', but don't expect a response.
notify :: Network.Addr -> NotifySeq -> IO (Either Exception.IOException ())
notify addr notify = Exception.try $ Network.withHandle addr $ \hdl -> do
    repl_send hdl $ QNotify notify
    IO.hFlush hdl

-- | Send a 'QCommand'.
query_cmd :: Network.Addr -> Text -> IO CmdResult
query_cmd addr cmd = query addr (QCommand cmd) >>= return . \case
    Right (RCommand result) -> result
    Right response -> raw $ "unexpected response: " <> showt response
    Left exc
        | IO.Error.isDoesNotExistError exc -> raw $
            "addr (" <> showt addr <> ") threw ENOENT,\
            \ this can happen if karya is not running, or it's stuck."
        | otherwise -> raw $ "exception: " <> showt exc

-- | A simple one-shot 'query_cmd'.
query_cmd_simple :: Text -> IO Text
query_cmd_simple cmd =
    format_result <$> query_cmd (Network.Unix Config.repl_socket_name) cmd

-- | Ask for the current save filename.  Nothing for an error, and Just Nothing
-- for no save file.
query_save_file :: Network.Addr -> IO (Maybe (Maybe FilePath))
query_save_file addr = query addr QSaveFile >>= \case
    Right (RSaveFile fname) -> return (Just fname)
    Left exc | IO.Error.isDoesNotExistError exc -> return Nothing
    response -> do
        Log.error $ "unexpected response to QSaveFile: " <> showt response
        return Nothing

query_completion :: Network.Addr -> Text -> IO [Text]
query_completion addr prefix = query addr (QCompletion prefix) >>= \case
    Right (RCompletion words) -> return words
    Left exc | IO.Error.isDoesNotExistError exc -> return []
    response -> do
        Log.error $ "unexpected response to QCompletion: " <> showt response
        return []

-- * low level implementation

-- Specialize 'send' and 'receive' to keep types consistent between seq and
-- repl.

seq_receive :: IO.Handle -> IO Query
seq_receive = receive

seq_send :: IO.Handle -> Response -> IO ()
seq_send = send

repl_send :: IO.Handle -> Query -> IO ()
repl_send = send

repl_receive :: IO.Handle -> IO Response
repl_receive = receive

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
format (Edit editors) = "Edit: " <> showt (fmap _file editors)

abbreviate_package_loads :: [Log.Msg] -> [Log.Msg]
abbreviate_package_loads logs = loaded ++ filter (not . package_log) logs
    where
    loaded =
        [ Log.msg Log.Notice Nothing $
            "Loaded " <> Text.pack (show packages) <> " packages"
        | packages > 0
        ]
    packages =
        Lists.count (("Loading package" `Text.isPrefixOf`) . Log.msg_text) logs
    package_log log = any (`Text.isPrefixOf` Log.msg_text log)
        ["Loading package", "linking ...", "done."]

-- * instances

instance Serialize.Serialize Query where
    put QSaveFile = put_tag 0
    put (QCommand a) = put_tag 1 >> put a
    put (QCompletion a) = put_tag 2 >> put a
    put (QNotify a) = put_tag 3 >> put a
    get = get_tag >>= \case
        0 -> return QSaveFile
        1 -> QCommand <$> get
        2 -> QCompletion <$> get
        3 -> QNotify <$> get
        tag -> Serialize.bad_tag "Query" tag

instance Serialize.Serialize NotifySeq where
    put = \case
        NEditorOpened -> put_tag 0
        NEditorClosed -> put_tag 1
    get = get_tag >>= \case
        0 -> return NEditorOpened
        1 -> return NEditorClosed
        tag -> Serialize.bad_tag "NotifySeq" tag

instance Serialize.Serialize Response where
    put (RSaveFile a) = put_tag 0 >> put a
    put (RCommand a) = put_tag 1 >> put a
    put (RCompletion a) = put_tag 2 >> put a
    get = get_tag >>= \case
        0 -> RSaveFile <$> get
        1 -> RCommand <$> get
        2 -> RCompletion <$> get
        tag -> Serialize.bad_tag "Response" tag

instance Serialize.Serialize CmdResult where
    put (CmdResult a b) = put a >> put b
    get = CmdResult <$> get <*> get

instance Serialize.Serialize Result where
    put (Raw a) = put_tag 0 >> put a
    put (Format a) = put_tag 1 >> put a
    put (Edit a) = put_tag 2 >> put a
    get = get_tag >>= \case
        0 -> Raw <$> get
        1 -> Format <$> get
        2 -> Edit <$> get
        tag -> Serialize.bad_tag "Result" tag

instance Serialize.Serialize Editor where
    put (Editor a b c d) = put a >> put b >> put c >> put d
    get = Editor <$> get <*> get <*> get <*> get

instance Serialize.Serialize File where
    put (FileName a) = put_tag 0 >> put a
    put (Text a b) = put_tag 1 >> put a >> put b
    get = get_tag >>= \case
        0 -> FileName <$> get
        1 -> Text <$> get <*> get
        tag -> Serialize.bad_tag "File" tag

instance Serialize.Serialize FileType where
    put = Serialize.put_enum_unsafe
    get = Serialize.get_enum_unsafe

instance DeepSeq.NFData CmdResult where
    rnf (CmdResult a b) = a `seq` b `seq` ()
