-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Define the protocol between the sequencer's repl port and the repl client.
module App.ReplProtocol (
    -- * types
    Query(..), NotifySeq(..)
    , Response(..)
    , CmdResult(..), Result(..), Editor(..), File(..)
    , FileType(..), fileTypeExtension
    , emptyResult, errorResult, raw

    -- * repl protocol
    , initialize
    , queryCmd, querySaveFile, queryCompletion
    , notify
    , queryCmdSimple

    -- * seq protocol
    , seqReceive, seqSend

    -- * format
    , formatResult
    , abbreviatePackageLoads
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
import           Util.Serialize (get, getTag, put, putTag)

import qualified App.Config as Config

import           Global


-- | This is a simple RPC mechanism.  'Query' goes to the server, which
-- responds with the matching 'Response'.
data Query
    = QSaveFile
    | QCommand !Text
    | QCompletion !Text
    -- | A notification doesn't have a corrpesponding 'Response'.
    | QNotify !NotifySeq
    deriving (Eq, Show)

data NotifySeq
    -- | These are so the sequencer knows if there are open editors, so it
    -- can refuse to quit and hence orphan them.
    = NEditorOpened | NEditorClosed
    deriving (Eq, Show)

data Response
    = RSaveFile !(Maybe FilePath) -- ^ current save file
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

data Result
    = Raw !Text -- ^ Print this text directly, without formatting it.
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
data Editor = Editor
    { _file :: !File
    -- | Start editing on this line.
    , _lineNumber :: !Int
    -- | Send QCommands when the editor saves or quits.  A %s is replaced by
    -- the edited text.
    , _onSave :: !(Maybe Text)
    -- | Send on an explicit send cmd, @gs@ in vim.
    , _onSend :: !(Maybe Text)
    } deriving (Eq, Show)

data File
    = FileName !FilePath -- ^ open this file
    | Text !FileType !Text -- ^ open this text in a temp file
    deriving (Eq, Show)

data FileType = NoType | Ky | TScore
    deriving (Eq, Show, Bounded, Enum)

fileTypeExtension :: FileType -> String
fileTypeExtension = \case
    NoType -> ""
    Ky -> ".ky"
    TScore -> ".tscore"

emptyResult :: Result
emptyResult = Raw ""

errorResult :: Text -> CmdResult
errorResult msg = CmdResult emptyResult [Log.msg Log.Error Nothing msg]

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
    replSend hdl query
    IO.hFlush hdl
    replReceive hdl

-- | Like 'query', but don't expect a response.
notify :: Network.Addr -> NotifySeq -> IO (Either Exception.IOException ())
notify addr notify = Exception.try $ Network.withHandle addr $ \hdl -> do
    replSend hdl $ QNotify notify
    IO.hFlush hdl

-- | Send a 'QCommand'.
queryCmd :: Bool -> Network.Addr -> Text -> IO CmdResult
queryCmd noServerOk addr cmd = query addr (QCommand cmd) >>= pure . \case
    Right (RCommand result) -> result
    Right response -> raw $ "unexpected response: " <> showt response
    Left exc
        | IO.Error.isDoesNotExistError exc -> raw $
            if noServerOk then "" else
                "addr (" <> showt addr <> ") threw ENOENT,\
                \ this can happen if karya is not running, or it's stuck."
        | otherwise -> raw $ "exception: " <> showt exc

-- | A simple one-shot 'query_cmd'.
queryCmdSimple :: Bool -> Text -> IO Text
queryCmdSimple noServerOk cmd = formatResult <$>
    queryCmd noServerOk (Network.Unix Config.replSocketName) cmd

-- | Ask for the current save filename.  Nothing for an error, and Just Nothing
-- for no save file.
querySaveFile :: Network.Addr -> IO (Maybe (Maybe FilePath))
querySaveFile addr = query addr QSaveFile >>= \case
    Right (RSaveFile fname) -> pure (Just fname)
    Left exc | IO.Error.isDoesNotExistError exc -> pure Nothing
    response -> do
        Log.error $ "unexpected response to QSaveFile: " <> showt response
        pure Nothing

queryCompletion :: Network.Addr -> Text -> IO [Text]
queryCompletion addr prefix = query addr (QCompletion prefix) >>= \case
    Right (RCompletion words) -> pure words
    Left exc | IO.Error.isDoesNotExistError exc -> pure []
    response -> do
        Log.error $ "unexpected response to QCompletion: " <> showt response
        pure []

-- * low level implementation

-- Specialize 'send' and 'receive' to keep types consistent between seq and
-- repl.

seqReceive :: IO.Handle -> IO Query
seqReceive = receive

seqSend :: IO.Handle -> Response -> IO ()
seqSend = send

replSend :: IO.Handle -> Query -> IO ()
replSend = send

replReceive :: IO.Handle -> IO Response
replReceive = receive

-- | Write a msg size and then the msg.
send :: Serialize.Serialize a => IO.Handle -> a -> IO ()
send hdl msg = do
    ByteString.hPut hdl (Serialize.encode (ByteString.length bytes))
    ByteString.hPut hdl bytes
    where bytes = Serialize.encode msg

receive :: Serialize.Serialize a => IO.Handle -> IO a
receive hdl = do
    let intBytes = ByteString.length (Serialize.encode (0 :: Int))
    size <- ByteString.hGet hdl intBytes
    -- If the app just quit, say because it got a 'quit' cmd, the accept loop
    -- will continue to live long enough to accept another query, but then
    -- close the socket.  A proper fix might be to block the accept loop while
    -- it handles a cmd, but it's a tiny corner case and this seems to do just
    -- as well.
    when (size == "") $
        Exception.throwIO $ IO.Error.mkIOError IO.Error.doesNotExistErrorType
            "client closed handle" Nothing Nothing
    size <- either (errorIO . txt) pure (Serialize.decode size)
    either (errorIO . txt) pure . Serialize.decode
        =<< ByteString.hGet hdl size

-- * format

-- | Format the response and strip trailing whitespace.
formatResult :: CmdResult -> Text
formatResult (CmdResult response logs_) =
    Text.stripEnd $ Text.unlines $
        (if null logs then [] else "Logs:" : map pretty logs ++ [""])
            ++ [format response]
    where logs = abbreviatePackageLoads logs_

format :: Result -> Text
format (Raw val) = val
format (Format val) = txt $ PPrint.formatStr $ untxt val
format (Edit editors) = "Edit: " <> showt (fmap _file editors)

abbreviatePackageLoads :: [Log.Msg] -> [Log.Msg]
abbreviatePackageLoads logs = loaded ++ filter (not . packageLog) logs
    where
    loaded =
        [ Log.msg Log.Notice Nothing $
            "Loaded " <> Text.pack (show packages) <> " packages"
        | packages > 0
        ]
    packages =
        Lists.count (("Loading package" `Text.isPrefixOf`) . Log.msgText) logs
    packageLog log = any (`Text.isPrefixOf` Log.msgText log)
        ["Loading package", "linking ...", "done."]

-- * instances

instance Serialize.Serialize Query where
    put QSaveFile = putTag 0
    put (QCommand a) = putTag 1 >> put a
    put (QCompletion a) = putTag 2 >> put a
    put (QNotify a) = putTag 3 >> put a
    get = getTag >>= \case
        0 -> pure QSaveFile
        1 -> QCommand <$> get
        2 -> QCompletion <$> get
        3 -> QNotify <$> get
        tag -> Serialize.badTag "Query" tag

instance Serialize.Serialize NotifySeq where
    put = \case
        NEditorOpened -> putTag 0
        NEditorClosed -> putTag 1
    get = getTag >>= \case
        0 -> pure NEditorOpened
        1 -> pure NEditorClosed
        tag -> Serialize.badTag "NotifySeq" tag

instance Serialize.Serialize Response where
    put (RSaveFile a) = putTag 0 >> put a
    put (RCommand a) = putTag 1 >> put a
    put (RCompletion a) = putTag 2 >> put a
    get = getTag >>= \case
        0 -> RSaveFile <$> get
        1 -> RCommand <$> get
        2 -> RCompletion <$> get
        tag -> Serialize.badTag "Response" tag

instance Serialize.Serialize CmdResult where
    put (CmdResult a b) = put a >> put b
    get = CmdResult <$> get <*> get

instance Serialize.Serialize Result where
    put (Raw a) = putTag 0 >> put a
    put (Format a) = putTag 1 >> put a
    put (Edit a) = putTag 2 >> put a
    get = getTag >>= \case
        0 -> Raw <$> get
        1 -> Format <$> get
        2 -> Edit <$> get
        tag -> Serialize.badTag "Result" tag

instance Serialize.Serialize Editor where
    put (Editor a b c d) = put a >> put b >> put c >> put d
    get = Editor <$> get <*> get <*> get <*> get

instance Serialize.Serialize File where
    put (FileName a) = putTag 0 >> put a
    put (Text a b) = putTag 1 >> put a >> put b
    get = getTag >>= \case
        0 -> FileName <$> get
        1 -> Text <$> get <*> get
        tag -> Serialize.badTag "File" tag

instance Serialize.Serialize FileType where
    put = Serialize.putEnumUnsafe
    get = Serialize.getEnumUnsafe

instance DeepSeq.NFData CmdResult where
    rnf (CmdResult a b) = a `seq` b `seq` ()
