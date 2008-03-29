{- | Functions for logging.

Logs are used to report everything from errors and debug msgs to status
reports.  They are collected in a central place that writes them to a file
in a machine-readable serialized format, and also passes them to various
registered log actions.

So a progress bar would register a function that looks for Progress msgs from
the relevant system and updates itself when it sees them.  Blocks register
a function that watches for Block errors and highlights the part of the block
that had the error (probably emitted by Derive).  Informational msgs matching
a given pattern may go to the status bar.

-}

module Util.Log (
    Msg, Prio(..), msg, msg_io, log, write
    , debug, notice, warn, error
    , hex -- debugging, remove me later
) where
import Prelude hiding (error, log)
import qualified Data.Word as Word
import qualified Data.Time as Time
import Text.Printf (printf)

data Msg = Msg
    { msg_date :: Maybe Time.UTCTime
    , msg_prio :: Prio
    -- Named system, higher level than just the filename.
    -- , msg_system :: System
    -- , msg_file_context :: FileContext
    -- | Free form text for humans.
    , msg_text  :: String
    -- | Higher level context info for the msg.
    , msg_context :: [Context]
    -- Additional misc attributes the msg may have.
    -- , msg_attrs :: [(String, String)]
    } deriving (Show, Eq)

data Prio = Debug -- ^ Lots of msgs produced by code level.  Users don't look
        -- at this during normal use, but can be useful for debugging.
    | Notice -- ^ Informational msgs that the user will want to see.  Progress
        -- messages in e.g. derivation and play status are included here.
    | Warn -- ^ Something went wrong in e.g. derivation.  User definitely wants
        -- to see this.
    | Error -- ^ Code error in the app, which may quit after printing this.
    deriving (Show, Enum, Eq, Ord)

msg :: Prio -> String -> Msg
msg prio text = Msg Nothing prio text []
msg_io :: Prio -> String -> IO Msg
msg_io prio text = do
    utc <- Time.getCurrentTime
    return ((msg prio text) {msg_date = Just utc})

-- Later have versions that log context, log to a logging monad, etc.
-- TODO how can I automatically get FileContext?
-- TODO system should be dynamically scoped, 'Log.with_system sys (code)'
log :: Prio -> String -> IO ()
log prio text = write (msg prio text)
default_show msg = printf "%s - %s" (prio_stars (msg_prio msg)) (msg_text msg)

write :: Msg -> IO ()
write msg = putStrLn (default_show msg)

debug = log Debug
notice = log Notice
warn = log Warn
error = log Error

prio_stars prio = replicate (fromEnum prio + 1) '*'

{-
data System
    = App -- ^ app level stuff, goes to stderr
    | Playback
    | Derive
    | UI
    deriving (Show, Eq)

-- | Where in what file did the error happen?
data FileContext = FileContext
    { file_name :: String
    , file_lineno :: Int
    } deriving (Show, Eq)
-}

-- | More specific data for the msg.  A derivation would send Progress
-- reports at Info, an error in derivation would send Block at Warn, and
-- playback would send Block at Info.
data Context
    -- | Progress report on a process.
    = Progress
        { progress_amount :: Double
        , progress_total :: Double
        }
    -- | Msg pertains to this region in a block.
    | Block
        { block_name :: String -- ^ Just the name, to avoid circular imports.
        , block_area :: () -- Types.Selection -- ^ affected area
        }
    deriving (Show, Eq)

hex :: Word.Word8 -> String
hex = printf "0x%02x"
