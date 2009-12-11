module Perform.Warning where
import qualified Control.Monad.Error as Error
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Types as Types
import qualified Ui.Id as Id


data Warning = Warning {
    warn_msg :: String
    , warn_event :: Stack
    -- | Range that the warning covers.  It should be within the event's
    -- range.  It's in global time, so it needs to be converted back to
    -- local time, and it's (start, end) rather than (start, dur).
    -- TODO: convert these back to TrackPos with the tempo map
    -- TODO convert to (start, dur) for consistency.
    , warn_pos :: Maybe (TrackPos, TrackPos)
    } deriving (Eq, Show)
warning = Warning

instance Error.Error Warning where
    strMsg msg = Warning msg [] Nothing

-- | The location of an event that had a problem.
-- (block_id, track_id, (event_start, event_end))
type StackPos = (BlockId, Maybe TrackId, Maybe (TrackPos, TrackPos))

-- | Stack order is most recent call first.
type Stack = [StackPos]

-- | Format a StackPos.  These functions are used by LogView and LanguageCmds,
-- but are here since both places import this module.
--
-- Examples:
-- "untitled/b0 untitled/b0.t2 0-.25"
-- "untitled/b0 foo/bar *"
-- "untitled/b0 * *"
unparse_stack :: StackPos -> String
unparse_stack (bid, maybe_tid, maybe_range) =
    Seq.join " " [bid_s, tid_s, range_s]
    where
    bid_s = Id.show_id (Id.unpack_id bid)
    tid_s = maybe "*" (Id.show_id . Id.unpack_id) maybe_tid
    range_s = maybe "*"
        (\(from, to) -> float from ++ "-" ++ float to) maybe_range
    float = Parse.show_float (Just 2)

parse_stack :: String -> Maybe StackPos
parse_stack = Parse.maybe_parse $ do
    bid <- Parse.p_word
    tid <- optional Parse.p_word
    range <- optional $ do
        from <- Parse.p_float
        P.char '-'
        to <- Parse.p_float
        return (TrackPos from, TrackPos to)
    return (Types.BlockId (Id.read_id bid),
        fmap (Types.TrackId . Id.read_id) tid, range)
    where
    optional p = (P.char '*' >> P.spaces >> return Nothing) <|> fmap Just p
