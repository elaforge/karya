{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Derive.Stack (
    Stack, empty, length, from_outermost, from_innermost
    , block, add, member, outermost, innermost
    , block_of, track_of, region_of, call_of
    , Frame(..)
    , format_ui, show_ui
    , to_strings, from_strings

    -- * more specialized utils
    , track_regions

    -- * ui
    , UiFrame, to_ui, unparse_ui_frame, unparse_ui_frame_, parse_ui_frame
) where
import qualified Prelude
import Prelude hiding (length)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Char8 as B
import qualified Text.Read as Read

import Util.Control
import qualified Util.ParseBs as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Types as Types

import Types


-- | The Stack is read in both inner -> outer and outer -> inner order.  Since
-- it's always modified at the innermost end, I keep it in inner -> outer
-- order.
--
-- I originally used "Data.Sequence" but it generates more garbage and
-- I couldn't figure out how to stop that from happening.
newtype Stack = Stack [Frame] deriving (Eq, Ord, DeepSeq.NFData)

instance Show Stack where
    show stack = "Stack.from_outermost " ++ show (outermost stack)
instance Read.Read Stack where
    readPrec = do
        Pretty.read_word
        frames <- Read.readPrec
        return (from_outermost frames)

empty :: Stack
empty = Stack []

length :: Stack -> Int
length (Stack f) = Prelude.length f

from_outermost :: [Frame] -> Stack
from_outermost = Stack . reverse

from_innermost :: [Frame] -> Stack
from_innermost = Stack

-- | Make a Stack with a single block.
block :: BlockId -> Stack
block = from_innermost . (:[]) . Block

add :: Frame -> Stack -> Stack
add frame (Stack stack) = Stack (frame:stack)

member :: Frame -> Stack -> Bool
member frame (Stack s) = frame `elem` s

outermost :: Stack -> [Frame]
outermost (Stack s) = reverse s

innermost :: Stack -> [Frame]
innermost (Stack s) = s

block_of :: Frame -> Maybe BlockId
block_of (Block b) = Just b
block_of _ = Nothing

track_of :: Frame -> Maybe TrackId
track_of (Track t) = Just t
track_of _ = Nothing

region_of :: Frame -> Maybe (ScoreTime, ScoreTime)
region_of (Region s e) = Just (s, e)
region_of _ = Nothing

call_of :: Frame -> Maybe String
call_of (Call s) = Just s
call_of _ = Nothing

data Frame =
    Block BlockId
    | Track TrackId
    | Region ScoreTime ScoreTime
    | Call String
    deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData Frame where
    rnf f = case f of
        Block bid -> bid `seq` ()
        Track tid -> tid `seq` ()
        Region s e -> s `seq` e `seq` ()
        Call s -> s `seq` ()

instance Pretty.Pretty Stack where
    format = Pretty.format_list . outermost

instance Pretty.Pretty Frame where
    pretty (Block bid) = show bid
    pretty (Track tid) = show tid
    pretty (Region s e) = Pretty.pretty s ++ "--" ++ Pretty.pretty e
    pretty (Call call) = call

format_ui :: Stack -> Pretty.Doc
format_ui = Pretty.text_list . map unparse_ui_frame . to_ui

show_ui :: Stack -> String
show_ui = Seq.join ": " . map unparse_ui_frame . to_ui

-- | Serialize a Stack to and from a list of strings, as used in
-- 'Util.Log.Msg'.  Since I use a list of Strings instead of a String, I can
-- conceal that internally the stack is stored innermost first.
to_strings :: Stack -> [String]
to_strings = map show . outermost

from_strings :: [String] -> Stack
from_strings = from_outermost . map read

-- * more specialized utils

-- | Get the Regions associated with a track in a given stack.  It's a little
-- tricky because track level calls will go in between the track and the
-- region, e.g. [track, call, call, region].
track_regions :: Stack -> TrackId -> [Ranges.Ranges ScoreTime]
track_regions stack track_id =
    [Ranges.range s e | (_:rest) <- grps, (s, e) <- get_region rest ]
    where
    grps = Seq.split_with (== Track track_id) (outermost stack)
    get_region frames = case dropWhile is_call frames of
        (Region s e) : _ -> [(s, e)]
        _ -> []
    is_call (Call {}) = True
    is_call _ = False
    -- find [track, call*, region] where the region overlaps

-- * ui

-- | This is an abbreviation of the stack that only has elements that are
-- visible in the UI.
--
-- @(block_id, track_id, (event_start, event_end))@
type UiFrame = (BlockId, Maybe TrackId, Maybe (ScoreTime, ScoreTime))

-- | UiFrames are returned in outermost to innermost order.
to_ui :: Stack -> [UiFrame]
to_ui stack = reverse $ foldr f [] (innermost stack)
    where
    f (Block bid) accum = (bid, Nothing, Nothing) : accum
    f (Track tid) ((bid, _, _) : rest) = (bid, Just tid, Nothing) : rest
    f (Region s e) ((bid, tid@(Just _), _) : rest) =
        (bid, tid, Just (s, e)) : rest
    f _ accum = accum

-- | These functions are used by LogView and Cmd.Lang.*, but are here since
-- both places import this module.  Examples:
--
-- > "untitled/b0 untitled/b0.t2 0-.25"
-- > "untitled/b0 foo/bar *"
-- > "untitled/b0 * *"
unparse_ui_frame :: UiFrame -> String
unparse_ui_frame (bid, maybe_tid, maybe_range) =
    Seq.join " " [bid_s, tid_s, range_s]
    where
    bid_s = Id.show_id (Id.unpack_id bid)
    tid_s = maybe "*" (Id.show_id . Id.unpack_id) maybe_tid
    range_s = maybe "*"
        (\(from, to) -> float from ++ "-" ++ float to) maybe_range
    float = Pretty.show_float 2 . ScoreTime.to_double

-- | This is like 'unparse_ui_frame' except it omits the namespaces for a less
-- cluttered but potentially ambiguous output.
unparse_ui_frame_ :: UiFrame -> String
unparse_ui_frame_ (bid, maybe_tid, maybe_range) =
    Seq.join " " [bid_s, tid_s, range_s]
    where
    bid_s = Id.ident_name bid
    tid_s = maybe "*" Id.ident_name maybe_tid
    range_s = maybe "*"
        (\(from, to) -> float from ++ "-" ++ float to) maybe_range
    float = Pretty.show_float 2 . ScoreTime.to_double

parse_ui_frame :: String -> Maybe UiFrame
parse_ui_frame = Parse.maybe_parse_string $ do
    bid <- Parse.lexeme Parse.p_word
    tid <- optional (Parse.lexeme Parse.p_word)
    range <- optional $ do
        from <- Parse.p_float
        Parse.char '-'
        to <- Parse.p_float
        return (ScoreTime.double from, ScoreTime.double to)
    return (Types.BlockId (Id.read_id (B.unpack bid)),
        fmap (Types.TrackId . Id.read_id . B.unpack) tid, range)
    where
    optional p = (Parse.char '*' >> Parse.spaces >> return Nothing)
        <|> fmap Just p
