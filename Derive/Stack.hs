-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Derive.Stack (
    Stack, empty, length, from_outermost, from_innermost
    , block, call, add, member, outermost, innermost
    , block_of, track_of, region_of, call_of
    , block_track_of
    , match
    , Frame(..)
    , format_ui, show_ui, show_ui_
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
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Set as Set

import qualified Text.Read as Read

import Util.Control
import Util.Crc32Instances ()
import qualified Util.ParseBs as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

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
newtype Stack = Stack [Frame]
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize, CRC32.CRC32)

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

-- | Make a Stack with a single call.
call :: Text -> Stack
call = from_innermost . (:[]) . Call

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

region_of :: Frame -> Maybe (TrackTime, TrackTime)
region_of (Region s e) = Just (s, e)
region_of _ = Nothing

call_of :: Frame -> Maybe Text
call_of (Call s) = Just s
call_of _ = Nothing

-- | Walk up the stack to discover the innermost BlockId and TrackId.
block_track_of :: Stack -> Maybe (BlockId, TrackId)
block_track_of = go Nothing . innermost
    where
    go _ [] = Nothing
    go Nothing (Track track_id : rest) = go (Just track_id) rest
    go Nothing (_ : rest) = go Nothing rest
    go (Just track_id) (Block block_id : _) = Just (block_id, track_id)
    go track_id (_ : rest) = go track_id rest

-- | Nothing is a wildcard, and matches anything, but if a field is set then it
-- only matches frames where the corresponding field is set, and is equal (or
-- overlaps in the case of range).
type Pattern =
    (Maybe BlockId, Maybe (Set.Set TrackId), Maybe (TrackTime, TrackTime))

match :: Pattern -> Stack -> Bool
match pattern = any (ui_match pattern) . to_ui

-- ** frames

data Frame =
    Block !BlockId
    | Track !TrackId
    | Region !TrackTime !TrackTime
    | Call !Text
    deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData Frame where
    rnf f = f `seq` ()

instance Pretty.Pretty Stack where
    format = Pretty.format_list . outermost

instance Pretty.Pretty Frame where
    pretty (Block bid) = show bid
    pretty (Track tid) = show tid
    pretty (Region s e) = Pretty.pretty s ++ "--" ++ Pretty.pretty e
    pretty (Call call) = untxt call

instance Serialize.Serialize Frame where
    put frame = case frame of
        Block bid -> Serialize.put_tag 0 >> Serialize.put bid
        Track tid -> Serialize.put_tag 1 >> Serialize.put tid
        Region s e -> Serialize.put_tag 2 >> Serialize.put s >> Serialize.put e
        Call s -> Serialize.put_tag 4 >> Serialize.put s
    get = do
        tag <- Serialize.get_tag
        case tag of
            0 -> do
                bid :: BlockId <- Serialize.get
                return $ Block bid
            1 -> do
                tid :: TrackId <- Serialize.get
                return $ Track tid
            2 -> do
                s :: ScoreTime <- Serialize.get
                e :: ScoreTime <- Serialize.get
                return $ Region s e
            3 -> do
                s :: String <- Serialize.get
                return $ Call (txt s)
            4 -> do
                s :: Text <- Serialize.get
                return $ Call s
            _ -> Serialize.bad_tag "Stack.Frame" tag

instance CRC32.CRC32 Frame where
    crc32Update n (Block block_id) = n `CRC32.crc32Update` block_id
    crc32Update n (Track track_id) = n + 1 `CRC32.crc32Update` track_id
    crc32Update n (Region s e) =
        n + 2 `CRC32.crc32Update` s `CRC32.crc32Update` e
    crc32Update n (Call call) = n + 3 `CRC32.crc32Update` call

format_ui :: Stack -> Pretty.Doc
format_ui = Pretty.text_list . map unparse_ui_frame . to_ui

show_ui :: Stack -> String
show_ui = Seq.join ": " . map unparse_ui_frame . to_ui

show_ui_ :: Stack -> String
show_ui_ = Seq.join ": " . map unparse_ui_frame_ . to_ui

-- | Serialize a Stack to and from a list of strings, as used in
-- 'Util.Log.Msg'.  Since I use a list of Strings instead of a String, I can
-- conceal that internally the stack is stored innermost first.
to_strings :: Stack -> [String]
to_strings = map show . outermost

-- | Turn strings back into a stack.  This uses 'read' so it WILL CRASH if
-- the input isn't parseable.
from_strings :: [String] -> Stack
from_strings = from_outermost . map read

-- * more specialized utils

-- | Get the Regions associated with a track in a given stack.  It's a little
-- tricky because track level calls will go in between the track and the
-- region, e.g. [track, call, call, region].
track_regions :: Stack -> TrackId -> [Ranges.Ranges TrackTime]
track_regions stack track_id =
    [Ranges.range s e | (_:rest) <- grps, (s, e) <- get_region rest ]
    where
    grps = Seq.split_with (== Track track_id) (outermost stack)
    get_region frames = case dropWhile is_call frames of
        Region s e : _ -> [(s, e)]
        _ -> []
    is_call (Call {}) = True
    is_call _ = False
    -- find [track, call*, region] where the region overlaps

-- * ui

ui_match :: Pattern -> UiFrame -> Bool
ui_match (bid_pattern, tids_pattern, range_pattern) (bid, tid, range) = and
    [ maybe True ((==bid) . Just) bid_pattern
    , case (tids_pattern, tid) of
        (Just tids, Just tid) -> Set.member tid tids
        (Nothing, _) -> True
        (_, Nothing) -> False
    , maybe True overlaps range_pattern
    ]
    where
    overlaps (s, e) = case range of
        Nothing -> False
        Just (start, end) -> not (end <= s || start >= e)

-- | This is an abbreviation of the stack that only has elements that are
-- visible in the UI.
--
-- @(block_id, track_id, (event_start, event_end))@
type UiFrame = (Maybe BlockId, Maybe TrackId, Maybe (TrackTime, TrackTime))

-- | UiFrames are returned in outermost to innermost order.
to_ui :: Stack -> [UiFrame]
to_ui stack = reverse $ foldr f [] (innermost stack)
    where
    f (Block bid) accum = (Just bid, Nothing, Nothing) : accum
    f (Track tid) ((bid, _, _) : rest) = (bid, Just tid, Nothing) : rest
    f (Region s e) ((bid, tid@(Just _), _) : rest) =
        (bid, tid, Just (s, e)) : rest
    f _ accum = accum

-- | These functions are used by LogView and Cmd.Repl.*, but are here since
-- both places import this module.  Examples:
--
-- > "untitled/b0 untitled/b0.t2 0-.25"
-- > "untitled/b0 foo/bar *"
-- > "untitled/b0 * *"
unparse_ui_frame :: UiFrame -> String
unparse_ui_frame (maybe_bid, maybe_tid, maybe_range) =
    Seq.join " " [bid_s, tid_s, range_s]
    where
    bid_s = maybe "*" (Id.show_id . Id.unpack_id) maybe_bid
    tid_s = maybe "*" (Id.show_id . Id.unpack_id) maybe_tid
    range_s = maybe "*"
        (\(from, to) -> float from ++ "-" ++ float to) maybe_range
    float = Pretty.show_float 2 . ScoreTime.to_double

-- | This is like 'unparse_ui_frame' except it omits the namespaces for a less
-- cluttered but potentially ambiguous output.
unparse_ui_frame_ :: UiFrame -> String
unparse_ui_frame_ (maybe_bid, maybe_tid, maybe_range) =
    Seq.join " " [bid_s, tid_s, range_s]
    where
    bid_s = maybe "*" Id.ident_name maybe_bid
    tid_s = maybe "*" Id.ident_name maybe_tid
    range_s = maybe "*"
        (\(from, to) -> float from ++ "-" ++ float to) maybe_range
    float = Pretty.show_float 2 . ScoreTime.to_double

parse_ui_frame :: String -> Maybe UiFrame
parse_ui_frame = Parse.maybe_parse_string $ do
    bid <- optional $ Parse.lexeme Parse.p_word
    tid <- optional $ Parse.lexeme Parse.p_word
    range <- optional $ do
        from <- Parse.p_float
        Parse.char '-'
        to <- Parse.p_float
        return (ScoreTime.double from, ScoreTime.double to)
    return
        ( Types.BlockId . Id.read_id . B.unpack <$> bid
        , Types.TrackId . Id.read_id . B.unpack <$> tid
        , range
        )
    where
    optional p = (Parse.char '*' >> Parse.spaces >> return Nothing)
        <|> fmap Just p
