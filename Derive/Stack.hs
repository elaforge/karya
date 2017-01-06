-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Stack (
    Stack, empty, length, from_outermost, from_innermost
    , block, call, add, member, outermost, innermost
    , block_of, track_of, region_of, call_of
    , block_track_of, block_track_region_of
    , match
    , Frame(..)
    , format_ui, pretty_ui, pretty_ui_, pretty_ui_inner
    , log_ui_frame

    -- * more specialized utils
    , track_regions

    -- * ui
    , UiFrame, to_ui, to_ui_innermost
    , unparse_ui_frame, unparse_ui_frame_, parse_ui_frame
) where
import qualified Prelude
import Prelude hiding (length)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as A
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Text.Read as Read

import Util.Crc32Instances ()
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import Global
import Types


-- | The Stack is read in both inner -> outer and outer -> inner order.  Since
-- it's always modified at the innermost end, I keep it in inner -> outer
-- order.
--
-- I originally used "Data.Sequence" but it generates more garbage and
-- I couldn't figure out how to stop that from happening.
newtype Stack = Stack [Frame]
    deriving (Eq, Ord, DeepSeq.NFData, Serialize.Serialize, CRC32.CRC32,
        Aeson.ToJSON, Aeson.FromJSON)

instance Show Stack where
    show stack = "Stack.from_outermost " ++ show (outermost stack)
instance Read.Read Stack where
    readPrec = do
        Pretty.readWord
        frames <- Read.readPrec
        return (from_outermost frames)

empty :: Stack
empty = Stack []

length :: Stack -> Int
length (Stack f) = Prelude.length f

-- | Construct a Stack from frames starting with the outermost and ending with
-- the innermost.
from_outermost :: [Frame] -> Stack
from_outermost = Stack . reverse

-- | Construct a Stack from frames starting with the innermost and ending with
-- the outermost.
from_innermost :: [Frame] -> Stack
from_innermost = Stack

-- | Make a Stack with a single block.
block :: BlockId -> Stack
block = from_innermost . (:[]) . Block

-- | Make a Stack with a single call.
call :: Text -> Stack
call = from_innermost . (:[]) . Call

-- | Add the frame to the innermost end of the stack.
add :: Frame -> Stack -> Stack
add frame (Stack stack) = Stack (frame:stack)

member :: Frame -> Stack -> Bool
member frame (Stack s) = frame `elem` s

-- | The stack, starting with the outermost call and ending with the innermost.
-- All display should use this order.
outermost :: Stack -> [Frame]
outermost (Stack s) = reverse s

-- | The stack, starting with the innermost call and ending with the outermost.
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

-- | Walk up the stack to discover the innermost TrackId, then BlockId.
block_track_of :: Stack -> Maybe (BlockId, TrackId)
block_track_of = find . innermost
    where
    find frames = do
        (track_id, frames) <- find_rest track_of frames
        (block_id, _) <- find_rest block_of frames
        return (block_id, track_id)

-- | Walk up the stack to discover the innermost Region, TrackId, then BlockId.
block_track_region_of :: Stack
    -> Maybe (BlockId, TrackId, (TrackTime, TrackTime))
block_track_region_of = find . innermost
    where
    find frames = do
        (region, frames) <- find_rest region_of frames
        (track_id, frames) <- find_rest track_of frames
        (block_id, _) <- find_rest block_of frames
        return (block_id, track_id, region)

-- | Find a value, and return the rest of the list.
find_rest :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
find_rest f = go
    where
    go (x:xs) = maybe (go xs) (\y -> Just (y, xs)) (f x)
    go [] = Nothing

-- | Nothing is a wildcard, and matches anything, but if a field is set then it
-- only matches frames where the corresponding field is set, and is equal (or
-- overlaps in the case of range).
type Pattern =
    (Maybe BlockId, Maybe (Set TrackId), Maybe (TrackTime, TrackTime))

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
    format = Pretty.formatList . outermost

instance Pretty.Pretty Frame where
    pretty (Block bid) = showt bid
    pretty (Track tid) = showt tid
    pretty (Region s e) = pretty s <> "--" <> pretty e
    pretty (Call call) = call

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
                s :: TrackTime <- Serialize.get
                e :: TrackTime <- Serialize.get
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

instance Aeson.ToJSON Frame where
    toJSON frame = Aeson.Array $ case frame of
            Block block_id -> tagged "Block" $
                Aeson.toJSON $ Id.ident_text block_id
            Track track_id -> tagged "Track" $
                Aeson.toJSON $ Id.ident_text track_id
            Region s e -> tagged "Region" $
                Aeson.toJSON (ScoreTime.to_double s, ScoreTime.to_double e)
            Call text -> tagged "Call" (Aeson.toJSON text)
        where tagged name val = Vector.fromList [Aeson.String name, val]

instance Aeson.FromJSON Frame where
    parseJSON (Aeson.Array a) = case Vector.toList a of
        [Aeson.String tag, val]
            | tag == "Block" ->
                Block . Id.BlockId . Id.read_id <$> Aeson.parseJSON val
            | tag == "Track" ->
                Track . Id.TrackId . Id.read_id <$> Aeson.parseJSON val
            | tag == "Region" ->
                uncurry Region . (ScoreTime.double *** ScoreTime.double) <$>
                    Aeson.parseJSON val
            | tag == "Call" -> Call <$> Aeson.parseJSON val
            | otherwise -> fail $ "unknown tag: " <> untxt tag
        _ -> fail "expecting two element array"
    parseJSON _ = fail "expecting array"

format_ui :: Stack -> Pretty.Doc
format_ui = Pretty.textList . map unparse_ui_frame . to_ui

pretty_ui :: Stack -> Text
pretty_ui = Text.intercalate " / " . map unparse_ui_frame . to_ui

pretty_ui_ :: Stack -> Text
pretty_ui_ = Text.intercalate " / " . map unparse_ui_frame_ . to_ui

-- | Loggable msg with the last position of the stack.
pretty_ui_inner :: Stack -> Maybe Text
pretty_ui_inner = fmap log_ui_frame . Seq.head . to_ui_innermost

-- | Format a UiFrame for logging.  This means it wraps it in @{s "..."}@,
-- which causes logview to make it clickable, which will highlight the stack
-- location.
log_ui_frame :: UiFrame -> Text
log_ui_frame frame = "{s " <> showt (unparse_ui_frame frame) <> "}"

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
to_ui = reverse . to_ui_innermost

to_ui_innermost :: Stack -> [UiFrame]
to_ui_innermost = foldr f [] . innermost
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
unparse_ui_frame :: UiFrame -> Text
unparse_ui_frame (maybe_bid, maybe_tid, maybe_range) =
    Text.intercalate " " [bid_s, tid_s, range_s]
    where
    bid_s = maybe "*" (Id.show_id . Id.unpack_id) maybe_bid
    tid_s = maybe "*" (Id.show_id . Id.unpack_id) maybe_tid
    range_s = maybe "*"
        (\(from, to) -> float from <> "-" <> float to) maybe_range
    float = Num.showFloat 2 . ScoreTime.to_double

-- | This is like 'unparse_ui_frame' except it omits the namespaces for a less
-- cluttered but potentially ambiguous output.
unparse_ui_frame_ :: UiFrame -> Text
unparse_ui_frame_ (maybe_bid, maybe_tid, maybe_range) =
    Text.unwords [bid_s, tid_s, range_s]
    where
    bid_s = maybe "*" Id.ident_name maybe_bid
    tid_s = maybe "*" Id.ident_name maybe_tid
    range_s = maybe "*"
        (\(from, to) -> float from <> "-" <> float to) maybe_range
    float = Num.showFloat 2 . ScoreTime.to_double

parse_ui_frame :: String -> Maybe UiFrame
parse_ui_frame = ParseText.maybe_parse_string $ do
    bid <- optional $ ParseText.lexeme ParseText.p_word
    tid <- optional $ ParseText.lexeme ParseText.p_word
    range <- optional $ do
        from <- ParseText.p_float
        A.char '-'
        to <- ParseText.p_float
        return (ScoreTime.double from, ScoreTime.double to)
    return
        ( Id.BlockId . Id.read_id <$> bid
        , Id.TrackId . Id.read_id <$> tid
        , range
        )
    where
    optional p = (A.char '*' >> A.skipSpace >> return Nothing)
        <|> fmap Just p
