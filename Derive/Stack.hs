{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Derive.Stack (
    Stack, empty, make, add, drop, length, member, outermost, innermost
    , Frame(..)

    -- * more specialized utils
    , track_regions

    -- * ui
    , UiFrame, to_ui, unparse_ui_frame, parse_ui_frame
) where
import Prelude hiding (drop, length)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Sequence as Sequence
import qualified Data.Foldable as Foldable
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import qualified Util.Parse as Parse
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Id as Id
import qualified Ui.Types as Types


newtype Stack = Stack (Sequence.Seq Frame)
    deriving (Eq, Ord, Read, Show)

instance DeepSeq.NFData Stack where
    rnf _ = () -- TODO

empty :: Stack
empty = Stack Sequence.empty

make :: [Frame] -> Stack
make = Stack . Sequence.fromList

add :: Frame -> Stack -> Stack
add frame (Stack stack) = Stack (stack Sequence.|> frame)

drop :: Stack -> Stack
drop (Stack stack) = Stack (Sequence.drop 1 stack)

length :: Stack -> Int
length (Stack stack) = Sequence.length stack

member :: Frame -> Stack -> Bool
member frame stack = frame `elem` outermost stack

outermost :: Stack -> [Frame]
outermost (Stack stack) = Foldable.foldr (:) [] stack

innermost :: Stack -> [Frame]
innermost (Stack stack) = Foldable.foldl (flip (:)) [] stack

data Frame =
    Block BlockId
    | Track TrackId
    | Region ScoreTime ScoreTime
    | Call String
    deriving (Eq, Ord, Read, Show)

instance Pretty.Pretty Stack where
    pretty stack = "[" ++ Seq.join " / " (map f (outermost stack)) ++ "]"
        where
        f (Block bid) = show bid
        f (Track tid) = show tid
        f (Region s e) = Pretty.pretty s ++ "--" ++ Pretty.pretty e
        f (Call call) = call

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

to_ui :: Stack -> [UiFrame]
to_ui (Stack stack) = reverse $ Foldable.foldl f [] stack
    where
    f accum (Block bid) = (bid, Nothing, Nothing) : accum
    f ((bid, _, _) : rest) (Track tid) = (bid, Just tid, Nothing) : rest
    f ((bid, tid@(Just _), _) : rest) (Region s e) =
        (bid, tid, Just (s, e)) : rest
    f accum _ = accum

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
    float = Parse.show_float (Just 2)

parse_ui_frame :: String -> Maybe UiFrame
parse_ui_frame = Parse.maybe_parse $ do
    bid <- Parse.p_word
    tid <- optional Parse.p_word
    range <- optional $ do
        from <- Parse.p_float
        P.char '-'
        to <- Parse.p_float
        return (ScoreTime from, ScoreTime to)
    return (Types.BlockId (Id.read_id bid),
        fmap (Types.TrackId . Id.read_id) tid, range)
    where
    optional p = (P.char '*' >> P.spaces >> return Nothing) <|> fmap Just p
