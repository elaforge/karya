-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Derive.Stack (
    Stack, empty, length, fromOutermost, fromInnermost
    , block, call, add, member, outermost, innermost
    , blockOf, trackOf, regionOf, callOf
    , blockTrackOf, blockTracksOf, blockTrackRegionOf
    , trackIdsOf
    , match
    , Frame(..), Serial
    , formatUi, prettyUi, prettyUi_, prettyUiInner
    , logUiFrame

    -- * more specialized utils
    , trackRegions

    -- * ui
    , UiFrame, toUi, toUiInnermost
    , unparseUiFrame, unparseUiFrame_, parseUiFrame
) where
import qualified Prelude
import           Prelude hiding (length)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as A
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Text.Read as Read

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.ParseText as ParseText
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Seed as Seed
import qualified Util.Serialize as Serialize

import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


-- | The Stack is read in both inner -> outer and outer -> inner order.  Since
-- it's always modified at the innermost end, I keep it in inner -> outer
-- order.
--
-- I originally used "Data.Sequence" but it generates more garbage and
-- I couldn't figure out how to stop that from happening.
newtype Stack = Stack [Frame]
    deriving stock (Eq, Ord)
    deriving newtype (DeepSeq.NFData, Serialize.Serialize, Aeson.ToJSON,
        Aeson.FromJSON)

instance Show Stack where
    show stack = "Stack.from_outermost " ++ show (outermost stack)
instance Read.Read Stack where
    readPrec = do
        Pretty.readWord
        frames <- Read.readPrec
        pure (fromOutermost frames)

empty :: Stack
empty = Stack []

length :: Stack -> Int
length (Stack f) = Prelude.length f

-- | Construct a Stack from frames starting with the outermost and ending with
-- the innermost.
fromOutermost :: [Frame] -> Stack
fromOutermost = Stack . reverse

-- | Construct a Stack from frames starting with the innermost and ending with
-- the outermost.
fromInnermost :: [Frame] -> Stack
fromInnermost = Stack

-- | Make a Stack with a single block.
block :: BlockId -> Stack
block = fromInnermost . (:[]) . Block

-- | Make a Stack with a single call.
call :: Text -> Stack
call = fromInnermost . (:[]) . Call

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

blockOf :: Frame -> Maybe BlockId
blockOf (Block b) = Just b
blockOf _ = Nothing

trackOf :: Frame -> Maybe TrackId
trackOf (Track t) = Just t
trackOf _ = Nothing

regionOf :: Frame -> Maybe (TrackTime, TrackTime)
regionOf (Region s e) = Just (s, e)
regionOf _ = Nothing

callOf :: Frame -> Maybe Text
callOf (Call s) = Just s
callOf _ = Nothing

-- | Walk up the stack to discover the innermost TrackId, then BlockId.
blockTrackOf :: Stack -> Maybe (BlockId, TrackId)
blockTrackOf = find . innermost
    where
    find frames = do
        (trackId, frames) <- findRest trackOf frames
        (blockId, _) <- findRest blockOf frames
        pure (blockId, trackId)

-- | Get each block and the tracks under it, starting from the innermost.
blockTracksOf :: Stack -> [(BlockId, [TrackId])]
blockTracksOf = go [] . innermost
    where
    go trackIds (frame : frames) = case frame of
        Track trackId -> go (trackId : trackIds) frames
        Block blockId -> (blockId, trackIds) : go [] frames
        _ -> go trackIds frames
    go _ [] = []

-- | Walk up the stack to discover the innermost Region, TrackId, then BlockId.
blockTrackRegionOf :: Stack
    -> Maybe (BlockId, TrackId, (TrackTime, TrackTime))
blockTrackRegionOf = find . innermost
    where
    find frames = do
        (region, frames) <- findRest regionOf frames
        (trackId, frames) <- findRest trackOf frames
        (blockId, _) <- findRest blockOf frames
        pure (blockId, trackId, region)

trackIdsOf :: Stack -> [TrackId]
trackIdsOf = mapMaybe trackOf . innermost

-- | Find a value, and return the rest of the list.
findRest :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
findRest f = go
    where
    go (x:xs) = maybe (go xs) (\y -> Just (y, xs)) (f x)
    go [] = Nothing

-- | Nothing is a wildcard, and matches anything, but if a field is set then it
-- only matches frames where the corresponding field is set, and is equal (or
-- overlaps in the case of range).
type Pattern =
    (Maybe BlockId, Maybe (Set TrackId), Maybe (TrackTime, TrackTime))

match :: Pattern -> Stack -> Bool
match pattern = any (uiMatch pattern) . toUi

-- ** frames

data Frame
    = Block !BlockId
    | Track !TrackId
    | Region !TrackTime !TrackTime
    | Call !Text
    | Serial !Serial
    deriving stock (Eq, Ord, Read, Show)

-- | The 'Stack' is used as a unique key for a unique call of a generator.
-- For instance, the cache uses it to cache generator output, and the random
-- mechanism uses it to permute 'Derive.EnvKey.seed'.  Since a single track
-- event may call multiple generators internally, each one is given a unique
-- serial number.
type Serial = Int

instance DeepSeq.NFData Frame where
    rnf f = f `seq` ()

instance Pretty Stack where
    format = Pretty.formatList . outermost

instance Pretty Frame where
    pretty (Block bid) = showt bid
    pretty (Track tid) = showt tid
    pretty (Region s e) = pretty s <> "--" <> pretty e
    pretty (Call call) = call
    pretty (Serial n) = pretty n

instance Serialize.Serialize Frame where
    put frame = case frame of
        Block bid -> Serialize.putTag 0 >> Serialize.put bid
        Track tid -> Serialize.putTag 1 >> Serialize.put tid
        Region s e -> Serialize.putTag 2 >> Serialize.put s >> Serialize.put e
        Call s -> Serialize.putTag 4 >> Serialize.put s
        Serial n -> Serialize.putTag 5 >> Serialize.put n
    get = do
        tag <- Serialize.getTag
        case tag of
            0 -> do
                bid :: BlockId <- Serialize.get
                pure $ Block bid
            1 -> do
                tid :: TrackId <- Serialize.get
                pure $ Track tid
            2 -> do
                s :: TrackTime <- Serialize.get
                e :: TrackTime <- Serialize.get
                pure $ Region s e
            3 -> do
                s :: String <- Serialize.get
                pure $ Call (txt s)
            4 -> do
                s :: Text <- Serialize.get
                pure $ Call s
            5 -> do
                n :: Int <- Serialize.get
                pure $ Serial n
            _ -> Serialize.badTag "Stack.Frame" tag

instance Seed.Seed Frame where
    toSeed n = \case
        Block blockId -> n Seed.& blockId
        Track trackId -> n + 1 Seed.& trackId
        Region s e -> n + 2 Seed.& s Seed.& e
        Call call -> n + 3 Seed.& call
        Serial i -> n + 4 Seed.& i

instance Aeson.ToJSON Frame where
    toJSON frame = Aeson.Array $ case frame of
            Block blockId -> tagged "Block" $
                Aeson.toJSON $ Id.identText blockId
            Track trackId -> tagged "Track" $
                Aeson.toJSON $ Id.identText trackId
            Region s e -> tagged "Region" $
                Aeson.toJSON (ScoreTime.toDouble s, ScoreTime.toDouble e)
            Call text -> tagged "Call" (Aeson.toJSON text)
            Serial n -> tagged "Serial" (Aeson.toJSON n)
        where tagged name val = Vector.fromList [Aeson.String name, val]

instance Aeson.FromJSON Frame where
    parseJSON (Aeson.Array a) = case Vector.toList a of
        [Aeson.String tag, val]
            | tag == "Block" ->
                Block . Id.BlockId . Id.readId <$> Aeson.parseJSON val
            | tag == "Track" ->
                Track . Id.TrackId . Id.readId <$> Aeson.parseJSON val
            | tag == "Region" -> uncurry Region
                . bimap ScoreTime.fromDouble ScoreTime.fromDouble <$>
                Aeson.parseJSON val
            | tag == "Call" -> Call <$> Aeson.parseJSON val
            | tag == "Serial" -> Serial <$> Aeson.parseJSON val
            | otherwise -> fail $ "unknown tag: " <> untxt tag
        _ -> fail "expecting two element array"
    parseJSON _ = fail "expecting array"

formatUi :: Stack -> Pretty.Doc
formatUi = Pretty.textList . map unparseUiFrame . toUi

prettyUi :: Stack -> Text
prettyUi = Text.intercalate " / " . map unparseUiFrame . toUi

prettyUi_ :: Stack -> Text
prettyUi_ = Text.intercalate " / " . map unparseUiFrame_ . toUi

-- | Loggable msg with the last position of the stack.
prettyUiInner :: Stack -> Maybe Text
prettyUiInner = fmap logUiFrame . Lists.head . toUiInnermost

-- | Format a UiFrame for logging.  This means it wraps it in @{s "..."}@,
-- which causes logview to make it clickable, which will highlight the stack
-- location.
logUiFrame :: UiFrame -> Text
logUiFrame frame = "{s " <> showt (unparseUiFrame frame) <> "}"

-- * more specialized utils

-- | Get the Regions associated with a track in a given stack.  It's a little
-- tricky because track level calls will go in between the track and the
-- region, e.g. [track, call, call, region].
trackRegions :: Stack -> TrackId -> [Ranges.Ranges TrackTime]
trackRegions stack trackId =
    [Ranges.range s e | _ : rest <- groups, (s, e) <- getRegion rest ]
    where
    groups = Lists.splitBefore (== Track trackId) (outermost stack)
    getRegion frames = case dropWhile isCall frames of
        Region s e : _ -> [(s, e)]
        _ -> []
    isCall (Call {}) = True
    isCall _ = False
    -- find [track, call*, region] where the region overlaps

-- * ui

uiMatch :: Pattern -> UiFrame -> Bool
uiMatch (bidPattern, tidsPattern, rangePattern) (bid, tid, range) = and
    [ maybe True ((==bid) . Just) bidPattern
    , case (tidsPattern, tid) of
        (Just tids, Just tid) -> Set.member tid tids
        (Nothing, _) -> True
        (_, Nothing) -> False
    , maybe True overlaps rangePattern
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
toUi :: Stack -> [UiFrame]
toUi = reverse . toUiInnermost

toUiInnermost :: Stack -> [UiFrame]
toUiInnermost = foldr f [] . innermost
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
unparseUiFrame :: UiFrame -> Text
unparseUiFrame (maybeBid, maybeTid, maybeRange) =
    Text.intercalate " " [bidS, tidS, rangeS]
    where
    bidS = maybe "*" (Id.showId . Id.unpackId) maybeBid
    tidS = maybe "*" (Id.showId . Id.unpackId) maybeTid
    rangeS = maybe "*"
        (\(from, to) -> float from <> "-" <> float to) maybeRange
    float = Num.showFloat 2 . ScoreTime.toDouble

-- | This is like 'unparse_ui_frame' except it omits the namespaces for a less
-- cluttered but potentially ambiguous output.
unparseUiFrame_ :: UiFrame -> Text
unparseUiFrame_ (maybeBid, maybeTid, maybeRange) =
    Text.unwords [bidS, tidS, rangeS]
    where
    bidS = maybe "*" Id.identName maybeBid
    tidS = maybe "*" Id.identName maybeTid
    rangeS = maybe "*"
        (\(from, to) -> float from <> "-" <> float to) maybeRange
    float = Num.showFloat 2 . ScoreTime.toDouble

parseUiFrame :: String -> Maybe UiFrame
parseUiFrame = ParseText.maybeParseString $ do
    bid <- optional $ ParseText.lexeme ParseText.pWord
    tid <- optional $ ParseText.lexeme ParseText.pWord
    range <- optional $ do
        from <- ParseText.pFloat
        A.char '-'
        to <- ParseText.pFloat
        pure (ScoreTime.fromDouble from, ScoreTime.fromDouble to)
    pure
        ( Id.BlockId . Id.readId <$> bid
        , Id.TrackId . Id.readId <$> tid
        , range
        )
    where
    optional p = (A.char '*' >> A.skipSpace >> pure Nothing)
        <|> fmap Just p
