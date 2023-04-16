-- | Functions to produce text diffs.
module Util.Diffs (
    ColorCode(..)
    , CharRange
    , colored1, colored2
    , highlightLines
    , ranges
    , Numbered(..)
    , numberedDiff
) where
import qualified Data.Algorithm.Diff as Diff
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text (Text)

import qualified Util.Maps as Maps
import qualified Util.Ranges as Ranges
import qualified Util.Lists as Lists


newtype ColorCode = ColorCode Text
    deriving (Show)

type CharRange = (Int, Int)

-- | Only show the "from" side of the diff, if it's just deletion then
-- that will be enough.
colored1 :: Text -> Text -> Text
colored1 a b = Text.unlines $ highlightLines redColor diffA $ Text.lines a
    where (diffA, _diffB) = ranges a b

colored2 :: Text -> Text -> Text
colored2 a b =
    fmtLines "->"
        (highlightLines redColor diffA $ Text.lines a)
        (highlightLines redColor diffB $ Text.lines b)
    where (diffA, diffB) = ranges a b

fmtLines :: Text -> [Text] -> [Text] -> Text
fmtLines operator xs ys = ("\n"<>) $ Text.stripEnd $
    Text.unlines $ xs <> ["    " <> operator] <> ys

highlight :: ColorCode -> Text -> Text
highlight (ColorCode code) text
    | Text.null text = text
    | otherwise = code <> text <> vt100Normal

-- | These codes should probably come from termcap, but I can't be bothered.
redColor :: ColorCode
redColor = ColorCode "\ESC[31m" -- red

vt100Normal :: Text
vt100Normal = "\ESC[m\ESC[m"

-- | Apply color ranges as produced by 'ranges'.
highlightLines :: ColorCode -> IntMap.IntMap [CharRange] -> [Text]
    -> [Text]
highlightLines color nums = zipWith hi [0..]
    where
    hi i line = case IntMap.lookup i nums of
        Just ranges -> highlightRanges color ranges line
        Nothing -> line

highlightRanges :: ColorCode -> [CharRange] -> Text -> Text
highlightRanges color ranges = mconcat . map hi . splitRanges ranges
    where hi (outside, inside) = outside <> highlight color inside

splitRanges :: [(Int, Int)] -> Text -> [(Text, Text)] -- ^ (out, in) pairs
splitRanges = go 0
    where
    go _ [] text
        | Text.null text = []
        | otherwise = [(text, mempty)]
    go prev ((s, e) : ranges) text = (pre, within) : go e ranges post
        where
        (pre, rest) = Text.splitAt (s-prev) text
        (within, post) = Text.splitAt (e - s) rest

ranges :: Text -> Text
    -> (IntMap.IntMap [CharRange], IntMap.IntMap [CharRange])
ranges first second =
    toMap $ Lists.partitionPaired $ map diffLine $
        Maps.pairs firstByLine secondByLine
    where
    toMap (as, bs) = (IntMap.fromList as, IntMap.fromList bs)
    diffLine (num, d) = case d of
        Lists.Both line1 line2 -> Lists.Both (num, d1) (num, d2)
            where (d1, d2) = char line1 line2
        Lists.First line1 -> Lists.First (num, [(0, Text.length line1)])
        Lists.Second line2 -> Lists.Second (num, [(0, Text.length line2)])
    firstByLine = Map.fromList
        [(n, text) | Diff.First (Numbered n text) <- diffs]
    secondByLine = Map.fromList
        [(n, text) | Diff.Second (Numbered n text) <- diffs]
    diffs = numberedDiff (==) (Text.lines first) (Text.lines second)

char :: Text -> Text -> ([CharRange], [CharRange])
char first second
    | tooDifferent firstCs || tooDifferent secondCs =
        ([(0, Text.length first)], [(0, Text.length second)])
    | otherwise = (firstCs, secondCs)
    where
    firstCs = toRanges [n | Diff.First (Numbered n _) <- diffs]
    secondCs = toRanges [n | Diff.Second (Numbered n _) <- diffs]
    diffs = numberedDiff (==) (Text.unpack first) (Text.unpack second)
    -- If there are too many diff ranges let's just mark the whole thing
    -- different.  Perhaps I should ignore spaces that are the same, but let's
    -- see how this work first.
    tooDifferent ranges = length ranges > 2

toRanges :: [Int] -> [(Int, Int)]
toRanges xs = Ranges.merge_sorted [(n, n+1) | n <- xs]

data Numbered a = Numbered {
    numbered :: !Int
    , numberedVal :: !a
    } deriving (Show)

numberedDiff :: (a -> a -> Bool) -> [a] -> [a] -> [Diff.Diff (Numbered a)]
numberedDiff equal a b =
    Diff.getDiffBy (\a b -> numberedVal a `equal` numberedVal b)
        (number a) (number b)
    where number = zipWith Numbered [0..]
