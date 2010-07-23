{-# LANGUAGE OverloadedStrings #-}
-- | Columnize .prof output, since ghc doesn't make it line up nicely.
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char


type Word = B.ByteString

main = do
    lines <- fmap B.lines B.getContents
    let (pre, (header:post)) = break is_cost_line lines
    mapM_ B.putStrLn pre
    B.putStrLn header
    mapM_ B.putStrLn (columnize 4 (map split_words post))

is_cost_line s = "COST CENTRE" `B.isPrefixOf` s && "no." `B.isInfixOf` s

-- | Split words, but keep the indentation on the first word.
split_words :: B.ByteString -> [Word]
split_words s = w : drop 1 ws
    where
    (leading, rest) = B.span Char.isSpace s
    w = leading `B.append` B.takeWhile (not . Char.isSpace) rest
    ws = B.words s

columnize :: Int -> [[Word]] -> [B.ByteString]
columnize spacing lines = map respace lines
    where
    widths = map maximum $ rotate 0 $ map (map B.length) lines
    respace ws = fst $ B.spanEnd Char.isSpace $ B.concat $
        map (uncurry pad) (zip ws widths)
    pad word cols =
        word `B.append` B.replicate (cols - len + spacing) ' '
        where len = B.length word

-- | Rotate a list of rows into a list of columns, filling in missing tails
-- with 'deflt'.
rotate :: a -> [[a]] -> [[a]]
rotate deflt groups
    | all null groups = []
    | otherwise = heads : rotate deflt tails
    where
    (heads, tails) = unzip (map uncons groups)
    uncons [] = (deflt, [])
    uncons (x:xs) = (x, xs)
