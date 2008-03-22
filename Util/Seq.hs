module Util.Seq
where

import qualified Char
import List

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn key = sortBy (\a b -> compare (key a) (key b))

rDropWhile f = reverse . dropWhile f . reverse

lstrip = dropWhile Char.isSpace
rstrip = rDropWhile Char.isSpace
strip = lstrip . rstrip

-- | Split 'xs' before places where 'f' matches.
-- So "splitWith (==1) [1,2,1]" is "[[1,2][1]]".
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = map reverse (doSplit f xs [])
    where
    doSplit f [] collect = [collect]
    doSplit f (x:xs) collect
        | f x = collect : doSplit f xs [x]
        | otherwise = doSplit f xs (x:collect)

splitSeqWith :: ([a] -> Maybe Int) -> [a] -> [[a]]
splitSeqWith f [] = []
splitSeqWith f xs = pre : ap_head (matched++) (splitSeqWith f rest)
    where
    (pre, post, n) = doSplitSeqWith f xs
    (matched, rest) = splitAt n post

doSplitSeqWith f [] = ([], [], 0)
doSplitSeqWith f elts@(x:xs) = case f elts of
    Just n -> ([], elts, n)
    Nothing -> let (match, rest, n) = doSplitSeqWith f xs
        in (x : match, rest, n)

match val xs
    | val `isPrefixOf` xs = Just (length val)
    | otherwise = Nothing

-- | Split 'xs' on 'sep', dropping 'sep' from the result.
split :: Eq a => [a] -> [a] -> [[a]]
split sep xs = ap_tail (map (drop (length sep))) (splitSeqWith (match sep) xs)

-- | Concat a list with 'sep' in between.
join sep = concat . intersperse sep

-- | Replace sublists in 'xs'.  'repl' is given the tails of 'xs' and can
-- return (replacement, rest_of_xs) or Nothing.
replaceWith :: ([a] -> Maybe ([a], [a])) -> [a] -> [a]
replaceWith repl [] = []
replaceWith repl xs = case repl xs of
    Just (insert, rest) -> insert ++ replaceWith repl rest
    Nothing -> head xs : replaceWith repl (tail xs)

-- | Replace sublist 'val' with 'repl' in the given list.
replace val repl = replaceWith (replaceVal val repl)

-- | Helper for replaceWith to replace a constant sublist 'val' with 'repl'.
replaceVal val repl xs
    | val `isPrefixOf` xs = Just (repl, drop (length val) xs)
    | otherwise = Nothing

-- | maybe for lists
decons def f xs = if null xs then def else f (head xs) (tail xs)

ap_head f = decons [] (\x xs -> f x : xs)
ap_tail f = decons [] (\x xs -> x : f xs)
