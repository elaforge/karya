{- | Utilities to help parse event text.
-}
module Derive.Parse where
import Control.Monad
import qualified Data.Char as Char
import qualified Numeric

import qualified Derive.Score as Score
import qualified Derive.Derive3 as Derive

-- * split up args

lex = map (\event -> (event, split_args (Score.event_text event)))

-- TODO later this stuff can go in Derive.Parse?
split_args s
    | null word = []
    | otherwise = word : split_args rest
    where
    (word, rest) = break Char.isSpace (dropWhile Char.isSpace s)

-- * parse values

warn_float :: (Monad m) => String -> String -> Derive.DeriveT m Double
warn_float fail_str s = do
    (val, rest) <- case read_float s of
            [] -> Derive.throw $
                fail_str ++ " can't parse float from " ++ show s
            (parse:_) -> return parse
    when (not (null rest)) $
        Derive.warn $ fail_str ++ " has trailing junk: " ++ show rest
    return val

read_float ('-':s) = map (\(i, s) -> (-i, s)) (Numeric.readFloat (add0 s))
read_float s = Numeric.readFloat (add0 s)
add0 ('.':s) = '0' : '.' : s
add0 s = s
