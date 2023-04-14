-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Support for testing the GUI.

    There is a function 'Ui.BlockC.dump', which emits a sexpr-like set of
    key-value pairs representing its current state.  Tests can then check this
    dump for certain expected values.

    Example input: @key1 val1 key2 (subkey1 subval1)@

    Flattened output: @[("key1", "val1"), ("key2.subkey1", "subval1")]@
-}
module Ui.Dump where
import qualified Control.Applicative as Applicative
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.ParseText as ParseText

import           Global


type Dump = [(String, String)]

newtype Tree = Tree [(String, Val)] deriving (Show)
data Val = Val String | Sub Tree deriving (Show)

parse :: String -> Either Text Dump
parse = fmap flatten . ParseText.parse1 p_tree . Text.pack

flatten :: Tree -> Dump
flatten (Tree pairs) = concatMap (go []) pairs
    where
    go prefix (key, Val val) = [(flatten_key (key:prefix), val)]
    go prefix (key, Sub (Tree subs)) = concatMap (go (key:prefix)) subs
    flatten_key = Lists.join "." . reverse

p_tree :: A.Parser Tree
p_tree = Tree <$> Applicative.many p_pair

p_pair :: A.Parser (String, Val)
p_pair = (,) <$> ParseText.lexeme p_word <*> ParseText.lexeme (p_sub <|> p_val)

p_sub :: A.Parser Val
p_sub = Sub <$> ParseText.between (A.char '(') (A.char ')') p_tree

p_val :: A.Parser Val
p_val = Val <$> p_word

p_word :: A.Parser String
p_word = Text.unpack <$> (p_str <|> A.takeWhile1 (`notElem` (" ()" :: [Char])))

p_str :: A.Parser Text
p_str = ParseText.between (A.char '"') (A.char '"')
        (mconcat <$> Applicative.many str)
    where
    str = do
        chunk <- A.takeWhile (\c -> c /= '"' && c /= '\\')
        quoted <- A.option "" (A.string "\\\"" <|> A.string "\\\\")
        let res = chunk <> Text.drop 1 quoted
        if Text.null res then Applicative.empty else return res
